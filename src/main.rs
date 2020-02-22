use std::env;
use std::process;

use spool_vm;
use spool_vm::Config;
use std::path::PathBuf;
use crate::runtime::{VM, CallFrame};
use crate::opcode::OpCode::*;
use crate::instance::{Instance, Instance::*, Type, Function};
use std::intrinsics::transmute;
use crate::opcode::{Chunk, OpCode};
use std::rc::Rc;
use std::collections::HashSet;
use crate::string_pool::StringPool;
use std::cell::RefCell;
use crate::vm::NewVM;
use crate::instance::Function::Native;

mod runtime;
mod vm;
mod opcode;
mod instance;
mod string_pool;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut vm = NewVM::new();

    let mut func_chunk = Chunk::new();
    func_chunk.write_const(0, Int16(8));
    func_chunk.write_const(1, Int16(32));
    func_chunk.write_instruction(OpCode::Get(0, true));
    func_chunk.write_instruction(Print);
    func_chunk.write_instruction(OpCode::Get(0, false));
    func_chunk.write_instruction(Print);
    func_chunk.write_instruction(OpCode::Get(1, true));
    func_chunk.write_instruction(Return(true));

    let i16_type = vm.type_from_name("silicon.lang.Int16");
    let func = Function::Standard(vec![i16_type.clone()], Rc::from(func_chunk));

    let native_func = Native(1, print);

    let mut chunk = Chunk::new();
    chunk.write_const(0, Int16(16));
    chunk.write_const(1, Func(func));
    chunk.write_const(2, Func(native_func));

    chunk.write_instruction(Get(0, true));
    chunk.write_instruction(Get(1, true));
    chunk.write_instruction(Call);
    chunk.write_instruction(Get(2, true));
    chunk.write_instruction(Call);

    vm.run(chunk)
}

fn print(new_vm: &mut NewVM, args: Vec<Instance>) -> Instance {
    println!("{}", args.get(0).unwrap());
    return Void;
}
