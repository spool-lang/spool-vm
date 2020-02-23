use std::env;
use std::process;

use spool_vm;
use spool_vm::Config;
use std::path::PathBuf;
use crate::instruction::Instruction::*;
use crate::instance::{Instance, Instance::*, Function};
use std::intrinsics::transmute;
use crate::instruction::{Chunk, Instruction};
use std::rc::Rc;
use std::collections::HashSet;
use crate::string_pool::StringPool;
use std::cell::RefCell;
use crate::vm::VM;
use crate::instance::Function::Native;

mod vm;
mod instruction;
mod instance;
mod string_pool;

mod _type;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut vm = VM::new();

    let mut chunk = Chunk::new();

    chunk.write_const(0, Byte(4));
    chunk.write_const(1, Byte(6));
    chunk.write_const(2, Byte(8));
    chunk.write_const(3, Byte(1));
    chunk.write_const(4, Byte(5));

    chunk.write_instruction(Instruction::Get(0, true));
    chunk.write_instruction(Instruction::Get(1, true));
    chunk.write_instruction(Instruction::Get(2, true));
    chunk.write_instruction(Instruction::InitArray(3));
    chunk.write_instruction(Instruction::Declare(false));
    chunk.write_instruction(Instruction::Get(0, false));
    chunk.write_instruction(Instruction::Print);
    chunk.write_instruction(Instruction::Get(4, true));
    chunk.write_instruction(Instruction::Get(3, true));
    chunk.write_instruction(Instruction::Get(0, false));
    chunk.write_instruction(Instruction::IndexSet);
    chunk.write_instruction(Instruction::Get(3, true));
    chunk.write_instruction(Instruction::Get(0, false));
    chunk.write_instruction(Instruction::IndexGet);
    chunk.write_instruction(Instruction::Print);
    chunk.write_instruction(Instruction::Get(0, false));
    chunk.write_instruction(Instruction::Print);

    vm.run(chunk)
}
