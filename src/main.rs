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
    chunk.write_name(0, vm.pool_string("spool.core.Object"));

    chunk.write_instruction(GetType(0));
    chunk.write_instruction(New(0));
    chunk.write_instruction(Print);

    vm.run(chunk)
}
