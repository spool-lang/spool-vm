use std::{env, fs};
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
use std::io::Error;

mod vm;
mod instruction;
mod instance;
mod string_pool;

mod _type;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut vm = VM::new();
    let bytecode = load_bytecode("test.sbc");
    let chunk = Chunk::from_bytes(bytecode);

    vm.run(chunk)
}

fn load_bytecode(filename: &str) -> Vec<u8> {
    let contents = fs::read(filename);

    match contents {
        Ok(bytes) => bytes,
        Err(_) => panic!(),
    }
}