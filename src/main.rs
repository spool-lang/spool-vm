use std::{env, fs};
use std::process;

use spool_vm;
use spool_vm::Config;
use std::path::PathBuf;
use crate::instruction::Instruction::*;
use crate::instance::{Instance, Instance::*, Function};
use std::intrinsics::transmute;
use crate::instruction::{Chunk, Instruction, Bytecode};
use std::rc::Rc;
use std::collections::HashSet;
use crate::string_pool::StringPool;
use std::cell::RefCell;
use crate::vm::VM;
use crate::instance::Function::Native;
use std::io::Error;
use crate::_type::TypeRegistry;

mod vm;
mod instruction;
mod instance;
mod string_pool;

mod _type;

fn main() {
    let args: Vec<String> = env::args().collect();

    let bytes = load_bytecode("test.sbc");
    // print_bytes(&bytes, 30, 52);
    let mut string_pool = StringPool::new();
    let mut type_registry = TypeRegistry::new(&mut string_pool);
    let mut bytecode_vec = Bytecode::from_bytes(bytes, &mut string_pool);
    let mut bytecode_iter = bytecode_vec.iter();
    let mut main: Option<Rc<Chunk>> = None;

    loop {
        match bytecode_iter.next() {
            None => break,
            Some(bytecode) => match bytecode {
                Bytecode::LoadedMain(main_chunk) => {
                    main = Some(Rc::clone(main_chunk));
                },
                Bytecode::LoadedType(_type) => type_registry.register_ref(_type),
            },
        }
    }

    let mut vm = VM::new(string_pool, type_registry);
    match main {
        None => panic!(),
        Some(chunk) => vm.run(chunk),
    }
}

fn load_bytecode(filename: &str) -> Vec<u8> {
    let contents = fs::read(filename);

    match contents {
        Ok(bytes) => bytes,
        Err(_) => panic!(),
    }
}

fn print_bytes(bytes: &Vec<u8>, from: usize, to: usize) {
    let mut iter = bytes.iter();
    let mut index = 0;
    println!("Bytes: [");
    iter.for_each(|byte| {
        if index >= from && index <= to {
            println!("{}: {}", index, byte);
        }
        index += 1
    });
    println!("]")
}