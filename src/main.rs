use std::{env, fs};
use std::process;

use spool_vm;
use spool_vm::Config;
use std::path::PathBuf;
use crate::instruction::Instruction::*;
use crate::instance::{Instance, Instance::*, Function};
use crate::instruction::{Chunk, Instruction, Bytecode};
use std::rc::Rc;
use std::collections::HashSet;
use crate::string_pool::StringPool;
use std::cell::RefCell;
use crate::vm::VM;
use crate::instance::Function::Native;
use std::io::{Error, Read};
use crate::_type::TypeRegistry;
use std::fs::File;
use zip::ZipArchive;
use zip::result::ZipError;

mod vm;
mod instruction;
mod instance;
mod string_pool;

mod _type;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut vm = VM::new();
    vm.load("test.zip");
    vm.run();
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