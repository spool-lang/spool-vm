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

    let mut string_pool = StringPool::new();
    let bytecode = load_bytecode_zip("test.zip", &mut string_pool);
    let mut type_registry = TypeRegistry::new(&mut string_pool);
    let mut bytecode_iter = bytecode.iter();
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
        None => panic!("Main function not found in loaded bytecode."),
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

fn load_bytecode_zip(filename: &str, string_pool: &mut StringPool) -> Vec<Bytecode> {
    let result = std::fs::File::open(filename);
    let mut bytecode_vec: Vec<Bytecode> = vec![];

    match result {
        Ok(file) => {
            match ZipArchive::new(file) {
                Ok(mut archive) => {
                    for i in 0..archive.len() {
                        let mut file = archive.by_index(i).unwrap();
                        let mut slice: &[u8] = &[0u8; 65535];
                        let mut buf: Vec<u8> = Vec::from(slice);

                        match file.read(&mut buf) {
                            Ok(count) => buf.truncate(count),
                            Err(err) => println!("Error occurred when reading zip file: {}", err),
                        }

                        let vec = Bytecode::from_bytes(buf, string_pool);
                        for bytecode in vec {
                            bytecode_vec.push(bytecode)
                        }
                    }
                },
                Err(error) => {
                    println!("Error occurred when reading zip file: {}", error);
                    panic!()
                },
            }
        },
        Err(error) => {
            println!("Error occurred when loading zip file: {}", error);
            panic!()
        },
    };

    return bytecode_vec
}