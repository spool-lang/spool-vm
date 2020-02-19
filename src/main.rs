use std::env;
use std::process;

use silicon;
use silicon::Config;
use std::path::PathBuf;
use crate::runtime::{VM, CallFrame};
use crate::opcode::OpCode::*;
use crate::instance::{Instance, Instance::*, Type};
use std::intrinsics::transmute;
use crate::opcode::Chunk;
use std::rc::Rc;
use std::collections::HashSet;
use crate::string_pool::StringPool;
use std::cell::RefCell;

mod runtime;
mod vm;
mod opcode;
mod instance;
mod string_pool;
mod math;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut vm = VM::new();

    let mut chunk = Chunk::new();
    chunk.add_const(0, UByte(255));

    chunk.write(Get(true, 0));
    chunk.write(Print);

    vm.execute_chunk(Rc::new(chunk), Rc::new(RefCell::new(CallFrame::new())), vec![], vec![]);

    /*
    if args.len() >= 2 {
        //engine::run(PathBuf::from(&args[1]))
    }
    else {
        println!("Please specify the path to the main script!");
        process::exit(1);
    }

    println!("Program execution finished successfully.");
    */
}
