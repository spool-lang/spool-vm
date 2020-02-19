use std::env;
use std::process;

use spool_vm;
use spool_vm::Config;
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
use crate::vm::NewVM;

mod runtime;
mod vm;
mod opcode;
mod instance;
mod string_pool;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut vm = NewVM::new();

    let mut chunk = Chunk::new();
    chunk.add_const(0, Int16(2));
    chunk.add_const(1, Int16(4));

    chunk.write(Get(true, 0));
    chunk.write(Get(true, 1));
    chunk.write(Greater);
    chunk.write(Print);

    chunk.write(Get(true, 0));
    chunk.write(Get(true, 1));
    chunk.write(Less);
    chunk.write(Print);

    chunk.write(Get(true, 0));
    chunk.write(Get(true, 1));
    chunk.write(Eq);
    chunk.write(Print);

    chunk.write(Get(true, 0));
    chunk.write(Get(true, 1));
    chunk.write(GreaterOrEq);
    chunk.write(Print);

    chunk.write(Get(true, 0));
    chunk.write(Get(true, 1));
    chunk.write(LessOrEq);
    chunk.write(Print);

    chunk.write(Get(true, 0));
    chunk.write(Get(true, 1));
    chunk.write(NotEq);
    chunk.write(Print);

    vm.run(chunk)
}
