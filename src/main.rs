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
    chunk.add_const(0, Bool(true));
    chunk.jump_table.insert(0, 3);
    chunk.add_type(0, Rc::from("silicon.lang.Boolean".to_string()));
    chunk.add_type(1, Rc::from("silicon.lang.Int16".to_string()));
    chunk.add_type(2, Rc::from("silicon.lang.Object".to_string()));

    chunk.write(Get(0, true));
    chunk.write(GetType(0));
    chunk.write(Is);
    chunk.write(Print);
    chunk.write(Get(0, true));
    chunk.write(GetType(1));
    chunk.write(Is);
    chunk.write(Print);
    chunk.write(Get(0, true));
    chunk.write(GetType(2));
    chunk.write(Is);
    chunk.write(Print);

    vm.run(chunk)
}
