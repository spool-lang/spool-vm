use std::collections::HashMap;
use crate::instance::Instance;
use crate::runtime::Register;
use std::rc::Rc;

// OpCode instructions. All instructions should be 4 bytes at the most.
#[derive(Debug)]
pub enum OpCode {
    GetTrue,
    GetFalse,
    Get(u16, bool),
    Declare(bool),
    Set(u16),
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    IntNegate,
    Less,
    Greater,
    Eq,
    GreaterOrEq,
    LessOrEq,
    NotEq,
    Is,
    LogicNegate,
    Concat,
    Call,
    InitArray(u16),
    IndexGet,
    IndexSet,
    Jump(u16, bool),
    ExitScope(u16),
    Return(bool),
    GetType(u16),
    // Debug only.
    Print,
}

#[derive(Debug)]
pub struct Chunk {
    pub op_codes: Vec<OpCode>,
    pub is_locked: bool,
    pub jump_table: HashMap<u16, usize>,
    pub const_table:  HashMap<u16, Instance>,
    pub type_table: HashMap<u16, Rc<String>>
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            op_codes: vec![],
            is_locked: false,
            jump_table: Default::default(),
            const_table: Default::default(),
            type_table: Default::default()
        }
    }

    pub fn write(&mut self, op : OpCode) {
        if self.is_locked {
            panic!("Attempted to write to locked chunk!")
        }
        self.op_codes.push(op)
    }

    pub fn add_const(&mut self, index: u16, constant: Instance) {
        if self.is_locked {
            panic!("Attempted to write to locked chunk!")
        }
        self.const_table.insert(index,constant);
    }

    pub fn add_type(&mut self, index: u16, name: Rc<String>) {
        if self.is_locked {
            panic!("Attempted to write to locked chunk!")
        }
        self.type_table.insert(index,name);
    }

    pub fn lock(&mut self) {
        self.is_locked = true;
    }

    pub fn get(&self, pt : usize) -> Option<&OpCode> {
        return self.op_codes.get(pt)
    }

    pub fn get_const(&self, index: u16) -> Instance {
        match self.const_table.get(&index) {
            Some(instance) => {
                return instance.to_owned()
            },
            None => panic!("Constant table slot `{}` was empty.", index)
        };
    }
}