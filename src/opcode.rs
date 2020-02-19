use std::collections::HashMap;
use crate::instance::Instance;
use crate::runtime::Register;

// OpCode instructions. All instructions should be 4 bytes at the most.
#[derive(Debug)]
pub enum OpCode {
    GetTrue,
    GetFalse,
    Get(bool, u16),
    Declare(bool, u16),
    Set(u16),
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    IntNegate,
    LogicNegate,
    Less,
    Greater,
    LessOrEq,
    GreaterOrEq,
    Eq,
    NotEq,
    Is(u16),
    Concat,
    Jump(bool, u16),
    Call,
    Return(bool),
    InitArray(u16),
    IndexGet,
    IndexSet,
    EnterScope(u16),
    ExitScope,
    // Debug only.
    Print,
}

#[derive(Debug)]
pub struct Chunk {
    pub op_codes: Vec<OpCode>,
    pub is_locked: bool,
    pub jump_table: HashMap<u16, usize>,
    pub const_table:  HashMap<u16, Instance>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            op_codes: vec![],
            is_locked: false,
            jump_table: Default::default(),
            const_table: Default::default()
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