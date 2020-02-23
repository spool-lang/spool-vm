use std::collections::HashMap;
use crate::instance::Instance;
use std::rc::Rc;
use std::slice::Iter;

struct ByteFeed<'a> {
    bytes: Iter<'a, u8>
}

impl ByteFeed<'_> {
    fn new(mut bytes: Iter<u8>) -> ByteFeed {
        ByteFeed {
            bytes
        }
    }

    fn next_instruction_byte(&mut self) -> Option<&u8> {
        self.bytes.next()
    }

    fn next_byte(&mut self) -> u8 {
        match self.bytes.next() {
            None => panic!(),
            Some(byte) => *byte,
        }
    }

    fn next_u16(&mut self) -> u16 {
        let first = self.next_byte();
        let second = self.next_byte();
        u16::from_le_bytes([first, second])
    }

    fn next_bool(&mut self) -> bool {
        match self.next_byte() {
            0 => false,
            1 => true,
            _ => panic!("Expected u8 of value 1 or 0!")
        }
    }
}

// OpCode instructions. All instructions should be 4 bytes at the most.
#[derive(Debug)]
pub enum Instruction {
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
    LessOrEq,
    GreaterOrEq,
    NotEq,
    Is,
    LogicNegate,
    InitArray(u16),
    IndexGet,
    IndexSet,
    Concat,
    Jump(u16, bool),
    ExitScope(u16),
    Call,
    CallInstance(u16),
    Return(bool),
    GetType(u16),
    // Debug only.
    Print,
}

impl Instruction {
    fn from_bytes(bytes: Vec<u8>) -> Vec<Instruction> {
        let mut instructions = vec![];
        let mut feed = ByteFeed::new(bytes.iter());

        loop {
            match feed.next_instruction_byte() {
                None => break,
                Some(byte) => {
                    let instruction = match byte {
                        0 => Instruction::GetTrue,
                        1 => Instruction::GetFalse,
                        2 => {
                            let index = feed.next_u16();
                            let from_chunk = feed.next_bool();
                            Instruction::Get(index, from_chunk)
                        },
                        3 => {
                            let writable = feed.next_bool();
                            Instruction::Declare(writable)
                        },
                        4 => {
                            let index = feed.next_u16();
                            Instruction::Set(index)
                        },
                        5 => Instruction::Add,
                        6 => Instruction::Subtract,
                        7 => Instruction::Multiply,
                        8 => Instruction::Divide,
                        9 => Instruction::Power,
                        11 => Instruction::IntNegate,
                        12 => Instruction::Less,
                        13 => Instruction::Greater,
                        14 => Instruction::Eq,
                        15 => Instruction::LessOrEq,
                        16 => Instruction::GreaterOrEq,
                        17 => Instruction::NotEq,
                        18 => Instruction::Is,
                        19 => Instruction::LogicNegate,
                        20 => {
                            let size = feed.next_u16();
                            Instruction::InitArray(size)
                        },
                        21 => Instruction::IndexGet,
                        22 => Instruction::IndexSet,
                        _ => panic!("Unknown instruction!")
                    };
                    instructions.push(instruction)
                },
            }
        }

        return instructions;
    }
}

#[derive(Debug)]
pub struct Chunk {
    pub op_codes: Vec<Instruction>,
    pub is_locked: bool,
    pub jump_table: HashMap<u16, usize>,
    pub const_table:  HashMap<u16, Instance>,
    pub name_table: HashMap<u16, Rc<String>>
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            op_codes: vec![],
            is_locked: false,
            jump_table: Default::default(),
            const_table: Default::default(),
            name_table: Default::default()
        }
    }

    pub fn write_instruction(&mut self, op : Instruction) {
        if self.is_locked {
            panic!("Attempted to write to locked chunk!")
        }
        self.op_codes.push(op)
    }

    pub fn write_jump(&mut self, index: u16, point: usize) {
        if self.is_locked {
            panic!("Attempted to write to locked chunk!")
        }
        self.jump_table.insert(index,point);
    }

    pub fn write_const(&mut self, index: u16, constant: Instance) {
        if self.is_locked {
            panic!("Attempted to write to locked chunk!")
        }
        self.const_table.insert(index,constant);
    }

    pub fn write_name(&mut self, index: u16, name: Rc<String>) {
        if self.is_locked {
            panic!("Attempted to write to locked chunk!")
        }
        self.name_table.insert(index, name);
    }

    pub fn lock(&mut self) {
        self.is_locked = true;
    }

    pub fn get(&self, pt : usize) -> Option<&Instruction> {
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

    pub fn get_name(&self, index: u16) -> Rc<String> {
        match self.name_table.get(&index) {
            Some(instance) => {
                return instance.clone()
            },
            None => panic!("Name table slot `{}` was empty.", index)
        };
    }
}