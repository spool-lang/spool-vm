use std::collections::HashMap;
use crate::instance::Instance;
use std::rc::Rc;
use std::slice::Iter;
use crate::instruction::Instruction::Call;

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
    New(u16),
    InstanceGet(u16),
    InstanceSet(u16),
    InitArray(u16),
    IndexGet,
    IndexSet,
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
    Concat,
    Jump(u16, bool),
    ExitBlock(u16),
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
                            let writable = feed.next_bool();
                            Instruction::Declare(writable)
                        },
                        3 => {
                            let index = feed.next_u16();
                            Instruction::Set(index)
                        },
                        4 => {
                            let index = feed.next_u16();
                            let from_chunk = feed.next_bool();
                            Instruction::Get(index, from_chunk)
                        },
                        5 => {
                            let index = feed.next_u16();
                            Instruction::New(index)
                        },
                        6 => {
                            let index = feed.next_u16();
                            Instruction::InstanceGet(index)
                        },
                        7 => {
                            let index = feed.next_u16();
                            Instruction::InstanceSet(index)
                        }
                        8 => {
                            let size = feed.next_u16();
                            Instruction::InitArray(size)
                        },
                        9 => Instruction::IndexGet,
                        10 => Instruction::IndexSet,
                        11 => Instruction::Add,
                        12 => Instruction::Subtract,
                        13 => Instruction::Multiply,
                        14 => Instruction::Divide,
                        15 => Instruction::Power,
                        16 => Instruction::IntNegate,
                        17 => Instruction::Less,
                        18 => Instruction::Greater,
                        19 => Instruction::Eq,
                        20 => Instruction::LessOrEq,
                        21 => Instruction::GreaterOrEq,
                        22 => Instruction::NotEq,
                        23 => Instruction::Is,
                        24 => Instruction::LogicNegate,
                        25 => {
                            let index = feed.next_u16();
                            let conditional = feed.next_bool();
                            Instruction::Jump(index, conditional)
                        },
                        26 => {
                            let to_clear = feed.next_u16();
                            Instruction::ExitBlock(to_clear)
                        },
                        27 => Instruction::Call,
                        28 => {
                            let index = feed.next_u16();
                            Instruction::CallInstance(index)
                        },
                        29 => {
                            let with_value = feed.next_bool();
                            Instruction::Return(with_value)
                        },
                        30 => {
                            let name_index = feed.next_u16();
                            Instruction::GetType(name_index)
                        },
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