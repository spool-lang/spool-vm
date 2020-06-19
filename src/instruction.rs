use std::collections::HashMap;
use crate::instance::Instance;
use std::rc::Rc;
use std::slice::Iter;
use crate::instruction::Instruction::Call;
use std::string::FromUtf8Error;
use crate::instance::Instance::{Str, Int16, Bool};
use std::num::ParseIntError;
use crate::_type::Type;
use std::str::Utf8Error;

struct ByteFeed {
    vec: Vec<u8>,
    index: usize
}

impl ByteFeed {
    fn new(mut bytes: Vec<u8>) -> ByteFeed {
        ByteFeed {
            vec: bytes,
            index: 0
        }
    }

    fn next_byte(&mut self) -> Option<u8> {
        let index = self.index;
        self.index += 1;
        match self.vec.get(index) {
            None => None,
            Some(byte) => Some(*byte),
        }
    }

    fn next_bytes(&mut self, count: usize) -> Vec<u8> {
        let mut next_bytes: Vec<u8> = vec![];

        for _x in 0..count {
            match self.next_byte() {
                None => break,
                Some(byte) => next_bytes.push(byte),
            }
        }

        return next_bytes
    }

    fn peek_byte(&self) -> Option<u8> {
        let index = self.index;
        match self.vec.get(index) {
            None => None,
            Some(byte) => Some(*byte),
        }
    }

    fn peek_byte_at(&self, index: usize) -> Option<u8> {
        match self.vec.get(index) {
            None => None,
            Some(byte) => Some(*byte),
        }
    }

    fn peek_bytes(&self, count: usize) -> Vec<u8> {
        let mut peeked = vec![];
        let mut index = self.index;

        for _x in 0..count {
            match self.peek_byte_at(index) {
                None => break,
                Some(byte) => peeked.push(byte),
            }
            index += 1
        }

        return peeked
    }

    fn next_u16(&mut self) -> Option<u16> {
        let next_bytes = self.next_bytes(2);

        let first = next_bytes.get(0);
        let second = next_bytes.get(1);

        return match (first, second) {
            (Some(f_byte), Some(r_byte)) => {
                Some(u16::from_le_bytes([*f_byte, *r_byte]))
            }
            (_, _) => None
        }
    }

    fn next_bool(&mut self) -> Option<bool> {
        match self.next_byte() {
            Some(int) => match int {
                0 => Some(false),
                1 => Some(true),
                _ => None
            }
            None => None
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let option = self.next_byte();
        match option {
            None => None,
            Some(byte) => {
                match String::from_utf8([byte].to_vec()) {
                    Ok(string) => {
                        return string.chars().next()
                    },
                    Err(error) => {
                        println!("{}", error);
                        panic!()
                    },
                }
            }
        }
    }

    fn peek_char(&self) -> Option<char> {
        match self.peek_byte() {
            None => None,
            Some(byte) => {
                match String::from_utf8(vec![byte]) {
                    Ok(string) => {
                        return string.chars().next()
                    },
                    Err(error) => {
                        println!("{}", error);
                        panic!()
                    },
                }
            },
        }
    }

    fn has_string(&mut self, string: &str) -> bool {
        let len = string.len();
        let bytes = self.next_bytes(len);
        let result = std::str::from_utf8(&bytes[..]);

        return match result {
            Ok(to_compare) => string == to_compare,
            Err(err) => false,
        }
    }

    fn split(&mut self, amount: u16) -> Vec<u8> {
        let mut bytes = vec![];
        for _ in 0..amount {
            match self.next_byte() {
                None => {},
                Some(byte) => bytes.push(byte),
            }
        }
        return bytes
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
    And,
    Or,
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
    fn from_bytes(bytes: Vec<u8>, buffer: &mut Vec<Instruction>) {
        let mut feed = ByteFeed::new(bytes);

        loop {
            match feed.next_byte() {
                None => break,
                Some(byte) => {
                    let instruction = match byte {
                        0 => Instruction::GetTrue,
                        1 => Instruction::GetFalse,
                        2 => {
                            let writable = feed.next_bool().unwrap();
                            Instruction::Declare(writable)
                        },
                        3 => {
                            let index = feed.next_u16().unwrap();
                            Instruction::Set(index)
                        },
                        4 => {
                            let index = feed.next_u16().unwrap();
                            let from_chunk = feed.next_bool().unwrap();
                            Instruction::Get(index, from_chunk)
                        },
                        5 => {
                            let index = feed.next_u16().unwrap();
                            Instruction::New(index)
                        },
                        6 => {
                            let index = feed.next_u16().unwrap();
                            Instruction::InstanceGet(index)
                        },
                        7 => {
                            let index = feed.next_u16().unwrap();
                            Instruction::InstanceSet(index)
                        }
                        8 => {
                            let size = feed.next_u16().unwrap();
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
                        23 => Instruction::And,
                        24 => Instruction::Or,
                        25 => Instruction::Is,
                        26 => Instruction::LogicNegate,
                        27 => {
                            let index = feed.next_u16().unwrap();
                            let conditional = feed.next_bool().unwrap();
                            Instruction::Jump(index, conditional)
                        },
                        28 => {
                            let to_clear = feed.next_u16().unwrap();
                            Instruction::ExitBlock(to_clear)
                        },
                        29 => Instruction::Call,
                        30 => {
                            let index = feed.next_u16().unwrap();
                            Instruction::CallInstance(index)
                        },
                        31 => {
                            let with_value = feed.next_bool().unwrap();
                            Instruction::Return(with_value)
                        },
                        32 => {
                            let name_index = feed.next_u16().unwrap();
                            Instruction::GetType(name_index)
                        },
                        _ => panic!("Unknown instruction!")
                    };
                    buffer.push(instruction)
                },
            }
        }

        return;
    }
}

#[derive(Debug)]
pub struct Chunk {
    pub instructions: Vec<Instruction>,
    pub is_locked: bool,
    pub jump_table: HashMap<u16, usize>,
    pub const_table:  HashMap<u16, Instance>,
    pub name_table: HashMap<u16, Rc<String>>
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            instructions: vec![],
            is_locked: false,
            jump_table: Default::default(),
            const_table: Default::default(),
            name_table: Default::default()
        }
    }

    pub fn from_bytes(bytes: Vec<u8>) -> Chunk {
        let mut feed = ByteFeed::new(bytes);

        let mut chunk = Chunk::new();

        loop {
            match feed.next_char() {
                None => {
                    return chunk
                },
                Some(c) => {
                    if c == '#' {
                        let mut current_string = "".to_string();
                        loop {
                            let c2 = feed.next_char().unwrap();
                            if c2 == '(' { break }
                            current_string.push(c2)
                        }

                        current_string.clear();
                        let mut index = 0;

                        loop {
                            let c2 = feed.next_char().unwrap();

                            if c2 == ';' {
                                chunk.write_name(index, Rc::from(current_string.clone()));
                                current_string.clear();
                                break
                            }
                            if c2 == ',' {
                                chunk.write_name(index, Rc::from(current_string.clone()));
                                current_string.clear();
                                index += 1
                            }
                            else {
                                current_string.push(c2)
                            }
                        }

                        current_string.clear();
                        index = 0;

                        let mut string_mode = false;
                        let mut found_string = false;
                        loop {
                            let c2 = feed.next_char().unwrap();

                            if c2 == '"' {
                                string_mode = !string_mode;
                                found_string = true
                            }
                            else if (c2 == ',' || c2 == ';') && !string_mode {
                                if found_string {
                                    chunk.write_const(index, Str(Rc::new(current_string.clone())));
                                    current_string.clear();
                                    found_string = false
                                }
                                else {
                                    let int = str::parse::<i16>(current_string.as_str());
                                    let boolean = str::parse::<bool>(current_string.as_str());
                                    if int.is_ok() {
                                        chunk.write_const(index, Int16(int.unwrap()))
                                    }
                                    else if boolean.is_ok() {
                                        chunk.write_const(index, Bool(boolean.unwrap()))
                                    }
                                    else {
                                        panic!()
                                    }
                                }

                                if c2 == ';' { break }
                                current_string.clear();
                                index += 1
                            }
                            else { current_string.push(c2); }
                        }

                        current_string.clear();
                        loop {
                            let c2 = feed.next_char().unwrap();
                            if c2 == ')' { break }
                            current_string.push(c2)
                        }
                        match str::parse::<u16>(current_string.as_str()) {
                            Ok(count) => {
                                let instruction_bytes = feed.split(count);
                                Instruction::from_bytes(instruction_bytes, chunk.instructions.as_mut());
                                return chunk
                            },
                            Err(_) => panic!(),
                        }
                    }
                    else { panic!() }
                },
            }
        }
    }

    pub fn write_instruction(&mut self, op : Instruction) {
        if self.is_locked {
            panic!("Attempted to write to locked chunk!")
        }
        self.instructions.push(op)
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
        return self.instructions.get(pt)
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

pub enum Bytecode {
    LoadedChunk(Chunk),
    LoadedType(Type)
}

impl Bytecode {
    fn from_bytes(bytes: Vec<u8>) -> () {
        let mut feed = ByteFeed::new(bytes);

        if feed.has_string("#main") { }
    }
}