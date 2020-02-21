use std::rc::Rc;
use std::collections::HashMap;
use std::slice::Chunks;
use crate::opcode::{OpCode, Chunk};
use crate::instance::{Instance, Instance::*, Variable, Type};
use std::convert::TryInto;
use crate::runtime::InstructionResult::{Return, Continue, ReturnWith, ExitScope};
use std::cell::RefCell;
use crate::string_pool::StringPool;

pub struct NewCallFrame {
    register_offset: u16,
    stack_offset: usize,
    ip: usize
}

impl NewCallFrame {
    fn new(register_offset: u16, stack_offset: usize) -> NewCallFrame {
        NewCallFrame {
            register_offset,
            stack_offset,
            ip: 0
        }
    }
}

pub enum NewInstructionResult {
    Continue,
    Jump(u16)
}

pub struct VM {
    pub(crate) type_registry: TypeRegistry,
    pub string_pool: StringPool,
    pub register: Register,
    pub stack: Vec<Instance>,
    pub pc : usize,
    jumped: bool
}

impl VM {

    pub fn new() -> VM {
        let mut string_pool = StringPool::new();
        let mut type_registry = TypeRegistry::new(&mut string_pool);
        VM {
            type_registry,
            string_pool,
            register: Register::new(true),
            stack: vec![],
            pc: 0,
            jumped: false,
        }
    }

    pub fn execute_chunk(&mut self, chunk: Rc<Chunk>, frame: Rc<RefCell<CallFrame>>, args: Vec<Instance>, arg_types: Vec<Rc<Type>>) -> InstructionResult {
        let register_offset = frame.borrow().register_access_offset;

        for i in 0..args.len() {
            let _type: Rc<Type> = Rc::clone(&arg_types.get(i).unwrap());
            let instance = args.get(i).unwrap();
            self.register.declare(true, instance.to_owned(), _type);
        }

        loop {
            let op = chunk.get(self.pc);
            match op {
                Some(code) => {
                    let result = self.execute_instruction(code, Rc::clone(&chunk), Rc::clone(&frame));
                    match result {
                        Continue => {},
                        Return | ExitScope => return result,
                        ReturnWith(instance) => return ReturnWith(instance),
                    }
                },
                None => return Return
            }
            if !self.jumped {self.pc += 1}
            self.jumped = false
        }
    }

    pub fn execute_instruction(&mut self, op_code: &OpCode, chunk: Rc<Chunk>, frame: Rc<RefCell<CallFrame>>) -> InstructionResult {
        match op_code {
            OpCode::Concat => self.concat(frame.borrow().stack_offset),
            OpCode::Call => {},
            OpCode::Return(return_instance) => if *return_instance { return ReturnWith(self.get_stack_top(frame.borrow().stack_offset)) } else { return Return }
            OpCode::Print => println!("{}", self.get_stack_top(frame.borrow().stack_offset)),
            _ => panic!("This instruction is unimplemented!")
        };
        return Continue
    }

    pub fn concat(&mut self, stack_offset: usize) {
        let right = self.get_stack_top(stack_offset);
        let left = self.get_stack_top(stack_offset);

        match left {
            Str(string) => {
                let new_string = match right {
                    Str(s) => format!("{}{}", string, s),
                    Char(c) => format!("{}{}", string, c),
                    _ => format!("{}{}", string, right)
                };
                let pooled_string = self.string_pool.pool_string(new_string);
                self.stack.push(Str(pooled_string))
            }
            _ => panic!("Cannot concat operands!")
        }
    }

    pub fn get_stack_top(&mut self, stack_offset: usize) -> Instance {
        if self.stack.len() - stack_offset <= 0 {
            panic!("The stack was empty!")
        }

        return match self.stack.pop() {
            Some(instance) => instance,
            None => panic!("The stack was empty!")
        }
    }

    pub fn split_stack(&mut self, amount: usize, stack_offset: usize) -> Vec<Instance> {
        let mut vec : Vec<Instance> = vec![];
        for i in 0..amount {
            let next = self.get_stack_top(stack_offset);
            vec.push(next)
        }
        vec.reverse();
        return vec
    }
}

/*
Holds the current offset in the registry of the call frame as well as some
other useful information.
*/
pub struct CallFrame {
    register_access_offset: u16,
    register_declare_offset: u16,
    stack_offset: usize,
    scope_allocations: Vec<u16>
}

impl CallFrame {
    pub fn new() -> CallFrame {
        CallFrame {
            register_access_offset: 0,
            register_declare_offset: 0,
            stack_offset: 0,
            scope_allocations: vec![]
        }
    }

    pub fn new_with_offset(register_access_offset: u16, register_declare_offset: u16,stack_offset: usize) -> CallFrame {
        CallFrame {
            register_access_offset,
            register_declare_offset,
            stack_offset,
            scope_allocations: vec![]
        }
    }
}

#[derive(Debug)]
pub struct Register {
    internal: HashMap<u16, RefCell<Variable>>,
    size: u16,
    flag: bool
}

impl Register {
    pub fn new(flag: bool) -> Register {
        Register {
            internal: Default::default(),
            size: 0,
            flag
        }
    }

    pub fn declare(&mut self, is_const: bool, instance: Instance, _type: Rc<Type>) {
        let index = self.size;
        self.size += 1;

        let variable = Variable::new(is_const, instance, _type);
        self.internal.insert(index, RefCell::from(variable));
    }

    pub fn set(&mut self, index: u16, instance: Instance) {
        match self.internal.get(&index) {
            None => panic!(),
            Some(var) => {
                var.borrow_mut().set(instance)
            },
        }
    }

    pub fn get(&self, index: u16) -> Instance {
        match self.internal.get(&index) {
            None => panic!(),
            Some(var) => var.borrow().stored.to_owned(),
        }
    }

    pub fn truncate(&mut self, to_size: u16) {
        if to_size == self.size || self.size == 0 {
            return;
        }

        let mut to_clear = self.size - 1;
        loop {
            self.internal.remove(&to_clear);
            self.size -= 1;
            if self.size == 0 {
                return;
            }
            to_clear = to_clear - 1;
            if to_size == self.size {
                return;
            }
        }
    }
}

#[derive(Debug)]
pub struct TypeRegistry {
    type_map: HashMap<u16, Rc<Type>>,
    name_map: HashMap<Rc<String>, u16>,
    size: u16
}

impl TypeRegistry {
    fn new(string_pool: &mut StringPool) -> TypeRegistry {
        let mut _self = TypeRegistry {
            type_map: Default::default(),
            name_map: Default::default(),
            size: 0
        };
        _self
    }

    fn register(&mut self, _type: Type) {
        let index = self.size;
        let name = Rc::clone(&_type.canonical_name);

        self.size += 1;

        self.name_map.insert(name, index);
        self.type_map.insert(index, Rc::from(_type));
    }

    fn get(&self, index: u16) -> Rc<Type> {
        match self.type_map.get(&index) {
            None => panic!("Non-existant type!"),
            Some(t) => Rc::clone(t),
        }
    }

    fn get_by_name(&self, name: Rc<String>) -> Rc<Type> {
        match self.name_map.get(&name) {
            None => panic!("Type {} does not exist.", name),
            Some(i) => self.get(*i),
        }
    }
}

pub enum InstructionResult{
    Continue,
    Return,
    ReturnWith(Instance),
    ExitScope,
}
