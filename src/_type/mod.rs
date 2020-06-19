use std::rc::Rc;
use crate::instance::{Function, Instance};
use crate::instance::Function::{NativeInstance, Native};
use std::collections::HashMap;
use crate::string_pool::StringPool;
use crate::vm::{VM, Mut};
use std::cell::{RefCell, Ref};

pub(crate) mod number;

// Stores information about properties for a type.
#[derive(Debug)]
pub struct Property {
    pub(crate) name: Rc<String>,
    pub(crate) writable: bool,
    pub(crate) _type: Rc<String>
}

impl Property {
    fn new(name: Rc<String>, writable: bool, _type: Rc<String>) -> Property {
        Property {
            name,
            writable,
            _type
        }
    }
}

#[derive(Debug)]
pub struct Type {
    pub(crate) canonical_name: Rc<String>,
    supertype: Option<TypeRef>,
    ctors: Vec<Function>,
    ctorable: bool,
    instance_functions: HashMap<Rc<String>, Function>,
    props: Vec<Rc<Property>>
}

impl Type {
    pub fn new(canonical_name: Rc<String>, supertype: Option<TypeRef>, ctors: Vec<Function>, ctorable: bool, instance_functions: HashMap<Rc<String>, Function>, props: Vec<Rc<Property>>) -> Type {
        Type {
            canonical_name,
            supertype,
            ctors,
            ctorable,
            instance_functions,
            props
        }
    }

    pub fn get_canonical_name(&self) -> Rc<String> {
        let mut actual_name = format!("{}", self.canonical_name);
        Rc::new(actual_name)
    }

    pub(crate) fn matches_type(&self, other: Mut<Type>) -> bool {
        let mut other = other;

        if &*self.canonical_name == &*other.borrow().canonical_name {
            return true
        }

        let supertype = &mut other.borrow_mut().supertype;

        if supertype.is_none() { return false }

        return supertype.unwrap().get().borrow_mut().matches_type(Rc::clone(&other))
    }

    pub(crate) fn get_ctor(&mut self, index: usize) -> Function {
        if !self.ctorable { panic!() }
        match self.ctors.get(index) {
            None => {
                let sup_op = &mut self.supertype;
                match sup_op {
                    None => panic!(),
                    Some(supertype) => supertype.get().get_mut().get_ctor(index),
                }
            },
            Some(ctor) => ctor.clone(),
        }
    }

    pub(crate) fn get_instance_func(&mut self, name: Rc<String>) -> Function {
        let mut sup_op = &mut self.supertype;
        return match self.instance_functions.get(&*name.clone()) {
            None => match sup_op {
                None => panic!(),
                Some(sup) => sup.get().get_mut().get_instance_func(name),
            },
            Some(thing) => thing.clone(),
        }
    }

    pub(crate) fn get_prop(&mut self, index: usize) -> Rc<Property> {
        if !self.ctorable { panic!() }
        match self.props.get(index) {
            None => {
                let sup_op = &mut self.supertype;
                match sup_op {
                    None => panic!(),
                    Some(supertype) => supertype.get().get_mut().get_prop(index),
                }
            },
            Some(prop) => Rc::clone(prop),
        }
    }
}

pub(crate) struct TypeBuilder {
    canonical_name: Rc<String>,
    supertype: Option<TypeRef>,
    ctors: Vec<Function>,
    ctorable: bool,
    instance_functions: HashMap<Rc<String>, Function>,
    props: Vec<Rc<Property>>
}

impl TypeBuilder {
    pub(crate) fn new(canonical_name: Rc<String>) -> TypeBuilder {
        TypeBuilder {
            canonical_name,
            supertype: None,
            ctors: vec![],
            ctorable: false,
            instance_functions: Default::default(),
            props: vec![]
        }
    }

    pub(crate) fn supertype(mut self, supertype: TypeRef) -> TypeBuilder {
        self.supertype = Some(supertype);
        return self
    }

    pub(crate) fn ctor(mut self, arity: u8, ctor: fn(&mut VM, Vec<Instance>) -> Instance) -> TypeBuilder {
        self.ctors.push(Native(arity, ctor));
        self.ctorable = true;
        self
    }

    pub(crate) fn ctorable(mut self, ctorable: bool) -> TypeBuilder {
        self.ctorable = ctorable;
        self
    }

    pub(crate) fn instance_function(mut self, name: Rc<String>, arity: u8, func: fn(&mut VM, Instance, Vec<Instance>) -> Instance) -> TypeBuilder {
        self.instance_functions.insert(name, NativeInstance(arity, func));
        self
    }

    pub(crate) fn prop(mut self, prop: Property) -> TypeBuilder {
        self.props.push(Rc::new(prop));
        self
    }

    pub(crate) fn build(self) -> Type {
        Type::new(self.canonical_name, self.supertype, self.ctors, self.ctorable, self.instance_functions, self.props)
    }
}

#[derive(Debug)]
pub(crate) struct TypeRegistry {
    types: HashMap<Rc<String>, Rc<RefCell<Type>>>
}

impl TypeRegistry {
    pub(crate) fn new(string_pool: &mut Rc<RefCell<StringPool>>) -> Rc<RefCell<TypeRegistry>> {
        let mut _self = Rc::new(
                RefCell::new(
                    TypeRegistry {
                        types: Default::default()
                    }
                )
            );
        object_type::create(string_pool, &mut _self);
        boolean_type::create(string_pool, &mut _self);
        number::create(string_pool, &mut _self);
        number::byte::create(string_pool, &mut _self);
        number::ubyte::create(string_pool, &mut _self);
        number::int16::create(string_pool, &mut _self);
        number::uint16::create(string_pool, &mut _self);
        number::int32::create(string_pool, &mut _self);
        number::uint32::create(string_pool, &mut _self);
        number::int64::create(string_pool, &mut _self);
        number::uint64::create(string_pool, &mut _self);
        number::int128::create(string_pool, &mut _self);
        number::uint128::create(string_pool, &mut _self);
        number::float32::create(string_pool, &mut _self);
        number::float64::create(string_pool, &mut _self);
        char_type::create(string_pool, &mut _self);
        string_type::create(string_pool, &mut _self);
        array_type::create(string_pool, &mut _self);
        func_type::create(string_pool, &mut _self);
        void_type::create(string_pool, &mut _self);
        console_type::create(string_pool, &mut _self);
        random_type::create(string_pool, &mut _self);
        _self
    }

    pub(crate) fn register(&mut self, _type: Type) {
        let name = Rc::clone(&_type.canonical_name);
        self.types.insert(name, Rc::from(RefCell::from(_type)));
    }

    pub(crate) fn get(&self, name: Rc<String>) -> Rc<RefCell<Type>> {
        match self.types.get(&name) {
            None => panic!("Type {} does not exist.", name),
            Some(_type) => Rc::clone(_type),
        }
    }
}

#[derive(Debug)]
pub(crate) struct TypeRef {
    registry: Mut<TypeRegistry>,
    canonical_name: Rc<String>,
    cached_type: Option<Mut<Type>>
}

impl TypeRef {
    fn new(registry: Mut<TypeRegistry>, canonical_name: Rc<String>) -> TypeRef {
        TypeRef {
            registry,
            canonical_name,
            cached_type: None
        }
    }

    fn get(&mut self) -> Mut<Type> {
        let name: Rc<String> = Rc::clone(&self.canonical_name);
        let cached = &self.cached_type;

        match cached {
            None => {
                let _type = self.registry.borrow_mut().get(name);
                self.cached_type = Some(Rc::clone(&_type));
                _type
            },
            Some(cached_type) => Rc::clone(cached_type),
        }
    }
}

pub(crate) mod object_type {
    use crate::string_pool::StringPool;
    use crate::_type::{TypeBuilder, TypeRegistry, Property};
    use std::rc::Rc;
    use crate::vm::VM;
    use crate::instance::Instance;
    use crate::instance::Instance::{Object, Bool};
    use std::collections::HashMap;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.Object"))
            .ctor(0, ctor)
            .prop(Property::new(string_pool.borrow_mut().pool_str("funny"), true, string_pool.borrow_mut().pool_str("spool.core.Boolean")))
            .build();
        type_registry.borrow_mut().register(_type)
    }

    fn ctor(vm: &mut VM, args: Vec<Instance>) -> Instance {
        let _type = vm.type_from_name("spool.core.Object");
        let mut values = HashMap::new();
        values.insert(vm.pool_string("funny"), Bool(false));

        return Object(_type, Rc::new(RefCell::new(values)));
    }
}

pub(crate) mod boolean_type {
    use crate::string_pool::StringPool;
    
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.Boolean"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.object")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod char_type {
    use std::rc::Rc;
    use std::cell::RefCell;
    use crate::string_pool::StringPool;
    use crate::_type::{TypeRegistry, TypeBuilder, TypeRef};

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.Char"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.object")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod string_type {
    use crate::vm::{VM};
    use crate::instance::{Instance, Function};
    use crate::instance::Instance::{Str, Void};
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.String"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.object")))
            .instance_function(string_pool.borrow_mut().pool_str("capitalize"), 0, capitalize)
            .build();
        type_registry.borrow_mut().register(_type)
    }

    fn capitalize(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
        if let Str(string) = instance {
            let capitalized = vm.pool_string(string.to_uppercase().as_str());
            return Str(capitalized)
        };
        panic!()
    }
}

pub(crate) mod array_type {
    use crate::string_pool::StringPool;
    
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.Array"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.object")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod func_type {
    use crate::string_pool::StringPool;
    
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.Func"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.object")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod void_type {
    use crate::string_pool::StringPool;
    
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.Void"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.object")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod console_type {
    use std::rc::Rc;
    use std::cell::RefCell;
    use crate::string_pool::StringPool;
    use crate::_type::{TypeRegistry, TypeBuilder, TypeRef};
    use crate::instance::Instance;
    use crate::vm::VM;
    use std::collections::HashMap;
    use crate::instance::Instance::{Object, Void};

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.Console"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.object")))
            .ctor(0, ctor)
            .instance_function(string_pool.borrow_mut().pool_str("println"), 1, println)
            .instance_function(string_pool.borrow_mut().pool_str("print"), 1, print)
            .build();
        type_registry.borrow_mut().register(_type)
    }

    fn ctor(vm: &mut VM, args: Vec<Instance>) -> Instance {
        let _type = vm.type_from_name("spool.core.Console");
        let mut values = HashMap::new();

        return Object(_type, Rc::new(RefCell::new(values)));
    }

    fn println(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
        println!("{}", match args.get(0) {
            None => panic!(),
            Some(instance) => instance
        });
        return Void
    }

    fn print(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
        print!("{}", match args.get(0) {
            None => panic!(),
            Some(instance) => instance
        });
        return Void
    }
}

pub(crate) mod random_type {
    use std::rc::Rc;
    use std::cell::RefCell;
    use crate::string_pool::StringPool;
    use crate::_type::{TypeRegistry, TypeBuilder, TypeRef};
    use crate::vm::VM;
    use crate::instance::Instance;
    use crate::instance::Instance::{Random, Int16};
    use rand::{thread_rng, Rng};

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.Random"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.object")))
            .ctor(0, ctor)
            .instance_function(string_pool.borrow_mut().pool_str("nextInt16"), 2, next_int16)
            .build();
        type_registry.borrow_mut().register(_type)
    }

    fn ctor(vm: &mut VM, args: Vec<Instance>) -> Instance {
        return Random(Box::new(thread_rng()))
    }

    fn next_int16(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
        if let (Some(Int16(start)), Some(Int16(end))) = (args.get(0), args.get(1)) {
            match instance {
                Random(mut rng) => {
                    return Int16(rng.gen_range(start, end))
                }
                _ => panic!()
            }
        }
        panic!()
    }
}