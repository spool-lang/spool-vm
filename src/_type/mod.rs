use std::rc::Rc;
use crate::instance::{Function, Instance, InstanceData};
use crate::instance::Function::{NativeInstance, Native, NativeConstructor, Constructor};
use std::collections::HashMap;
use crate::string_pool::StringPool;
use crate::vm::{VM, Mut};
use std::cell::{RefCell, Ref};
use crate::_type::object::*;

pub(crate) mod number;
pub(crate) mod object;

// Stores information about properties for a type.
#[derive(Debug)]
pub struct Property {
    pub(crate) name: Rc<String>,
    pub(crate) writable: bool,
    pub(crate) type_ref: TypeRef
}

impl Property {
    pub(crate) fn new(name: Rc<String>, writable: bool, type_name: Rc<String>) -> Property {
        Property {
            name,
            writable,
            type_ref: TypeRef::new(Rc::clone(&type_name))
        }
    }

    fn resolve_type(&mut self, registry: &TypeRegistry) {
        if !self.type_ref.is_resolved() {
            self.type_ref.resolve_type(registry)
        }
    }
}

#[derive(Debug)]
pub struct Type {
    pub(crate) canonical_name: Rc<String>,
    is_trait: bool,
    supertype: Option<TypeRef>,
    traits: Vec<TypeRef>,
    constructor: Vec<Function>,
    ctorable: bool,
    instance_functions: HashMap<Rc<String>, Function>,
    prop_map: HashMap<Rc<String>, Mut<Property>>,
}

impl Type {
    pub(crate) fn new(canonical_name: Rc<String>, is_trait: bool, supertype: Option<TypeRef>, traits: Vec<TypeRef>, ctors: Vec<Function>, ctorable: bool, instance_functions: HashMap<Rc<String>, Function>, prop_map: HashMap<Rc<String>, Mut<Property>>) -> Type {
        Type {
            canonical_name,
            is_trait,
            supertype,
            traits,
            constructor: ctors,
            ctorable,
            instance_functions,
            prop_map
        }
    }

    pub(crate) fn resolve_type_information(&mut self, registry: &mut TypeRegistry) {
        match &mut self.supertype {
            None => {},
            Some(supertype_ref) => {
                if !supertype_ref.is_resolved() {
                    supertype_ref.resolve_type(registry)
                }
            },
        }

        let mut trait_iter = self.traits.iter_mut();

        trait_iter.for_each(|trait_ref| {
           if !trait_ref.is_resolved() {
               trait_ref.resolve_type(registry)
           }
        });

        let mut constructor_iter = self.constructor.iter_mut();

        constructor_iter.for_each(|constructor| {
            if let Constructor(params, _) = constructor {
                for param in params {
                    param.resolve_type(registry)
                }
            }
        });

        let mut function_iter = self.instance_functions.iter_mut();

        loop {
            match &mut function_iter.next() {
                None => break,
                Some((name, instance_function)) => match instance_function {
                    Function::Standard(params, _) => {
                        for mut param in params {
                            param.resolve_type(registry)
                        }
                    },
                    Function::Instance(self_type, params, _) => {
                        self_type.resolve_type(registry);

                        for mut param in params {
                            param.resolve_type(registry)
                        }
                    },
                    _ => {}
                },
            }
        }

        let mut prop_iter = self.prop_map.iter_mut();

        loop {
            match &mut prop_iter.next() {
                None => break,
                Some((name, prop)) => {
                    prop.borrow_mut().resolve_type(registry)
                },
            }
        }
    }

    pub fn get_canonical_name(&self) -> Rc<String> {
        let mut actual_name = format!("{}", self.canonical_name);
        Rc::new(actual_name)
    }

    pub(crate) fn is_or_subtype_of(&self, other: Mut<Type>) -> bool {
        let mut other = other;

        if &*self.canonical_name == &*other.borrow().canonical_name {
            return true
        }

        match &other.borrow().supertype {
            None => return false,
            Some(supertype_ref) => {
                if self.is_or_subtype_of(supertype_ref.get()) { return true }

                for _trait in &self.traits {
                    if self.is_or_subtype_of(_trait.get()) { return true }
                }
            }
        }

        return false
    }

    pub(crate) fn get_ctor(&self, index: usize) -> Function {
        if self.is_trait { panic!("Type '{}' is a trait and cannot be constructed.") }
        if !self.ctorable { panic!("Type '{}' does not have a constructor.", self.canonical_name) }
        match self.constructor.get(index) {
            None => {
                let mut sup_op = self.supertype.clone();
                match &mut sup_op {
                    None => panic!(),
                    Some(supertype_ref) => supertype_ref.get().borrow().get_ctor(index)
                }
            },
            Some(ctor) => ctor.clone(),
        }
    }

    pub(crate) fn get_instance_func(&self, name: Rc<String>) -> Function {
        let sup_op = self.supertype.clone();
        return match self.instance_functions.get(&*name.clone()) {
            None => match sup_op {
                None => panic!("Failed to get instance function '{}' on type '{}", name, self.canonical_name),
                Some(supertype_ref) => supertype_ref.get().borrow().get_instance_func(name),
            },
            Some(thing) => thing.clone(),
        }
    }

    pub(crate) fn get_property(&self, name: Rc<String>) -> Mut<Property> {
        match self.prop_map.get(&name) {
            None => {
                let sup_op = self.supertype.clone();
                match sup_op {
                    None => panic!(),
                    Some(supertype) => supertype.get().borrow().get_property(Rc::clone(&name)),
                }
            },
            Some(prop) => Rc::clone(&prop),
        }
    }

    pub(crate) fn create_instance_data(&self) -> InstanceData {
        return InstanceData::new(&self.prop_map)
    }
}

#[derive(Debug)]
pub(crate) struct TypeRef {
    name: Rc<String>,
    _type: Option<Mut<Type>>
}

impl TypeRef {
    pub(crate) fn new(name: Rc<String>) -> TypeRef {
        TypeRef {
            name,
            _type: None
        }
    }

    pub(crate) fn get(&self) -> Mut<Type> {
        match &self._type {
            None => panic!(),
            Some(_type) => Rc::clone(_type),
        }
    }

    fn resolve_type(&mut self, registry: &TypeRegistry) {
        let name = Rc::clone(&self.name);
        self._type = Some(registry.get(name));
    }

    fn is_resolved(&self) -> bool {
        return self._type.is_some()
    }
}

impl Clone for TypeRef {
    fn clone(&self) -> Self {
        let name = Rc::clone(&self.name);
        let _type = match &self._type {
            None => None,
            Some(_type) => Some(Rc::clone(_type)),
        };

        TypeRef {
            name,
            _type
        }
    }
}

pub(crate) struct TypeBuilder {
    canonical_name: Rc<String>,
    is_trait: bool,
    supertype: Option<TypeRef>,
    traits: Vec<TypeRef>,
    ctors: Vec<Function>,
    ctorable: bool,
    instance_functions: HashMap<Rc<String>, Function>,
    prop_map: HashMap<Rc<String>, Rc<RefCell<Property>>>,
    props: Vec<Rc<Property>>
}

impl TypeBuilder {
    pub(crate) fn new(canonical_name: Rc<String>) -> TypeBuilder {
        TypeBuilder {
            canonical_name,
            is_trait: false,
            supertype: None,
            traits: vec![],
            ctors: vec![],
            ctorable: false,
            instance_functions: Default::default(),
            prop_map: Default::default(),
            props: vec![]
        }
    }

    pub(crate) fn supertype(mut self, supertype: TypeRef) -> TypeBuilder {
        self.supertype = Some(supertype);
        return self
    }

    pub(crate) fn add_trait(mut self, _trait: TypeRef) -> TypeBuilder {
        self.traits.push(_trait);
        return self
    }

    pub(crate) fn constructor(mut self, constructor: Function) -> TypeBuilder {
        self.ctors.push(constructor);
        self.ctorable = true;
        return self
    }

    pub(crate) fn native_constructor(mut self, arity: u8, ctor: fn(&mut VM, &Instance, Vec<Instance>)) -> TypeBuilder {
        self.ctors.push(NativeConstructor(arity, ctor));
        self.ctorable = true;
        self
    }

    pub(crate) fn ctorable(mut self, ctorable: bool) -> TypeBuilder {
        self.ctorable = ctorable;
        self
    }

    pub(crate) fn native_instance_function(mut self, name: Rc<String>, arity: u8, func: fn(&mut VM, Instance, Vec<Instance>) -> Instance) -> TypeBuilder {
        self.instance_functions.insert(name, NativeInstance(arity, func));
        self
    }

    pub(crate) fn instance_function(mut self, name: Rc<String>, function: Function) -> TypeBuilder {
        self.instance_functions.insert(name, function);
        self
    }

    pub(crate) fn prop(mut self, prop: Property) -> TypeBuilder {
        self.prop_map.insert(Rc::clone(&prop.name), Rc::new(RefCell::new(prop)));
        self
    }

    pub(crate) fn build(self) -> Type {
        Type::new(self.canonical_name, self.is_trait, self.supertype, self.traits, self.ctors, self.ctorable, self.instance_functions, self.prop_map)
    }
}

pub(crate) struct TypeRegistry {
    types: HashMap<Rc<String>, Mut<Type>>
}

impl TypeRegistry {
    pub(crate) fn new(string_pool: &mut StringPool) -> TypeRegistry {
        let mut _self = TypeRegistry {
            types: Default::default()
        };
        object::create(string_pool, &mut _self);
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
        raw_rng_type::create(string_pool, &mut _self);
        random_type::create(string_pool, &mut _self);
        _self
    }

    pub(crate) fn register(&mut self, _type: Type) {
        let name = Rc::clone(&_type.canonical_name);
        self.types.insert(name, Rc::from(RefCell::from(_type)));
    }

    pub(crate) fn register_ref(&mut self, _type: &Mut<Type>) {
        let name = Rc::clone(&_type.borrow().canonical_name);
        self.types.insert(name, Rc::clone(&_type));
    }

    pub(crate) fn resolve_supertypes(&mut self) {
        let mut iterator = self.types.iter();
        let mut to_resolve: Vec<Mut<Type>> = vec![];

        loop {
            match &mut iterator.next() {
                None => break,
                Some((key, value)) => {
                    to_resolve.push(Rc::clone(value))
                }
            }
        }

        for _type in to_resolve.iter() {
            _type.borrow_mut().resolve_type_information(self)
        }
    }

    pub(crate) fn get(&self, name: Rc<String>) -> Mut<Type> {
        match self.types.get(&name) {
            None => panic!("Type {} does not exist.", name),
            Some(_type) => Rc::clone(_type),
        }
    }
}