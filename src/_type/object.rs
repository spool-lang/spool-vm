use crate::string_pool::StringPool;
use crate::_type::{TypeBuilder, TypeRegistry, Property};
use std::rc::Rc;
use crate::vm::VM;
use crate::instance::Instance;
use crate::instance::Instance::{Object, Bool};
use std::collections::HashMap;
use std::cell::RefCell;

pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
    let _type = TypeBuilder::new(string_pool.pool("spool.core.Object"))
        .native_constructor(0, constructor)
        .prop(Property::new(string_pool.pool("funny"), true, string_pool.pool("spool.core.Boolean")))
        .build();
    type_registry.register(_type)
}

pub(crate) fn constructor(vm: &mut VM, uninitialized: &Instance, args: Vec<Instance>) {
    if let Object(_, data) = uninitialized {
        return;
    }
    panic!()
}

pub(crate) mod boolean_type {
    use crate::string_pool::StringPool;

    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool("spool.core.Boolean"))
            .supertype(TypeRef::new(string_pool.pool("spool.core.Object")))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod char_type {
    use crate::string_pool::StringPool;

    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool("spool.core.Char"))
            .supertype(TypeRef::new(string_pool.pool("spool.core.Object")))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod string_type {
    use crate::vm::{VM};
    use crate::instance::{Instance, Function};
    use crate::instance::Instance::{Str, Void};
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool("spool.core.String"))
            .supertype(TypeRef::new(string_pool.pool("spool.core.Object")))
            .native_instance_function(string_pool.pool("capitalize"), 0, capitalize)
            .build();
        type_registry.register(_type)
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

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool("spool.core.Array"))
            .supertype(TypeRef::new(string_pool.pool("spool.core.Object")))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod func_type {
    use crate::string_pool::StringPool;

    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool("spool.core.Func"))
            .supertype(TypeRef::new(string_pool.pool("spool.core.Object")))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod void_type {
    use crate::string_pool::StringPool;

    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool("spool.core.Void"))
            .supertype(TypeRef::new(string_pool.pool("spool.core.Object")))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod console_type {
    use crate::string_pool::StringPool;
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use std::rc::Rc;
    use crate::vm::VM;
    use crate::instance::Instance;
    use crate::instance::Instance::{Void, Bool, Object};
    use std::collections::HashMap;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool("spool.core.Console"))
            .supertype(TypeRef::new(string_pool.pool("spool.core.Object")))
            .native_constructor(0, constructor)
            .native_instance_function(string_pool.pool("println"), 1, println)
            .native_instance_function(string_pool.pool("print"), 1, print)
            .build();
        type_registry.register(_type)
    }

    fn constructor(vm: &mut VM, uninitialized: &Instance, args: Vec<Instance>) {
        if let Object(_, data) = uninitialized {
            super::constructor(vm, uninitialized, args);
            return;
        }
        panic!()
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

pub(crate) mod raw_rng_type {
    use crate::string_pool::StringPool;
    use crate::_type::{TypeRegistry, TypeBuilder, TypeRef};

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool("spool.core.RawRng"))
            .supertype(TypeRef::new(string_pool.pool("spool.core.Object")))
            .build();
        type_registry.register(_type);
    }
}

pub(crate) mod random_type {
    use crate::string_pool::StringPool;
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef, Property};
    use std::rc::Rc;
    use crate::vm::VM;
    use crate::instance::Instance;
    use crate::instance::Instance::{Void, Bool, Object, Int16, RawRng};
    use std::collections::HashMap;
    use std::cell::RefCell;
    use rand::{thread_rng, Rng};
    use std::borrow::BorrowMut;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool("spool.core.Random"))
            .supertype(TypeRef::new(string_pool.pool("spool.core.Object")))
            .prop(Property::new(string_pool.pool("rng"), false, string_pool.pool("spool.core.RawRng")))
            .native_constructor(0, constructor)
            .native_instance_function(string_pool.pool("nextInt16"), 2, next_int16)
            .build();
        type_registry.register(_type)
    }

    //TODO: Completely internalize RawRNG.
    fn constructor(vm: &mut VM, uninitialized: &Instance, args: Vec<Instance>) {
        if let Object(_, data) = uninitialized {
            super::constructor(vm, uninitialized, args);
            let foo = vm.pool_string("spool.core.RawRng");
            data.set(vm.pool_string("rng"), RawRng(Box::new(thread_rng())), vm.type_from_name(foo.as_str()));
            return;
        }
        panic!()
    }

    fn next_int16(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
        if let (Some(Int16(start)), Some(Int16(end))) = (args.get(0), args.get(1)) {
            if let Object(_, data) = instance {
                if let RawRng(mut rng) = data.get(vm.pool_string("rng")) {
                    return Int16(rng.gen_range(start, end))
                }
            }
        }
        panic!()
    }
}