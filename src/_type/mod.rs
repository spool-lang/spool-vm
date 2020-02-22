use std::rc::Rc;
use crate::instance::Function;
use crate::instance::Function::NativeInstance;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Type {
    pub(crate) canonical_name: Rc<String>,
    supertype: Option<Rc<Type>>,
    instance_functions: HashMap<Rc<String>, Function>
}

impl Type {
    pub fn new(canonical_name: Rc<String>, supertype: Option<Rc<Type>>, instance_functions: HashMap<Rc<String>, Function>) -> Type {
        Type {
            canonical_name,
            supertype,
            instance_functions
        }
    }

    pub fn get_canonical_name(&self) -> Rc<String> {
        let mut actual_name = format!("{}", self.canonical_name);
        Rc::new(actual_name)
    }

    pub(crate) fn matches_type(&self, other: Rc<Type>) -> bool {
        let mut other = other;

        loop {
            if &*self.canonical_name == &*other.canonical_name {
                return true
            }
            match &other.supertype {
                None => return false,
                Some(thing) => other = Rc::clone(thing),
            }
        }
    }

    pub(crate) fn get_instance_func(&self, name: Rc<String>) -> Function {
        let sup_op = self.supertype.clone();
        return match self.instance_functions.get(&*name.clone()) {
            None => match sup_op {
                None => panic!(),
                Some(sup) => sup.get_instance_func(name),
            },
            Some(thing) => thing.clone(),
        }
    }
}

struct TypeBuilder {
    canonical_name: Rc<String>,
    supertype: Option<Rc<Type>>,
    instance_functions: HashMap<Rc<String>, Function>
}

impl TypeBuilder {
    fn new(canonical_name: Rc<String>) -> TypeBuilder {
        TypeBuilder {
            canonical_name,
            supertype: None,
            instance_functions: Default::default()
        }
    }

    fn supertype(mut self, supertype: Rc<Type>) -> TypeBuilder {
        self.supertype = Some(supertype);
        return self
    }

    fn instance_function(mut self, name: Rc<String>, instance_function: Function) -> TypeBuilder {
        match instance_function {
            Function::NativeInstance(_, _) => self.instance_functions.insert(name, instance_function),
            _ => panic!(),
        };
        self
    }

    fn build(self) -> Type {
        Type::new(self.canonical_name, self.supertype, self.instance_functions)
    }
}

pub(crate) mod string_type {
    use crate::vm::{NewVM, TypeRegistry};
    use crate::instance::{Instance, Function};
    use crate::instance::Instance::{Str, Void};
    use crate::_type::TypeBuilder;
    use crate::string_pool::StringPool;
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.String"))
            .supertype(type_registry.get(Rc::new("silicon.core.Object".to_string())))
            .instance_function(string_pool.pool_str("capitalize"), Function::NativeInstance(0, capitalize))
            .build();
        type_registry.register(_type)
    }

    fn capitalize(vm: &mut NewVM, instance: Instance, args: Vec<Instance>) -> Instance {
        if let Str(string) = instance {
            let capitalized = vm.pool_string(string.to_uppercase().as_str());
            return Str(capitalized)
        };
        panic!()
    }
}