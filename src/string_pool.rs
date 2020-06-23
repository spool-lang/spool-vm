use std::collections::{HashSet, HashMap};
use std::rc::Rc;

pub struct StringPool {
    pool: HashSet<Rc<String>>,
    new_pool: HashMap<usize, String>,
}

impl StringPool {
    pub(crate) fn new() -> StringPool {
        StringPool {
            pool: Default::default(),
            new_pool: Default::default()
        }
    }

    pub(crate) fn pool<T: ToString>(&mut self, t: T) -> Rc<String> {
        let string = t.to_string();
        return match self.pool.get(&string) {
            Some(rc) => Rc::clone(&rc),
            None => {
                let rc: Rc<String> = Rc::from(string);
                self.pool.insert(Rc::clone(&rc));
                rc
            }
        }
    }

    pub(crate) fn resolve(&self, key: Key) -> Option<&str> {
        match self.new_pool.get(&key.0) {
            None => None,
            Some(string) => Some(string.as_str()),
        }
    }
}

pub struct Key(usize);

impl Clone for Key {
    fn clone(&self) -> Self {
        Key(self.0)
    }
}