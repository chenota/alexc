use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub enum MonoType {
    Variable(usize),
    Application(TypeName, Vec<MonoType>)
}

#[derive(Clone, PartialEq, Debug)]
pub enum TypeName {
    Int64,
    Char
}

pub type Context = HashMap<usize, MonoType>;
pub type Substitution = HashMap<usize, MonoType>;

pub trait DoesSub {
    fn applym(&self, m: &MonoType) -> MonoType;
    fn applyc(&self, c: &Context) -> Context;
}

impl DoesSub for Substitution {
    fn applym(&self, m: &MonoType) -> MonoType {
        match m {
            MonoType::Variable(x) => match self.get(x) {
                Some(x) => x.clone(),
                None => m.clone()
            },
            MonoType::Application(name, ls) => MonoType::Application(
                name.clone(), 
                ls.iter().map(|r| self.applym(r)).collect()
            )
        }
    }
    fn applyc(&self, c: &Context) -> Context {
        // New context
        let mut new_c = Context::new();
        // Apply self to each key-value pair
        for (k, v) in c {
            new_c.insert(*k, self.applym(v));
        };
        // Return
        new_c
    }
}