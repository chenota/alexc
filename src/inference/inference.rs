use std::{collections::HashMap, sync::atomic::{AtomicUsize, Ordering}};

#[macro_export]
macro_rules! sub {
    [] => {
        Substitution::new()
    };
    [$($e1:expr => $e2:expr),+] => {
        {
        let mut sub = Substitution::new();
        $(
            sub.insert($e1, $e2);
        )+
        sub
        }
    };
}

#[macro_export]
macro_rules! var {
    ($l:literal) => {
        MonoType::Variable($l)
    }
}

#[macro_export]
macro_rules! app {
    ($e1:expr $(=> $($e2:expr),+)?) => {
        MonoType::Application(
            $e1,
            vec![ $($($e2),+)? ]
        )
    }
}

#[derive(Clone, Debug)]
pub enum MonoType {
    Variable(usize),
    Application(TypeName, Vec<MonoType>)
}
impl PartialEq for MonoType {
    fn eq(&self, x: &Self) -> bool {
        match (self, x) {
            // Both variables, check IDs
            (MonoType::Variable(x1), MonoType::Variable(x2)) => x1 == x2,
            // Both applications, check names and contents
            (MonoType::Application(n1, v1), MonoType::Application(n2, v2)) => {
                n1 == n2 && 
                v1.len() == v2.len() && 
                v1.iter().zip(v2).all(|(r1, r2)| r1 == r2)
            },
            // Not same type, false
            _ => false
        }
    }
}
impl MonoType {
    pub fn contains(&self, t2: &MonoType) -> bool {
        // Check self
        match self {
            // Self is type application
            MonoType::Application(_, v) => {
                // Any parameters contain t2
                v.iter().any(|r| r.contains(t2))
            },
            // Self is variable
            _ => self == t2
        }
    }
    pub fn unify(&self, t2: &MonoType) -> Result<Substitution, String> {
        match (self, t2) {
            // Both variables
            (MonoType::Variable(x1), MonoType::Variable(x2)) => {
                // Same variable, don't map
                if x1 == x2 {
                    Ok(sub![])
                }
                // Different variables, map self to t2 
                else {
                    Ok(sub![*x1 => t2.clone()])
                }
            },
            // Variable and application
            (MonoType::Variable(x1), _) => {
                // Check for infinite type
                if t2.contains(self) { return Err("Infinite type detected".to_string()) }
                // Map self to t2
                Ok(sub![*x1 => t2.clone()])
            }
            // Any and variable
            (_, MonoType::Variable(_)) => {
                // Unify b and a
                t2.unify(self)
            },
            // Both applications
            (MonoType::Application(n1, v1), MonoType::Application(n2, v2)) => {
                // Check that type names are the same
                if n1 != n2 { return Err("Could not unify types due to different type functions".to_string()) }
                // Check that lengths are the same
                if v1.len() != v2.len() { return Err("Could not unify types due to different lengths".to_string()) }
                // Empty substitution
                let mut s = sub![];
                // Iterate through v1, v2 and combine
                for (x1, x2) in v1.iter().zip(v2) {
                    s = s.combine(&s.applym(x1).unify(&s.applym(x2))?)
                };
                // Return
                Ok(s)
            }
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum TypeName {
    Int64,
    Char,
    Tuple
}

pub type Context = HashMap<usize, MonoType>;
pub type Substitution = HashMap<usize, MonoType>;

pub trait DoesSub {
    fn applym(&self, m: &MonoType) -> MonoType;
    fn applyc(&self, c: &Context) -> Context;
    fn combine(&self, s: &Substitution) -> Substitution;
}

impl DoesSub for Substitution {
    fn applym(&self, m: &MonoType) -> MonoType {
        match m {
            // Variable: Take mapping if exists, otherwise clone original
            MonoType::Variable(x) => match self.get(x) {
                Some(x) => x.clone(),
                None => m.clone()
            },
            // Applicaton: Apply to all sub-types
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
    fn combine(&self, s: &Substitution) -> Substitution {
        // Copy top-level (s1, self)
        let mut s_new = self.clone();
        // Insert k -> s1(v) for each item (k, v) in s2
        for (k, v) in s { s_new.insert(*k, self.applym(v)); };
        // Return
        s_new
    }
}

pub fn uniqvar() -> MonoType {
    // Static variable to keep track of unique IDs
    static UID: AtomicUsize = AtomicUsize::new(0);
    // Load value
    let val = UID.load(Ordering::Relaxed);
    // Update value
    UID.store(val + 1, Ordering::Relaxed);
    // Return
    MonoType::Variable(val)
}