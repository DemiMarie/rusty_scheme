use value;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::cell::{UnsafeCell, Cell};
use std::rc::Rc;

pub type StackElement = usize;

/// This struct stores a symbol.
///
/// Symbols are never allocated on the GC heap.  They are instead stored
/// on the Rust heap in `SymbolTable` objects, which contain a `HashMap<Rc<str>, Symbol>`
/// that stores the actual symbols.  Each symbol contains a name.
///
/// Symbols always have tag `value::SYMBOL_TAG`.
#[derive(Debug)]
pub struct Symbol {
    /// The name of the symbol
    name: Rc<String>,

    /// A stack used for unspecified purposes in the compiler, such as scope handling.
    /// Must not contain Scheme values.
    pub stack: Vec<StackElement>,

    /// The contents
    pub contents: UnsafeCell<value::Value>,

    /// Is this alive?
    pub alive: Cell<bool>,
}

impl Symbol {
    pub fn name(&self) -> Rc<String> {
        self.name.clone()
    }
    pub fn new(name: Rc<String>) -> Self {
        Symbol {
            contents: UnsafeCell::new(value::Value::new(value::FALSE)),
            name: name,
            stack: vec![],
            alive: Cell::new(false),
        }
    }
}

/// A symbol table.
///
/// The symbol table consists of raw pointers to the strings,
/// which are stored on the GC heap.
///
/// WARNING: keep this in sync with the GC!  This code does manual relocation
/// of heap pointers!
#[derive(Debug)]
pub struct SymbolTable {
    pub contents: HashMap<Rc<String>, Box<Symbol>>,
}

impl SymbolTable {
    pub fn fixup(&mut self) {
        let mut vec = vec![];
        for (i, sym) in &self.contents {
            if sym.alive.get() {
                sym.alive.set(false)
            } else {
                vec.push(i.clone())
            }
        }
        // Loop through the dead objects and remove them from the hash table.
        for i in vec {
            match self.contents.entry(i.clone()) {
                Entry::Occupied(o) => drop(o.remove()),
                Entry::Vacant(_) => {
                    bug!("SymbolTable::fixup: entry \
                          to be deleted is already vacant")
                }
            }
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable { contents: HashMap::new() }
    }
}
