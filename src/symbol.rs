use value;
use std::hash::{Hash, Hasher};
use std::collections;
use std::cmp::{PartialEq, Eq};
use std::cell::Cell;

/// This struct stores a symbol.
///
/// Contract with runtime: the header length is always the size of the symbol
/// (as a usize).
///
/// Symbols always have tag `value::SYMBOL_TAG`.
#[repr(C)]
#[derive(Debug)]
pub struct Symbol {
    /// Header for the GC
    header: usize,

    /// Value (the value of the symbol)
    value: value::Value,

    /// The name of the symbol
    name: str,
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
    contents: collections::HashMap<Key, ()>,
}

impl SymbolTable {
    pub fn fixup(&mut self) {
        use std::collections::hash_map::Entry;
        let mut vec = vec![];
        for (i, _) in self.contents.iter() {
            let ptr = i.0.get() as *const usize;
            let header = unsafe { *ptr };
            if header == value::HEADER_TAG {
                // Forwarding pointer.  That means the symbol is live.
                // Relocate it.
                i.0.set(unsafe {
                    *((ptr as usize + 8) as *const *const Symbol)
                })
            } else {
                // The symbol is dead.  Place it on a queue of moribund symbols.
                vec.push(i.clone())
            }
        }
        // Loop through the dead objects and remove them from the hash table.
        for i in vec {
            match self.contents.entry(i.clone()) {
                Entry::Occupied(o) => o.remove(),
                Entry::Vacant(_) => panic!("internal error: SymbolTable::fixup: \
                                            entry to be deleted is already \
                                            vacant"),
            }
        }
    }
    pub fn new() -> Self {
        SymbolTable {
            contents: collections::HashMap::new(),
        }
    }
}

// Invariant: always points to a valid `str`
#[derive(Clone)]
struct Key(Cell<*const Symbol>);
use std::fmt;
impl fmt::Debug for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Key(Cell(@{:p}))", self.0.get())
    }
}

impl Hash for Key {
    fn hash<H: Hasher>(&self, h: &mut H) {
        unsafe {
            (*self.0.get()).name.hash(h)
        }
    }
}

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            (*self.0.get()).name == (*other.0.get()).name
        }
    }
}

impl Eq for Key {}
