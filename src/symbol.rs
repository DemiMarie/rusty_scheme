use value;
use std::collections::hash_map::Entry;
use std::hash::{Hash, Hasher};
use std::collections;
use std::cmp::{PartialEq, Eq};
use std::cell::Cell;
use std::ptr;
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
    pub header: usize,

    /// Value (the value of the symbol)
    pub value: value::Value,

    /// The name of the symbol
    pub name: String,
}

// Invariant: always points to a valid `str`
#[derive(Clone)]
pub struct Key(pub Cell<*mut Symbol>);
use std::fmt;
impl fmt::Debug for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Key(Cell(@{:p}))", self.0.get())
    }
}
impl Key {
    pub fn get(&self) -> *mut Symbol {
        self.0.get()
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
    pub fn entry(&mut self, other: Key) -> Entry<Key, ()> {
        self.contents.entry(other)
    }
    pub fn fixup(&mut self) {
        let mut vec = vec![];
        for (i, &()) in self.contents.iter() {
            let ptr = i.0.get();
            let sym = unsafe { &*ptr };
            if sym.header == value::HEADER_TAG {
                // Forwarding pointer.  That means the symbol is live.
                // Relocate it.
                i.0.set(unsafe {
                    *(sym.value.get() as *const *mut Symbol)
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
                Entry::Vacant(_) => bug!("SymbolTable::fixup: entry \
                                          to be deleted is already vacant"),
            }
            let sym = i.0.get();
            unsafe {
                ptr::drop_in_place(sym)
            }
        }
    }
    pub fn new() -> Self {
        SymbolTable {
            contents: collections::HashMap::new(),
        }
    }
}
