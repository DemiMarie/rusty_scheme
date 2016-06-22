use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::marker::PhantomData;
use std::mem;
use std::ptr;

#[macro_use]
use super::value;
use value::{Value, SIZEOF_PTR, SIZEOF_PAIR, HEADER_TAG, PAIR_HEADER};

/// An allocator for RustyScheme objects
pub trait Allocator<'a> {
    /// Allocates a vector
    fn alloc_vector<'b>(&'a mut self, &'b [Value]) -> value::Vector<'a>;

    /// Allocates a pair
    fn alloc_pair(&'a mut self, car: Value, cdr: Value);

    /// Allocates a closure
    fn alloc_closure(&'a mut self,
                         bytecode: &value::BCO,
                         upvalues: &[Value])
                         -> value::Closure<'a>;

    /// Allocates a record
    fn alloc_record(&'a mut self,
                    descriptor: &value::RecordDescriptor,
                    fields: &[Value])
                    -> value::Record<'a>;

    /// Allocates a hash table
    fn alloc_hash_table(&'a mut self, size: usize) -> value::HashTable;

    /// Allocates a port
    fn alloc_port(&mut self, File) -> value::IOPort;

    /// Allocates a rustdata, which contains an arbitrary Rust object
    fn alloc_rustdata<T>(&'a mut self, object: &'a T) -> value::RustData;

    // /// Allocates a boxed float on the top of the stack.
    //fn alloc_float(&mut self, float: f64) -> value::Float;
}

pub struct Heap<'a> {
    tospace: Vec<Value<'a>>,
    fromspace: Vec<Value<'a>>,
    pub stack: Vec<Value<'a>>,
}

unsafe fn copy_value(current: &mut Value, end: &mut *mut Value, size: usize) {
    ptr::copy_nonoverlapping(*end, (Ptr_Val!(current) as usize) as *mut Value, size_of!(usize) * (size + 1));
    *end = ((*end as usize + size *
             mem::size_of::<usize>())+0b111 & !0b111) as *mut Value
}

impl<'a> Heap<'a> {
    fn collect(&mut self) {
        mem::swap(&mut self.tospace, &mut self.fromspace);
        let (mut tospace_ptr, mut fromspace_ptr) = (0, 0);
        let current = 0; // Current object
        let fromspace_length = self.fromspace.len(); // Fromspace length
        let tospace_length = self.tospace.len();
        let tospace = self.tospace;
        assert!(fromspace_length < tospace_length);

        let mut end_ptr = tospace.as_mut_ptr();

        // We iterate over the stack differently than we iterate over the heap.

        // Stack are all GC roots
        for i in self.stack {
            i.size().map(|size| {
                copy_value(&mut i, &mut end_ptr, size)
            });
        }

        // Raw pointer to tospace
        let mut current = tospace.as_mut_ptr();

        let end_of_tospace = tospace.as_mut_ptr() as usize + tospace_length;

        // Heap consists of contiguous values, so loop over it.
        while (current as usize) < end_of_tospace {
            // Similar to the case of the stack.
            let size = mem::transmute::<_, usize>(*current) & !HEADER_TAG;
            unsafe { current = current.offset(1) }
            if !(*current).leafp() {
                for _ in 0..size {
                    (*current).size().map(|size| {
                        copy_value(&mut unsafe {*current}, &mut end_ptr, size)
                    });
                    current = current.offset(1)
                }
            }
        }

        // Set the size of the new fromspace
        self.fromspace.resize((end_of_tospace - tospace.as_ptr() as usize) *
                                    3/2, mem::transmute(0 as usize))
    }
}

impl<'a> Heap<'a> {
    /// Allocates a Scheme pair and pushes it onto the stack.
    fn alloc_pair(&mut self, car: Value, cdr: Value) {
        let tospace_space = self.tospace.capacity() - self.tospace.len();
        if tospace_space < SIZEOF_PAIR {
            self.collect()
        }
        self.stack.push(unsafe {
            let ptr = self.tospace.as_mut_ptr() as *mut value::Pair;
            (*ptr).header = PAIR_HEADER;
            (*ptr).car.set(car);
            (*ptr).cdr.set(cdr);
            self.tospace.set_length(self.tospace.len() + SIZEOF_PAIR);
            Value {
                contents: ptr as usize | value::PairTag,
                phantom: PhantomData,
            }
        })
    }
}
