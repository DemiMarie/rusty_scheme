use std::fs::File;
use std::mem;
use std::ptr;

use super::value;
use value::{Value, SIZEOF_PAIR, HEADER_TAG, PAIR_HEADER};

/// An allocator for RustyScheme objects
pub trait Allocator {
    /// Allocates a vector
    fn alloc_vector(& mut self, &[Value]) -> value::Vector;

    /// Allocates a pair
    fn alloc_pair(& mut self, car: Value, cdr: Value);

    /// Allocates a closure
    fn alloc_closure(&mut self,
                     bytecode: &value::BCO,
                     upvalues: &[Value])
                     -> value::Closure;

    /// Allocates a record
    fn alloc_record(&mut self,
                    descriptor: &value::RecordDescriptor,
                    fields: &[Value])
                    -> value::Record;

    /// Allocates a hash table
    fn alloc_hash_table(&mut self, size: usize) -> value::HashTable;

    /// Allocates a port
    fn alloc_port(&mut self, File) -> value::IOPort;

    /// Allocates a rustdata, which contains an arbitrary Rust object
    fn alloc_rustdata<T>(&mut self, object: &T) -> value::RustData;

    // /// Allocates a boxed float on the top of the stack.
    //fn alloc_float(&mut self, float: f64) -> value::Float;
}
#[derive(Debug)]
pub struct Heap {
    tospace: Vec<Value>,
    fromspace: Vec<Value>,
    pub stack: self::Stack,
}

/// Relocates a `Value` in the heap.
///
/// This function relocates a `Value` in the Scheme heap.  It takes two
/// arguments: `current`, the `Value` being relocated, and `end`, the current
/// end of tospace.
///
/// This function takes raw pointers because of aliasing concerns.
unsafe fn relocate(current: *mut Value, end: *mut*mut Value) {
    (*current).size().map(|size| {
        // pointer to head of object being copied
        let pointer = Ptr_Val!(*current);

        // Assert that the object header is nonzero.
        debug_assert!((*pointer).contents != 0,
                      "internal error: copy_value: invalid object header size");
        if (*pointer).contents == HEADER_TAG {
            // Forwarding pointer detected (this header tag is otherwise absurd,
            // since no object can have a size of zero).
            *current = *pointer.offset(1)
        } else {
            // The amount to copy
            let amount_to_copy = (size*size_of!(usize) + 0b111) & !0b111;

            ptr::copy_nonoverlapping(pointer, *end, amount_to_copy);
            *current = Value{ contents: (*end) as usize | (*current).raw_tag() };
            *end = (*end).offset(amount_to_copy as isize >> 3)
        }
    });
}

fn collect(heap: &mut Heap) {
    mem::swap(&mut heap.tospace, &mut heap.fromspace);
    assert!(heap.tospace.capacity() >= heap.fromspace.len());
    //let fromspace_length = heap.fromspace.len(); // Fromspace length
    let tospace_length = heap.tospace.len();
    //assert!(fromspace_length <= tospace_length);
    let ref mut tospace = heap.tospace;
    let mut end_ptr = tospace.as_mut_ptr();
    let start_ptr = end_ptr;

    // We iterate over the stack differently than we iterate over the heap.

    // Stack are all GC roots
    for i in heap.stack.iter_mut() {
        unsafe {
            relocate(i, &mut end_ptr);
            tospace.set_len((end_ptr as usize - start_ptr as usize)/size_of!(usize))
        }
    }

    // Raw pointer to tospace
    let mut current = tospace.as_mut_ptr();
    debug_assert!(end_ptr as usize - current as usize >= heap.stack.len());

    let end_of_tospace = tospace.as_mut_ptr() as usize + tospace_length;

    // Heap consists of contiguous values, so loop over it.
    while (current as usize) < end_of_tospace {
        unsafe {
            let size = mem::transmute::<_, usize>(*current) & !HEADER_TAG;
            current = current.offset(1);
            if !(*current).leafp() {
                for _ in 1..size {
                    relocate(current, &mut end_ptr);
                    current = current.offset(1)
                }
            }
        }
    }

    // Set the size of the new fromspace
    heap.fromspace.resize(((end_of_tospace as u64 - tospace.as_ptr() as u64)/8 *
                          3/2) as usize, unsafe { mem::transmute(1 as usize) });
    let ptr = tospace.as_ptr();
    unsafe {
        tospace.set_len((end_of_tospace - ptr as usize)/size_of!(usize))
    }
}

#[derive(Debug)]
pub struct Stack {
    pub innards: Vec<value::Value>,
}
#[cfg(none)]
impl ::std::ops::Index<usize> for Stack {
    type Output = value::Value;
    fn index(&self, other: u8) -> &value::Value {
        let val: usize = other.into();
        &self.innards[val]
    }
}

use ::std::ops::{Deref, DerefMut};
impl Deref for Stack {
    type Target = Vec<value::Value>;
    fn deref(&self) -> &Vec<value::Value> {
        &self.innards
    }
}

impl DerefMut for Stack {
    fn deref_mut<'a>(&'a mut self) -> &'a mut Vec<value::Value> {
        &mut self.innards
    }
}

impl Heap {
    /// Allocates a Scheme pair, which must be rooted by the caller.
    pub fn alloc_pair(&mut self, car: Value, cdr: Value) {
        let tospace_space = self.tospace.capacity() - self.tospace.len();
        if tospace_space < SIZEOF_PAIR {
            collect(self);
        }
        self.tospace.push(Value{ contents: PAIR_HEADER });
        self.tospace.push(car);
        self.tospace.push(cdr);
        let len = self.tospace.len() - 3;
        let new_value = Value {
            contents: unsafe {
                self.tospace.as_ptr().offset(len as isize) as usize |
                value::PAIR_TAG
            },
        };
        self.stack.push(new_value)
    }
    pub fn new(size: usize) -> Self {
        Heap {
            fromspace: Vec::with_capacity(size),
            tospace: Vec::with_capacity(size),
            stack: Stack{ innards: Vec::with_capacity(1 << 16) },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use value::*;
    const ZERO: Value = Value { contents: 0, };
    #[test]
    fn can_allocate_objects() {
        let mut heap = Heap::new(1 << 16);
        super::collect(&mut heap);
        println!("HEADER_TAG = {:x}, PAIR_TAG = {:x}, SIZEOF_PAIR = {:x}", HEADER_TAG, PAIR_HEADER, SIZEOF_PAIR);
        heap.alloc_pair(ZERO, ZERO);
        println!("{:?}", heap);
        for i in 1..((1 << 20) - 1000) {
            let old_pair = heap.stack[0];
            heap.alloc_pair(old_pair, old_pair);
            assert_eq!(heap.stack.len(), 2);
            assert_eq!(heap.stack[1].tag(), Tags::Pair);
            heap.stack[0] = heap.stack.pop().unwrap();
            let assert_valid = |heap: &Heap| {
                let new_pair = heap.stack[0];
                assert_eq!(heap.stack[0].tag(), Tags::Pair);
                assert_eq!(new_pair.size(), Some(3));
                if let EnumValue::Pair(ptr) = new_pair.enum_type() {
                    assert_eq!((unsafe { (*ptr).car.get().tag() }), Tags::Pair);
                    assert_eq!((unsafe { (*ptr).cdr.get().tag() }), Tags::Pair);
                } else {
                    unreachable!()
                }
            };
            assert_valid(&heap);
            super::collect(&mut heap);
            assert_valid(&heap);
            assert!(heap.tospace.len() == 3*i)
        }
        assert!(heap.fromspace.capacity() > 3* (1 << 20));
        println!("{:?}", heap);
        {
            super::collect(&mut heap);
        }
    }
}
