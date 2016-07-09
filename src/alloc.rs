use std::fs::File;
use std::io::prelude::*;
use std::io::stdout;
use std::mem;
use std::ptr;
use std::slice;
use super::value;
use value::{Value, SIZEOF_PAIR, HEADER_TAG, PAIR_HEADER};
#[cfg(debug_assertions)]
use value::Tags;
/// An allocator for RustyScheme objects
pub trait Allocator {
    /// Allocates a vector
    fn alloc_vector(&mut self, &[Value]) -> value::Vector;

    /// Allocates a pair
    fn alloc_pair(&mut self, car: Value, cdr: Value);

    /// Allocates a closure
    fn alloc_closure(&mut self, bytecode: &value::BCO, upvalues: &[Value]) -> value::Closure;

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
    // fn alloc_float(&mut self, float: f64) -> value::Float;
}

#[derive(Debug)]
pub struct Heap {
    tospace: Vec<Value>,
    fromspace: Vec<Value>,
    pub stack: self::Stack,
}

/// Consistency checks on the whole heap (in debug mode only) – sloooow.
#[cfg(debug_assertions)]
unsafe fn consistency_check(heap: &Vec<Value>) {
    let assert_in_heap = |heap: &Vec<_>, ptr: usize| {
        assert!(ptr >= heap.as_ptr() as usize);
        let upper_limit = heap.as_ptr() as usize + heap.capacity() * size_of!(Value);
        assert!(ptr < upper_limit,
                "Heap pointer out of range: {} >= {}",
                ptr,
                upper_limit)
    };
    let mut index = 0;
    while index < heap.len() {
        let mut current = heap[index].clone();
        let len = current.get() as usize & !HEADER_TAG;
        assert!(len > 0);
        index += 1;
        for _ in 1..len {
            current = heap[index].clone();
            match current.tag() {
                Tags::Num | Tags::Num2 => {
                    assert!(current.get() & 0b11 == 0);
                }
                Tags::Pair => {
                    assert!(current.get() & 0b111 == 0b111);
                    assert!((*Ptr_Val!(current)).get() == PAIR_HEADER);
                    for i in 1..3 {
                        assert_in_heap(heap, Ptr_Val!(current).offset(i) as usize)
                    }
                }
                Tags::Vector => {
                    assert!(len > 0);
                    for i in 1..len {
                        assert_in_heap(heap, Ptr_Val!(current) as usize + i)
                    }
                }
                _ => unimplemented!(),
            }
            index += 1;
        }
    }
}

#[cfg(not(debug_assertions))]
unsafe fn consistency_check(_heap: &Vec<Value>) {}

/// Rounds the size of a heap object up to the nearest multiple of 8 bytes,
/// expressed in words.
fn align_word_size(size: usize) -> usize {
    let size_of_value = ::std::mem::size_of::<Value>();
    match size_of_value {
        4 => size + 1 & 0b1,
        8 => size,
        _ => ((size * size_of_value + 0b111) & !0b111) / size_of_value,
    }
}

/// Relocates a `Value` in the heap.
///
/// This function relocates a `Value` in the Scheme heap.  It takes two
/// arguments: `current`, the `Value` being relocated, and `end`, the current
/// end of tospace.
///
/// This function takes raw pointers because of aliasing concerns.
unsafe fn relocate(current: *mut Value, tospace: &mut Vec<Value>, fromspace: &mut Vec<Value>) {
    debug_assert!(tospace.capacity() >= fromspace.len());
    debug!("Tospace capacity: {}, Fromspace length: {}",
           tospace.capacity(),
           fromspace.len());
    let size_of_value: usize = size_of!(Value);
    (*current).size().map(|size| {
        // pointer to head of object being copied
        let pointer: *mut Value = Ptr_Val!(*current);

        // Assert that the object header is nonzero.
        debug_assert!((*pointer).get() != 0,
                      "internal error: copy_value: invalid object header size");
        if (*pointer).get() == HEADER_TAG {
            // Forwarding pointer detected (this header tag is otherwise absurd,
            // since no object can have a size of zero).
            *current = (&*pointer.offset(1)).clone()
        } else {
            let len = tospace.len();

            // End pointer
            let end = tospace.as_mut_ptr().offset(len as isize);

            let amount_to_copy = align_word_size(size);

            // Check that the amount to copy is reasonable
            debug_assert!(amount_to_copy > 0);

            // Check that the end pointer is aligned
            debug_assert!(end as usize & 0b111 == 0);

            // Check that the pointer really is to fromspace
            debug_assert!((pointer as usize) <
                          (fromspace.as_ptr() as usize + fromspace.len() *
                           size_of!(usize)));
            debug_assert!(pointer as usize >= fromspace.as_ptr() as usize);

            if cfg!(feature = "memcpy-gc") {
                let words_to_copy = amount_to_copy * size_of_value;
                // The amount to copy
                debug_assert!(amount_to_copy + len <= tospace.capacity());
                debug_assert!(pointer as usize >= end as usize + words_to_copy ||
                              pointer as usize + words_to_copy <= end as usize);
                // NOTE: reverse pointer argument order from `memcpy`.
                ptr::copy_nonoverlapping(pointer, end, amount_to_copy);
                tospace.set_len(len + amount_to_copy)
            } else {
                // NOTE: this MUST come before replacing the old object with
                // a forwarding pointer – otherwise, this replacement will
                // clobber the copied object's header!
                tospace.extend_from_slice(slice::from_raw_parts(pointer, amount_to_copy));
            }
            *pointer = Value::new(HEADER_TAG);
            *current = Value::new(end as usize | ((*current).get() & 0b111));
            *pointer.offset(1) = (*current).clone();
        }
    });
}

/// Process the heap.
unsafe fn scavange_heap(tospace: &mut Vec<Value>, fromspace: &mut Vec<Value>) {
    let mut offset: isize = 0;
    let current = tospace.as_mut_ptr();
    while offset < tospace.len() as isize {
        let size = (*current.offset(offset)).get() & !HEADER_TAG;
        assert!(size > 0);
        offset += 1;
        if !(*current).leafp() {
            for _ in 1..size {
                relocate(current.offset(offset), tospace, fromspace);
                offset += 1
            }
        }
    }
}

/// Handles all of the data on the stack.
unsafe fn scavange_stack(stack: &mut Vec<Value>,
                         tospace: &mut Vec<Value>,
                         fromspace: &mut Vec<Value>) {
    for i in stack.iter_mut() {
        relocate(i, tospace, fromspace);
    }
}

/// Performs a full garbage collection
fn collect(heap: &mut Heap) {
    debug!("Initiated garbage collection");
    unsafe {
        consistency_check(&heap.tospace);
        debug!("Completed first consistency check");
        mem::swap(&mut heap.tospace, &mut heap.fromspace);
        heap.tospace.reserve(heap.fromspace.len() + heap.fromspace.len() / 2);
        debug!("Fromspace size is {}",
               heap.fromspace.len() + heap.fromspace.len() / 2);
        heap.tospace.resize(0, Value::new(0));
        debug!("Tospace resized to {}", heap.tospace.capacity());
        let _ = stdout().flush();
        scavange_stack(&mut heap.stack, &mut heap.tospace, &mut heap.fromspace);
        debug!("Stack scavanged");
        scavange_heap(&mut heap.tospace, &mut heap.fromspace);
        debug!("Heap scavanged");
        consistency_check(&heap.tospace);
        debug!("Completed second consistency check");
        heap.fromspace.resize(0, Value::new(0));
    }
}

/// Represents the stack.
#[derive(Debug)]
pub struct Stack {
    pub innards: Vec<value::Value>,
}

use std::ops::{Deref, DerefMut};

/// A `Stack` acts like a `Vec`.
impl Deref for Stack {
    type Target = Vec<value::Value>;
    fn deref(&self) -> &Vec<value::Value> {
        &self.innards
    }
}

/// A `Stack` acts like a `Vec`.
impl DerefMut for Stack {
    fn deref_mut<'a>(&'a mut self) -> &'a mut Vec<value::Value> {
        &mut self.innards
    }
}

impl Heap {
    /// Allocates a Scheme pair, which must be rooted by the caller.
    pub fn alloc_pair(&mut self, car: Value, cdr: Value) {
        self.check_space(SIZEOF_PAIR);
        self.tospace.push(Value::new(PAIR_HEADER));
        self.tospace.push(car);
        self.tospace.push(cdr);
        let len = self.tospace.len() - 3;
        let new_value = Value::new(unsafe {
            self.tospace.as_ptr().offset(len as isize) as usize | value::PAIR_TAG
        });
        self.stack.push(new_value);
        debug!("Allocated a pair")
    }

    fn check_space(&mut self, space: usize) -> (usize, usize) {
        use value::SIZEOF_PTR;
        let real_space = align_word_size(space);
        let tospace_space = self.tospace.capacity() - self.tospace.len();
        if tospace_space < real_space {
            collect(self);
        }
        (self.tospace.as_ptr() as usize + self.tospace.len(),
         self.tospace.len() + real_space)
    }
    pub fn alloc_vector(&mut self, elements: &[Value]) {
        let (value_ptr, final_len) = self.check_space(elements.len());
        self.tospace.push(Value::new(value::VECTOR_HEADER | elements.len()));
        let ptr = value_ptr | value::VECTOR_TAG;
        self.tospace.extend_from_slice(elements);
        unsafe {
            self.tospace.set_len(final_len)
        };
        self.stack.push(Value::new(ptr));
    }

    pub fn alloc_closure(&mut self, argcount: u16, vararg: bool,
                         elements: &[Value]) {
        let (value_ptr, final_len) = self.check_space(elements.len());
        let ptr = value_ptr | value::VECTOR_TAG;
        self.tospace.push(Value::new(value::VECTOR_HEADER | elements.len() + 1));
        self.tospace.push(Value::new((argcount as usize) << 2 |
                                     (-(vararg as isize) as usize &
                                      ::std::isize::MIN as usize)));
        self.tospace.extend_from_slice(elements);
        unsafe {
            self.tospace.set_len(final_len)
        };
        self.stack.push(Value::new(ptr));
    }
    pub fn new(size: usize) -> Self {
        Heap {
            fromspace: Vec::with_capacity(size),
            tospace: Vec::with_capacity(size),
            stack: Stack { innards: Vec::with_capacity(1 << 16) },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use value::*;
    use std::cell::Cell;
    #[test]
    fn can_allocate_objects() {
        let zero: Value = Value { contents: Cell::new(0) };
        let mut heap = Heap::new(1 << 4);
        super::collect(&mut heap);
        debug!("HEADER_TAG = {:x}, PAIR_TAG = {:x}, SIZEOF_PAIR = {:x}",
               HEADER_TAG,
               PAIR_HEADER,
               SIZEOF_PAIR);
        heap.alloc_pair(zero.clone(), zero.clone());
        // debug!("{:?}", heap);
        for i in 1..((1 << 11)) {
            let old_pair = heap.stack[0].clone();
            let old_pair2 = old_pair.clone();
            heap.alloc_pair(old_pair, old_pair2);
            assert_eq!(heap.stack.len(), 2);
            assert_eq!(heap.stack[1].tag(), Tags::Pair);
            heap.stack[0] = heap.stack.pop().unwrap();
            let assert_valid = |heap: &Heap| {
                let ref new_pair = heap.stack[0];
                assert_eq!(heap.stack[0].tag(), Tags::Pair);
                assert_eq!(new_pair.size(), Some(3));
                if let Kind::Pair(ptr) = new_pair.kind() {
                    assert_eq!((unsafe { (*ptr).car.tag() }), Tags::Pair);
                    assert_eq!((unsafe { (*ptr).cdr.tag() }), Tags::Pair)
                } else {
                    unreachable!()
                }
            };
            assert_valid(&heap);
            super::collect(&mut heap);
            assert_valid(&heap);
            assert!(heap.tospace.len() >= 3 * i)
        }
        heap.stack.pop();
        assert!(heap.stack.len() == 0);
        // assert!(heap.fromspace.capacity() > 3* (1 << 20));
        // debug!("{:?}", heap);
        super::collect(&mut heap);
        assert!(heap.tospace.len() == 0)
    }
}
