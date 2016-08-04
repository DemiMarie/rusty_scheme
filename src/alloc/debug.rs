//! Expensive, debug-mode-only consistency checks on the entire heap.

use value;
use value::{Value, HEADER_TAG, Tags};
use symbol;
use super::{PAIR, VECTOR, BYTECODE, RUSTDATA};

/// Consistency checks on the whole heap (in debug mode only) – sloooow.
pub unsafe fn consistency_check(heap: &[Value]) {
    if cfg!(debug_assertions) {
        let mut index = 0;
        while index < heap.len() {
            let current = heap[index].clone();
            let len = current.get() as usize & !HEADER_TAG;
            assert!(len > 1);
            index += 1;
            match current.get() as usize & HEADER_TAG {
                PAIR | VECTOR => {
                    for x in 1..len {
                        debug_assert_valid_value(heap, index, x, len);
                        index += 1;
                    }
                }
                BYTECODE | RUSTDATA => {
                    // do nothing, these are not scanned
                }
                _ => bug!("Strange header {:x}", current.get() as usize),
            }
        }
    }
}

/// Assert a value is valid (in debug mode)
///
/// Parameters:
///
/// - `heap`: the current tospace
/// - `index`: the index into the heap
unsafe fn debug_assert_valid_value(heap: &[Value], index: usize, x: usize, len: usize) {
    let current = heap[index].clone();
    if current.get() < 0xFF {
        return;
    }
    match current.tag() {
        Tags::Num | Tags::Num2 => {
            assert!(current.get() & 0b11 == 0);
        }
        Tags::Pair => {
            assert!(current.get() & 0b111 == 0b111);
            assert_valid_heap_pointer(heap, &current);
            if (*current.as_ptr()).get() != value::PAIR_HEADER {
                bug!("BAD PAIR: header length is \
                      0x{:x} and not \
                      0x{:x} at index 0x{:x} into heap and index \
                      0x{:x} into block",
                     ((*current.as_ptr()).get()),
                     value::PAIR_HEADER,
                     index,
                     x);
            }
            for i in 1..3 {
                assert_valid_heap_pointer(heap,
                                         &*(current.as_ptr().offset(i as isize) as *const Value))
            }
        }
        Tags::Vector => {
            assert_valid_heap_pointer(heap, &current);
            for i in 1..len {
                assert_valid_heap_pointer(heap, &*current.as_ptr().offset(i as isize))
            }
        }
        Tags::Symbol => {
            let aligned_size = super::align_word_size(size_of!(symbol::Symbol) /
                                                      size_of!(usize));
            assert!(len == aligned_size,
                    "len = {:x}, aligned_size = {:x}", len, aligned_size);
            assert_valid_heap_pointer(heap, &current);
            assert_valid_heap_pointer(heap, &*current.as_ptr().offset(1))
        }
        Tags::RustData => /* not scanned */ {}
        Tags::Function|Tags::RustFunc => panic!("not yet implemented: tag {:?} of {:x}", current.tag(), current.get())
    }
}

pub fn assert_valid_heap_pointer(vec: &[Value], i: &Value) {
    if cfg!(debug_assertions) {
        let lower_limit = vec.as_ptr() as usize;
        let upper_limit = lower_limit + vec.len() * size_of!(usize);
        let contents = i.contents.get();
        let untagged = contents & !0b111;
        if !(contents & 0b11 == 0 || contents < 0xFF || contents & 0b111 == 0b110 ||
             (untagged >= lower_limit && untagged < upper_limit)) {
            let contents = contents;
            bug!("argument not fixnum or pointing into \
                  tospace: {:x}",
                 contents)
        }
    }
}
