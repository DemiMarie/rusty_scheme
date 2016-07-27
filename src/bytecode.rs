use std::ptr;
use value;
use alloc;

/// A bytecode object.  Consists of a header, the length of the bytecodes,
/// the actual bytecodes, and finally the constants vector (not actually part
/// of the BCO, but always allocated after it).
pub struct BCO {
    /// The standard header object
    header: usize,

    /// The length of the bytecodes
    bytecode_length: usize,

    /// Pointer to the constants vector
    pub constants_vector: value::Value,
}

pub enum BadByteCode {
    StackUnderflow { index: usize, depth: usize, min: usize },
}

pub fn allocate_bytecode(obj: &[u8], heap: &mut alloc::Heap) {
    let (val, _) = heap.alloc_raw(size_of!(BCO) + obj.len(), value::BYTECODE_HEADER_TAG);
    heap.stack.push(value::Value::new(val as usize));
    unsafe {
        ptr::copy_nonoverlapping(
            obj.as_ptr(),
            (val as *mut u8).offset(size_of!(BCO) as isize),
            obj.len())
    }
}

pub enum SchemeResult {
    BadBytecode(BadByteCode),
}
#[cfg(none)]
pub fn verify_bytecodes(b: &[u8], argcount: u16) -> Result<(), BadByteCode> {
    let argcount: usize = argcount.into();
    let mut i = 0;
    let mut max_stack = 0;
    let mut current_depth: usize = 0;
    let mut current_stack: Vec<usize> = vec![];
    let iter = b.iter();

    macro_rules! check_stack {
        ($min: expr) => (if current_depth < $exp {
            return Err(BadByteCode::StackUnderflow { index: i - 1,
                                                     depth: current_depth,
                                                     min: $min, })
        } else {})
    }
    while let Some(opcode) = iter.next() {
        i += 1;
        match try!(byte_to_opcode(opcode)) {
            Opcode::Cons => {
                check_stack!(2);
                current_depth -= 1;
            }
            Opcode::Car | Opcode::Cdr => {
                check_stack!(1);
                current_depth += 1;
            }
            Opcode::Vector => {
                try!(iter.next().ok_or(BadByteCode::EOF)).into()
            }
        }
    }
}
