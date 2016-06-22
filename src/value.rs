//! The representation of Scheme values in RustyScheme.
//!
//! Inspired by the representation of Femtolisp.
//!
//! ### Representation
//!
//! | Type      | Representation |
//! |-----------|----------------|
//! |Fixnum     | As an immediate pointer, with tag 0 or 4.|
//! |Flonums    | As a pointer to a (boxed) floating-point number, with tag 1.|
//! |Pairs| As a pointer to a 2-tuple, with pointer tag 3. |
//! |Arrays| As an untagged, aligned pointer to a Rust slice. |
//! |Records| As a pointer to a Rust slice, with a special header for the GC that indicates how it should be marked.|
//! |Resources  | As a pointer into a 3-tuple, consisting of a GC header, a pointer to a `struct` that contains an object ID and custom equality, hashing, and other functions, and a pointer into memory not managed by the GC. |

use std::cell::Cell;
use std::{slice, ptr, mem};
use alloc::Allocator;
use std::marker::PhantomData;


// Same set used by Femtolisp
pub const NumTag: usize = 0b000;
pub const RustFuncTag: usize = 0b001;
pub const FunctionTag: usize = 0b010;
pub const VectorTag: usize = 0b011;
pub const NumTag2: usize = 0b100;
pub const RustDataTag: usize = 0b101;
pub const SymbolTag: usize = 0b110;
pub const PairTag: usize = 0b111;


#[cfg(target_pointer_width = "32")]
pub const SIZEOF_PTR: usize = 4;

#[cfg(target_pointer_width = "64")]
pub const SIZEOF_PTR: usize = 8;

pub const SIZEOF_PAIR: usize = (self::SIZEOF_PTR+0b111) & !0b111;

pub const HEADER_TAG: usize = 0b111 << (self::SIZEOF_PTR*8-3);

pub const PAIR_HEADER: usize = !HEADER_TAG | 2;


#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Tags {
    Num,
    RustFunc,
    Function,
    Vector,
    Num2,
    RustData,
    Symbol,
    Pair,
}


impl<'a> Value<'a> {
    #[inline(always)]
    pub fn raw_tag(&self) -> usize {
        self.contents & 0b111
    }

    #[inline(always)]
    pub fn tag(&self) -> Tags {
        use self::Tags::*;
        match self.raw_tag() {
            NumTag => Num,
            RustFuncTag => RustFunc,
            FunctionTag => Function,
            VectorTag => Vector,
            NumTag2 => Num2,
            RustDataTag => RustData,
            SymbolTag => Symbol,
            PairTag => Pair,
            _ => unsafe {
                enum Void {}
                match mem::transmute::<(),Void>(()) {}
            },
        }
    }
    #[inline(always)]
    pub fn leafp(&self) -> bool {
        self.raw_tag() & 0b11 != 0b11
    }
    #[inline(always)]
    pub fn both_fixnums(&self, other: &Self) -> bool {
        (self.contents | other.contents) & 0b11 == 0
    }
    #[inline(always)]
    pub fn self_evaluating(&self) -> bool {
        self.raw_tag() < 6
    }
    #[inline(always)]
    pub fn fixnump(&self) -> bool {
        self.raw_tag() & 0b11 == 0
    }
    #[inline(always)]
    pub fn pairp(&self) -> bool {
        self.tag() == Tags::Pair
    }
    #[inline(always)]
    pub fn flonump(&self) -> bool {
        unimplemented!()
    }
}



macro_rules! Ptr_Val {
    ($expr:expr) => {
        ($expr.contents & !0b111) as *mut Value
    }
}

macro_rules! size_of {
    ($ty:ty) => {
        ::std::mem::size_of::<$ty>()
    }
}
use alloc::Heap;

/// A Scheme value.
///
/// Scheme values are garbage collected, so must never appear outside
/// the heap, stack, or handles.  The GC will invalidate any other `Value`,
/// creating a dangling pointer.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct Value<'a> {
    phantom: PhantomData<&'a Heap<'a>>,
    pub contents: usize,
}

/// A Scheme "vector-like thing".
///
/// Vector-like things are Scheme values with tag `Tags::Vector`.
/// They all consist of a header followed by a slice of Scheme values.
/// The number of Scheme words is always computable by
///
/// ```rust
/// {
///     if (vector.tag() != Tags::Vector) {
///         None // not a vector-like thing
///     } else {
///         Some(unsafe {
///             *((vector.contents & !0b111) as *mut usize) &
///             (!0b111 << (::std::mem::size_of<usize>()*8 - 3))
///         })
///     }
/// }
/// ```
///
/// which is exposed as the method `len`.
///
/// Vector-like things have their own tags, in the 3 most significant bits
/// of the header word.  They have the following meanings:
///
/// | Tag |Meaning|
/// |-----|-------|
/// |0b000|Vector (chosen to simplify bounds checks)|
/// |0b001|Record.  The first word points to a record descriptor
/// used to identify the record type.|
/// |0b010|Closure.  The first word is a reference to a function object
/// that stores the function of the closure.|
/// |Others|Reserved.  These may be later used by the run-time system.
///
/// This struct **cannot** be moved, because it is followed by Scheme
/// objects that are not a part of the object.  As such, it has no public
/// constructors, and can only be instantiated by reference.
#[repr(C)]
pub struct Vector<'a> {
    /// Header.  Always ends in `0b000`.
    header: usize,

    /// Scheme values.
    data: [Value<'a>],
}

/// A descriptor for a `Record`.
pub struct RecordDescriptor {
    /// Always a multiple of 8, but never zero.
    id: usize,
}
#[repr(C)]
pub struct Record<'a> {
    /// Header.  Always ends in `0b000`, but is never zero.
    header: usize,

    /// Scheme values.
    data: [Value<'a>],
}

/// A (mutable) Scheme pair.  Subject to garbage collection.
#[repr(C)]
pub struct Pair<'a> {
    pub header: usize,
    pub car: Cell<Value<'a>>,
    pub cdr: Cell<Value<'a>>,
}

/// A Scheme closure.  Subject to garbage collection.
#[repr(C)]
pub struct Closure<'a> {
    pub header: usize,
    pub bytecode: Value<'a>,
    pub upvalues: [Value<'a>],
}

/// An object containing compiled Scheme bytecode.  Subject to garbage collection.
#[repr(C)]
pub struct BCO<'a> {
    /// Header.  Indicates that this is a BCO.
    header: usize,

    /// Phantom data to hold a lifetime.
    phantom: PhantomData<&'a Heap<'a>>,

    /// Actual bytecode
    contents: [u32],
}

impl<'a> Value<'a> {
    #[inline(never)]
    pub fn slow_add(first: &Self, second: &Self) -> Result<Self, SchemeError> {
        unimplemented!()
    }

    #[inline(always)]
    pub fn subtract(first: &Self, second: &Self) -> Self {
        unimplemented!()
    }

    pub fn size(&self) -> Option<usize> {
        if self.fixnump() {
            None
        } else {
            Some(unsafe {
                *(self.contents as *const usize) & HEADER_TAG
            })
        }
    }
}

pub struct SchemeError(String);
pub struct Bignum;
impl Bignum {
    pub fn new_from_fixnums(x: usize, y: usize) -> ! {
        unimplemented!()
    }
}
pub unsafe fn float_val(val: &Value) -> f64 {
    *((val.contents & 0b111) as *const f64)
}
pub struct HashTable;
pub struct IOPort;
pub struct RustData;
