use alloc;
use value;
use value::{Value};
pub fn exponential (_: Value, _: Value) -> ! { unimplemented!() }
pub fn slow_add (_alloc: alloc::Heap, _first: &mut Value, _other: &mut Value) -> ! { unimplemented!() }
/// Add two `Value`s, according to Scheme semantics.
///
/// The cases where both are fixnums or both are flonums is special-cased
/// as a fast path function, which is inlined into the interpreter.  The general case is much slower and put in a seperate function, which is not inlined.
/// function
#[inline(always)]
pub fn add<'a>(alloc: alloc::Heap, first: &mut Value, other: &mut Value)
           -> Result<Value, String> {
    if first.both_fixnums(other) {
        let res = Value {
            contents: (first.contents & !1) + other.contents,
        };
        if res.contents > first.contents {
            // Overflow!
            value::Bignum::new_from_fixnums(first.contents, other.contents)
        } else {
            Ok(res)
        }
    } else if first.flonump() && other.flonump() {
        // Multiply the `f64` values pointed to by the arguments
        //Ok(alloc.alloc_float(unsafe { float_val(first) * float_val(other) }))
        unimplemented!()
    } else {
        // Slow path.
        //return Err("non-fixnum addition not yet implemented".to_owned());
        self::slow_add(alloc, first, other)
    }
}
