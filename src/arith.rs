use alloc;
use value::{Value};
pub fn exponential (_: Value, _: Value) -> ! { unimplemented!() }
pub fn slow_add (_alloc: alloc::Heap, _first: &mut Value, _other: &mut Value) -> ! { unimplemented!() }
/// Add two `Value`s, according to Scheme semantics.
///
/// The cases where both are fixnums or both are flonums is special-cased
/// as a fast path function, which is inlined into the interpreter.  The general case is much slower and put in a seperate function, which is not inlined.
/// function
#[inline(always)]
pub fn add<'a>(_alloc: &mut alloc::Heap, first: &Value, other: &Value)
           -> Result<Value, String> {
    if first.both_fixnums(other) {
        let res = (first.get() & !1).checked_add(other.get());
        res.ok_or("overflow not yet implemented".to_owned())
           .map(Value::new)
        /*
        if res.contents > first.contents {
            // Overflow!
            value::Bignum::new_from_fixnums(first.contents, other.contents)
        } else {
            Ok(res)
        }*/
    } else if first.flonump() && other.flonump() {
        // Multiply the `f64` values pointed to by the arguments
        //Ok(alloc.alloc_float(unsafe { float_val(first) * float_val(other) }))
        //unimplemented!()
        Err("flonums not yet implemented".to_owned())
    } else {
        // Slow path.
        Err("non-fixnum addition not yet implemented".to_owned())
        //
        //self::slow_add(alloc, first, other)
    }
}
#[inline(always)]
pub fn subtract(_alloc: &mut alloc::Heap, first: &Value, other: &Value)
           -> Result<Value, String> {
    if first.both_fixnums(other) {
        let res = (first.get() & !1).checked_sub(other.get());
        res.ok_or("overflow not yet implemented".to_owned())
           .map(Value::new)
    } else if first.flonump() && other.flonump() {
        Err("flonums not yet implemented".to_owned())
    } else {
        Err("non-fixnum addition not yet implemented".to_owned())
    }
}

#[inline(always)]
pub fn multiply(_alloc: &mut alloc::Heap, first: &Value, other: &Value)
           -> Result<Value, String> {
    if first.both_fixnums(other) {
        let res = (first.get() & !1).checked_mul(other.get());
        res.ok_or("overflow not yet implemented".to_owned())
           .map(Value::new)
    } else if first.flonump() && other.flonump() {
        Err("flonums not yet implemented".to_owned())
    } else {
        Err("non-fixnum addition not yet implemented".to_owned())
    }
}

#[inline(always)]
pub fn divide(_alloc: &mut alloc::Heap, first: &Value, other: &Value)
           -> Result<Value, String> {
    if first.both_fixnums(other) {
        let (first, other) = (first.get() & !3, other.get() & !3);
        let res = (first.get() & !1).checked_div(other.get());
        res.ok_or("overflow not yet implemented".to_owned())
           .map(Value::new)
    } else if first.flonump() && other.flonump() {
        Err("flonums not yet implemented".to_owned())
    } else {
        Err("non-fixnum addition not yet implemented".to_owned())
    }
}
