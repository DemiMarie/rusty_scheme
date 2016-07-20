//! The public Rust embedding API of RustyScheme.  Very unstable.
//!
//! This API is similar to Lua's embedding API, in that an explicit stack is
//! used.
//!
//! Example:
//!
//! ```rust
//! use rusty_scheme;
//! let mut interp = rusty_scheme::State::new();
//!
//! // Push onto the stack.  Always works, unless the interpreter
//! // hits the memory limit set by the embedder (not yet implemented).
//! assert!(interp.push(23).is_ok());
//! assert!(interp.push(175).is_ok());
//!
//! // Compute the sum of these numbers
//! //assert!(interp.sum(-1, -2).is_ok());
//! //assert!(interp.pop().unwrap() == 198);
//!
//! // Now something more interesting: executing Scheme source code.
//! //assert!(interp.eval("(+ 123 657)").is_ok());
//! //assert!(interp.call(0).is_ok());
//! //assert!(interp.pop().unwrap() == 123 + 657);
//!
//! // Now let's try an error:
//! //assert!(interp.eval("(+ 1 5").is_err());
//! ```

use interp;
use value;
use alloc;

pub struct State(interp::State);


// Unsafe because the return value is not rooted
pub unsafe trait SchemeValue: Sized {
    fn to_value(&self, heap: &mut alloc::Heap) -> value::Value;
    fn of_value(val: &value::Value) -> Result<Self, String>;
}

unsafe impl SchemeValue for usize {
    fn to_value(&self, _: &mut alloc::Heap) -> value::Value {
        if self & 3 << (size_of!(usize) * 8 - 2) != 0 {
            panic!("bignums not yet supported")
        } else {
            value::Value::new(self << 2)
        }
    }
    fn of_value(val: &value::Value) -> Result<Self, String> {
        val.as_fixnum().map_err(|x|x.to_owned())
    }
}

impl State {
    pub fn new() -> Self {
        State(interp::new())
    }

    pub fn execute_bytecode(&mut self) -> Result<(), String> {
        interp::interpret_bytecode(&mut self.0)
    }

    pub fn push<T: SchemeValue>(&mut self, value: T) -> Result<(),()> {
        let new_val = value.to_value(&mut self.0.heap);
        Ok(self.0.heap.stack.push(new_val))
    }

    pub fn pop<T: SchemeValue>(&mut self) -> Result<T, String> {
        let x = self.0.heap.stack.pop();
        match x {
            Some(v) => T::of_value(&v),
            None => Err("Attempt to pop from empty stack".to_owned()),
        }
    }

    pub fn cons(&mut self) -> Result<(), String> {
        let len = self.0.heap.stack.len();
        Ok(self.0.heap.alloc_pair(len - 2, len - 1))
    }

    pub fn car(&mut self) -> Result<value::Value, String> {
        let len = self.0.heap.stack.len();
        self.0.heap.stack[len - 1]
            .car()
            .map_err(|()|
                     "Attempt to take the car of a \
                      non-pair".to_owned())
    }
    pub fn cdr(&mut self) -> Result<value::Value, String> {
        let len = self.0.heap.stack.len();
        self.0.heap.stack[len - 1]
            .cdr()
            .map_err(|()|
                     "Attempt to take the cdr of a \
                      non-pair".to_owned())
    }
    pub fn intern(_object: &str) -> Result<(), String> {
        unimplemented!()
    }
}
/*
Opcode::SetCdr => {
    try!(heap.stack[dst]
         .set_cdr(heap.stack[src].clone())
         .map_err(|()|
                  "Attempt to set the cdr of a \
                   non-pair".to_owned()));
    *pc += 1;
}
Opcode::Set => {
    heap.stack[dst] = heap.stack[src].clone();
    *pc += 1;
}
Opcode::Add => {
    // The hot paths are fixnums and flonums.  They are inlined.
    // Most scripts probably do not heavily use complex numbers.
    // Bignums or rationals will always be slow.
    let (fst, snd) = (heap.stack[src].get(), heap.stack[src2].get());
    heap.stack.push(if fst & snd & 3 == 0 {
        let res = fst.wrapping_add(snd);
        if res < fst {
            // Overflow
            value::Value::new(res) // TODO: implement bignums
        } else {
            value::Value::new(res)
        }
    } else {
        return Err("wrong type to add".to_owned())
    });
    *pc += 1;
}

Opcode::Subtract => {
    let (fst, snd) = (heap.stack[src].clone(), heap.stack[src2].clone());
    // See above.
    heap.stack[dst] =
        try!(arith::subtract(heap, &fst, &snd));
    *pc += 1;
}

Opcode::Multiply => {
    // See above.
    let (fst, snd) = (heap.stack[src].clone(), heap.stack[src2].clone());
    heap.stack[dst] = try!(arith::multiply(heap, &fst, &snd));
    *pc += 1;
}

Opcode::Divide => {
    // See above.
    let (fst, snd) = (heap.stack[src].clone(), heap.stack[src2].clone());
    heap.stack[dst] = try!(arith::divide(heap, &fst, &snd));
    *pc += 1;
}

Opcode::Power => {
    // See above.
    let (fst, snd) = (heap.stack[src].clone(), heap.stack[src2].clone());
    heap.stack[dst] = arith::exponential(fst, snd);
    *pc += 1;
}

Opcode::MakeArray => {
    let _value = alloc::Heap::alloc_vector(heap, &[]);
    *pc += 1;
}

Opcode::SetArray => {
    let index = try!(heap.stack[src]
                     .as_fixnum());
    try!(heap.stack[dst]
         .array_set(index, &heap.stack[src2]));
    *pc += 1;
}

Opcode::GetArray => {
    let index = try!(heap.stack[src]
                     .as_fixnum());
    heap.stack[dst] = try!(heap.stack[src2]
                           .array_get(index)
                           .map(|ptr| unsafe { (*ptr).clone() }));
    *pc += 1;
}

Opcode::LoadFalse => {
    heap.stack.push(value::Value::new(value::FALSE));
}

Opcode::LoadTrue => {
    heap.stack.push(value::Value::new(value::TRUE));
}

Opcode::LoadNil =>
    heap.stack.push(value::Value::new(value::NIL)),

Opcode::LoadGlobal => {
    match heap.stack.pop().map(|x|x.kind()) {
        Some(Kind::Symbol(ptr)) =>
            heap.stack.push((unsafe { &*ptr }).value.clone()),
        _ => return Err("Attempt to get the value of a non-symbol".to_owned()),
    }
    *pc += 1;
}

Opcode::StoreGlobal => {
    match heap.stack.pop().map(|x|x.kind()) {
        Some(Kind::Symbol(ptr)) =>
            (unsafe { &mut *ptr }).value = heap.stack.pop().unwrap(),
        _ => return Err("Attempt to get the value of a non-symbol".to_owned()),
    }
    *pc += 1;
}
*/

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn push_and_pop_fixnum() {
        let mut interp = State::new();
        let _ = interp.push(127);
        let x: Result<usize, _> = interp.pop();
        assert_eq!(x.unwrap(), 127)
    }
}
