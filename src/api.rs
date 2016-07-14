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
use value::Value;

pub struct State(interp::State);


// Unsafe because the return value is not rooted
pub unsafe trait SchemeValue: Sized {
    fn to_value(&self) -> value::Value;
    fn of_value(val: &value::Value) -> Result<Self, String>;
}

unsafe impl SchemeValue for usize {
    fn to_value(&self) -> Value {
        if self & 3 << (size_of!(usize) * 8 - 2) != 0 {
            panic!("bignums not yet supported")
        } else {
            Value::new(self << 2)
        }
    }
    fn of_value(val: &value::Value) -> Result<Self, String> {
        val.as_fixnum().map_err(|x|x.to_owned())
    }
}

#[cfg(none)]
unsafe impl SchemeValue for str {
    fn to_value(&self) -> Value {
        self.len + 0b111 & 0b111
    }
    fn of_value(val: &value::Value) -> Result<Self, String> {
        //val.as_string
        unimplemented!()
    }
}

impl State {
    pub fn new() -> Self {
        State(interp::new())
    }

    pub fn push<T: SchemeValue>(&mut self, value: T) -> Result<(),()> {
        Ok(self.0.heap.stack.push(value.to_value()))
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
}

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
