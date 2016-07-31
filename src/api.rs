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
use arith;
use value::Kind;
pub struct State {
    state: interp::State,
    fp: usize,
}


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
        val.as_fixnum().map_err(|x| x.to_owned())
    }
}

impl State {
    pub fn new() -> Self {
        State { state: interp::new(), fp: (- 1isize) as usize }
    }

    pub fn execute_bytecode(&mut self) -> Result<(), String> {
        interp::interpret_bytecode(&mut self.state)
    }

    pub fn push<T: SchemeValue>(&mut self, value: T) -> Result<(), ()> {
        let state = &mut self.state;
        let new_val = value.to_value(&mut state.heap);
        Ok(state.heap.stack.push(new_val))
    }

    pub fn pop<T: SchemeValue>(&mut self) -> Result<T, String> {
        let x = self.state.heap.stack.pop();
        match x {
            Some(v) => T::of_value(&v),
            None => Err("Attempt to pop from empty stack".to_owned()),
        }
    }

    pub fn cons(&mut self) -> Result<(), String> {
        let len = self.state.heap.stack.len();
        Ok(self.state.heap.alloc_pair(len - 2, len - 1))
    }

    pub fn car(&mut self) -> Result<value::Value, String> {
        let len = self.state.heap.stack.len();
        self.state.heap.stack[len - 1]
            .car()
            .map_err(|()| "Attempt to take the car of a non-pair".to_owned())
    }
    pub fn cdr(&mut self) -> Result<(), String> {
        let len = self.state.heap.stack.len();
        let new_val = try!(self.state.heap.stack[len - 1]
                               .cdr()
                               .map_err(|()| "Attempt to take the cdr of a non-pair".to_owned()));
        Ok(self.state.heap.stack[len - 1] = new_val)
    }
    pub fn intern(_object: &str) -> Result<(), String> {
        unimplemented!()
    }

    pub fn set(&mut self, src: usize, dst: usize) -> () {
        let heap = &mut self.state.heap;
        let fp = self.fp;
        heap.stack[dst - fp] = heap.stack[src - fp].clone();
    }
    pub fn add(&mut self, src: usize, src2: usize) -> Result<(), ()> {
        let fp = self.fp;
        let heap = &mut self.state.heap;
        // The hot paths are fixnums and flonums.  They are inlined.
        // Most scripts probably do not heavily use complex numbers.
        // Bignums or rationals will always be slow.
        let (fst, snd) = (heap.stack[src - fp].get(), heap.stack[src2 - fp].get());
        heap.stack.push(if fst & snd & 3 == 0 {
            let res = fst.wrapping_add(snd);
            if res < fst {
                // Overflow
                value::Value::new(res) // TODO: implement bignums
            } else {
                value::Value::new(res)
            }
        } else {
            return Err(());
        });
        Ok(())
    }

    pub fn subtract(&mut self, src: usize, src2: usize) -> Result<(), String> {
        let fp = self.fp;
        let heap = &mut self.state.heap;
        let (fst, snd) = (heap.stack[src - fp].clone(), heap.stack[src2 - fp].clone());
        // See above.
        let to_be_pushed = try!(arith::subtract(heap, &fst, &snd));
        Ok(heap.stack.push(to_be_pushed))
    }

    pub fn multiply(&mut self, src: usize, src2: usize) -> Result<(), String> {
        // See above.
        let fp = self.fp;
        let heap = &mut self.state.heap;
        let (fst, snd) = (heap.stack[src - fp].clone(), heap.stack[src2 - fp].clone());
        arith::multiply(heap, &fst, &snd).map(|_|())
    }

    pub fn divide(&mut self, src: usize, src2: usize, dst: usize) -> Result<(), String> {
        // See above.
        let fp = self.fp;
        let heap = &mut self.state.heap;
        let (fst, snd) = (heap.stack[src - fp].clone(), heap.stack[src2 - fp].clone());
        heap.stack[dst - fp] = try!(arith::divide(heap, &fst, &snd));
        Ok(())
    }

    pub fn exponential(&mut self, src: usize, src2: usize,
                       _dst: usize) -> Result<(), String> {
        // See above.
        let fp = self.fp;
        let heap = &mut self.state.heap;
        let (fst, snd) = (heap.stack[src - fp].clone(), heap.stack[src2 - fp].clone());
        heap.stack[_dst - fp] = arith::exponential(fst, snd);
    }

    pub fn vector(&mut self, src: usize, src2: usize) -> Result<(), String> {
        let heap = &mut self.state.heap;
        let _value = alloc::Heap::alloc_vector(heap, src, src2);
        Ok(())
    }

    pub fn array_set(&mut self, index: usize, src: usize, dst: usize) -> Result<(), String> {
        let fp = self.fp;
        let heap = &mut self.state.heap;
        heap.stack[dst - fp].array_set(index, &heap.stack[src])
    }

    pub fn array_get(&mut self, index: usize, src: usize, dst: usize) -> Result<(), String> {
        let fp = self.fp;
        let heap = &mut self.state.heap;
        heap.stack[dst + fp] = try!(heap.stack[src + fp]
                                        .array_get(index)
                                        .map(|ptr| unsafe { (*ptr).clone() }));
        Ok(())
    }

    pub fn push_false(&mut self) {
        self.state.heap.stack.push(value::Value::new(value::FALSE));
    }

    pub fn push_true(&mut self) {
        let heap = &mut self.state.heap;
        heap.stack.push(value::Value::new(value::TRUE));
    }

    pub fn push_nil(&mut self) {
        let heap = &mut self.state.heap;
        heap.stack.push(value::Value::new(value::NIL))
    }

    pub fn load_global(&mut self) -> Result<(), String> {
        let heap = &mut self.state.heap;
        match heap.stack.pop().map(|x| x.kind()) {
            Some(Kind::Symbol(ptr)) => Ok(heap.stack.push((unsafe { &*ptr }).value.clone())),
            _ => Err("Attempt to get the value of a non-symbol".to_owned()),
        }
    }

    pub fn store_global(&mut self) -> Result<(), String> {
        let heap = &mut self.state.heap;
        match heap.stack.pop().map(|x| x.kind()) {
            Some(Kind::Symbol(ptr)) =>
                Ok((unsafe { &mut *ptr }).value = heap.stack.pop().unwrap()),
            _ => return Err("Attempt to get the value of a non-symbol".to_owned()),
        }
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
