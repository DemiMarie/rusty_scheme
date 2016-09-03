//! The public Rust embedding API of `RustyScheme`.  Very unstable.
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

extern crate env_logger;

use interp;
use value;
use alloc;
use arith;
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

unsafe impl SchemeValue for bool {
    fn to_value(&self, _: &mut alloc::Heap) -> value::Value {
        value::Value::new(if *self {
            value::TRUE
        } else {
            value::FALSE
        })
    }
    fn of_value(val: &value::Value) -> Result<Self, String> {
        match val.get() {
            value::TRUE => Ok(true),
            value::FALSE => Ok(false),
            x => Err(format!("Bad bool {:x}", x)),
        }
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}
impl State {
    pub fn new() -> Self {
        State {
            state: interp::new(),
            fp: (-1isize) as usize,
        }
    }

    pub fn execute_bytecode(&mut self) -> Result<(), String> {
        interp::interpret_bytecode(&mut self.state)
    }

    pub fn push<T: SchemeValue>(&mut self, value: T) -> Result<(), ()> {
        let state = &mut self.state;
        let new_val = value.to_value(&mut state.heap);
        Ok(state.heap.stack.push(new_val))
    }

    /// Pops the top of the stack and converts it to a Rust value.
    pub fn pop<T: SchemeValue>(&mut self) -> Result<T, String> {
        let x = self.state.heap.stack.pop();
        match x {
            Some(v) => T::of_value(&v),
            None => Err("Attempt to pop from empty stack".to_owned()),
        }
    }

    /// Pops and discards the top of the stack.
    pub fn drop(&mut self) -> Result<(), String> {
        match self.state.heap.stack.pop() {
            Some(_) => Ok(()),
            None => Err("Attempt to pop from empty stack".to_owned()),
        }
    }

    /// Pushes the cons of the top two values on the stack.
    pub fn cons(&mut self) -> Result<(), String> {
        let len = self.state.heap.stack.len();
        debug_assert!(len > 1);
        self.state.heap.alloc_pair(len - 2, len - 1);
        Ok(())
    }

    /// Creates a list whose elements are the top `arg - 1` elements of the
    /// stack.  The top of the stack becomes the `cdr` of the last pair.
    pub fn list_with_tail(&mut self, arg: usize) -> Result<(), String> {
        let len = self.len();
        if arg > len - 1 {
            return Err("Attempt to make a list longer than the stack \
                        is deep".to_owned())
        }
        //error!("Stack depth: {:?}, args: {:?}", self.len(), arg);
        for _ in 0..(arg) {
            let q = self.len();
            try!(self.cons());
            self.store(2, 0);
            self.state.heap.stack.pop();
            self.state.heap.stack.pop();
            debug_assert_eq!(q, self.len() + 1)
        }
        debug_assert_eq!(len, self.len() + arg);
        //error!("Stack depth: {:?}", self.len());
        Ok(())
    }

    pub fn list(&mut self, arg: usize) -> Result<(), String> {
        self.push_nil();
        self.list_with_tail(arg)
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
    pub fn intern(&mut self, object: &str) -> Result<(), String> {
        Ok(self.state.heap.intern(object))
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
            value::Value::new(fst.wrapping_add(snd)) // TODO: bignums
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
        arith::multiply(heap, &fst, &snd).map(|_| ())
    }

    pub fn divide(&mut self, src: usize, src2: usize, dst: usize) -> Result<(), String> {
        // See above.
        let fp = self.fp;
        let heap = &mut self.state.heap;
        let (fst, snd) = (heap.stack[src - fp].clone(), heap.stack[src2 - fp].clone());
        heap.stack[dst - fp] = try!(arith::divide(heap, &fst, &snd));
        Ok(())
    }

    pub fn exponential(&mut self, src: usize, src2: usize, _dst: usize) -> Result<(), String> {
        // See above.
        let fp = self.fp;
        let heap = &mut self.state.heap;
        let (fst, snd) = (heap.stack[src - fp].clone(), heap.stack[src2 - fp].clone());
        heap.stack[_dst - fp] = arith::exponential(fst, snd);
    }

    pub fn vector(&mut self, src: usize, src2: usize) -> Result<(), String> {
        debug_assert!(src2 >= src);
        Ok(alloc::Heap::alloc_vector(&mut self.state.heap, src, src2))
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
        self.state.heap.load_global()
    }

    pub fn load(&mut self, src: usize) {
        let stack = &mut self.state.heap.stack;
        let val = stack[stack.len() - src - 1].clone();
        stack.push(val);
    }

    pub fn len(&self) -> usize {
        self.state.heap.stack.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn store(&mut self, src: usize, dst: usize) {
        let stack = &mut self.state.heap.stack;
        let len = stack.len();
        stack[len - dst - 1] = stack[len - src - 1].clone();
    }

    pub fn store_global(&mut self) -> Result<(), String> {
        self.state.heap.store_global()
    }
    pub fn gc(&mut self) {
        alloc::collect(&mut self.state.heap)
    }
}

#[cfg(test)]
mod tests {
    extern crate env_logger;
    use super::*;
    #[test]
    fn push_and_pop_fixnum() {
        let mut interp = State::new();
        let _ = interp.push(127);
        let x: Result<usize, _> = interp.pop();
        assert_eq!(x.unwrap(), 127)
    }

    #[test]
    fn intern_many_strings() {
        let _ = env_logger::init();
        let mut interp = State::new();
        let x = "Test string!".to_owned();
        for _ in &[0..1000] {
            interp.push(x.clone()).unwrap();
        }
        for _ in &[0..1000] {
            assert_eq!(interp.pop(), Ok(x.clone()))
        }
    }
    #[test]
    fn intern_many_symbols() {
        let _ = env_logger::init();
        let mut interp = State::new();
        interp.push_false();
        interp.gc();
        for i in 0..100 {
            assert_eq!(interp.state.heap.stack.len(), 1);
            assert_eq!(interp.state.heap.symbol_table.contents.len(), i);
            let _ = interp.intern(&format!("Falcon {}", i));
            assert_eq!(interp.state.heap.stack.len(), 2);
            interp.load(1);// fresh symbol
            assert_eq!(interp.state.heap.stack.len(), 3);
            interp.load(1);// old symbol
                assert_eq!(interp.state.heap.stack.len(), 4);
            interp.store_global().unwrap(); // stores old symbol into fresh symbol
            assert_eq!(interp.state.heap.stack.len(), 2);
            interp.store(0, 1);
            assert_eq!(interp.state.heap.stack.len(), 2);
            let x: Result<usize, _> = interp.pop();
            assert!(x.is_err());
            assert_eq!(interp.state.heap.stack.len(), 1);
        }
        assert_eq!(interp.state.heap.symbol_table.contents.len(), 100);
        let x: Result<usize, _> = interp.pop();
        assert!(x.is_err());
        interp.gc();
        assert_eq!(interp.state.heap.symbol_table.contents.len(), 0)
    }
}
