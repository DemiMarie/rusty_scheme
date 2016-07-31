//! The interpreter for RustyScheme.
//!
//! This is the part of RustyScheme that actually executes RustyScheme bytecode.
//! It is a simple `match`-based interpreter.  Future optimizations include using
//! tail calls to implement the equivalent of computed gotos.
//!
//! The entry point is in `self::interpret_bytecode`.  Upon entering this
//! function (ex. from a Rust API call), the called function must be at the
//! bottom of the stack, followed by the arguments.
//!
//! Upon a Scheme->Scheme function call, the data stack layout is:
//!
//! |--------------------|
//! | arguments          |
//! |--------------------|
//! | caller function    |
//! |--------------------|
//!
//! and the control stack layout is:
//!
//! |--------------------|
//! | return address     |
//! |--------------------|
//! | old frame pointer  |
//! |--------------------|
//! | captured?          |
//! |--------------------|
//!
//! but these three objects are all held in a single Rust struct.
//!
//! `const STACK_OFFSET: usize` holds the difference between the old stack
//! pointer and the new frame pointer. `captured?` holds whether the Scheme
//! environment has been captured.

use std::ptr;
use value;
use alloc;
use arith;

use bytecode::{Bytecode, Opcode};

const STACK_OFFSET: usize = 1;

pub struct ActivationRecord {
    return_address: usize,
    frame_pointer: usize,
    captured: bool,
}

/// The Scheme state.  It has several parts:
///
/// - the program counter (`program_counter`), which stores the current
///   bytecode instruction position.
/// - the stack pointer `sp`, which stores the current stack position.
/// - the control stack `control_stack`, which stores control flow
///   information.
/// - The environment pointer `env`, which (if non-NULL) points to the current
///   environment.
/// - the bytecode `bytecode`, which stores the bytecode currently being
///   executed.
pub struct State {
    program_counter: usize,
    sp: usize,
    control_stack: Vec<ActivationRecord>,
    bytecode: Vec<Bytecode>,
    pub heap: alloc::Heap,
}

/// Create a new Scheme interpreter
pub fn new() -> self::State {
    State {
        program_counter: 0,
        sp: 0,
        control_stack: vec![],
        heap: alloc::Heap::new(1 << 16),
        bytecode: vec![],
    }
}

/// This function interprets the Scheme bytecode.
pub fn interpret_bytecode(s: &mut State) -> Result<(), String> {
    use value::Kind;
    let pc = &mut s.program_counter;
    let heap = &mut s.heap;
    heap.environment = ptr::null_mut();
    let sp = &mut s.sp;
    let mut fp = 0;
    'main: loop {
        let Bytecode { opcode, src, src2, dst } = s.bytecode[*pc];
        let (src, src2, dst): (usize, usize, usize) = (src.into(), src2.into(), dst.into());
        // let len = heap.stack.len();
        match opcode {
            Opcode::Cons => {
                heap.alloc_pair(src, src2);
                heap.stack[dst] = heap.stack.pop().unwrap();
                *pc += 1;
            }
            Opcode::Car => {
                heap.stack[dst] = try!(heap.stack[src]
                                           .car()
                                           .map_err(|()| {
                                               "Attempt to take the \
                                                car of a non-pair"
                                                   .to_owned()
                                           }));
                *pc += 1;
            }
            Opcode::Cdr => {
                heap.stack[dst] = try!(heap.stack[src]
                                           .cdr()
                                           .map_err(|()| {
                                               "Attempt to take the \
                                                cdr of a non-pair"
                                                   .to_owned()
                                           }));
                *pc += 1;
            }
            Opcode::SetCar => {
                try!(heap.stack[dst]
                         .set_car(heap.stack[src].clone())
                         .map_err(|()| "Attempt to set the car of a non-pair".to_owned()));
                *pc += 1;
            }
            Opcode::SetCdr => {
                try!(heap.stack[dst]
                         .set_cdr(heap.stack[src].clone())
                         .map_err(|()| "Attempt to set the cdr of a non-pair".to_owned()));
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
                    return Err("wrong type to add".to_owned());
                });
                *pc += 1;
            }

            Opcode::Subtract => {
                let (fst, snd) = (heap.stack[src].clone(), heap.stack[src2].clone());
                // See above.
                heap.stack[dst] = try!(arith::subtract(heap, &fst, &snd));
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

            Opcode::Closure => {
                heap.alloc_closure(src as u8, src2 as u8, dst);
                let len = heap.stack.len();
                heap.environment = unsafe { heap.stack[len - 1].as_ptr() } as *mut value::Vector;
                *pc += 1;
            }

            Opcode::MakeArray => {
                let _value = alloc::Heap::alloc_vector(heap, src, src2);
                *pc += 1;
            }

            Opcode::SetArray => {
                let index = try!(heap.stack[src].as_fixnum());
                try!(heap.stack[dst].array_set(index, &heap.stack[src2]));
                *pc += 1;
            }

            Opcode::GetArray => {
                let index = try!(heap.stack[src].as_fixnum());
                heap.stack[dst] = try!(heap.stack[src2]
                                           .array_get(index)
                                           .map(|ptr| unsafe { (*ptr).clone() }));
                *pc += 1;
            }

            // Frame layout: activation record below rest of data
            Opcode::Call => {
                let frame_pointer = *sp - src - 1;
                s.control_stack.push(ActivationRecord {
                    return_address: *pc,
                    frame_pointer: frame_pointer,
                    captured: !heap.environment.is_null(),
                });
                *pc = 0;
                *sp = heap.stack.len();
                fp = frame_pointer;
            }

            Opcode::LoadFalse => {
                heap.stack.push(value::Value::new(value::FALSE));
            }

            Opcode::LoadTrue => {
                heap.stack.push(value::Value::new(value::TRUE));
            }

            Opcode::LoadNil => heap.stack.push(value::Value::new(value::NIL)),
            Opcode::TailCall => {
                let (first, rest) = heap.stack.split_at_mut(*sp - src - 1);
                *pc = 0;
                *sp = fp + src + 1;
                first[fp..*sp].clone_from_slice(rest);
            }

            Opcode::Return => {
                if let Some(return_frame) = s.control_stack.pop() {
                    *sp = fp;
                    *pc = return_frame.return_address;
                    fp = return_frame.frame_pointer
                } else {
                    return Ok(());
                }
            }

            Opcode::LoadEnvironment => {
                let to_be_pushed = if heap.environment.is_null() {
                    heap.stack[src + fp].clone()
                } else {
                    unsafe {
                        (*value::Value::raw_array_get(heap.environment as *const _, src).unwrap()).clone()
                    }
                };
                heap.stack.push(to_be_pushed.clone());
                *pc += 1;
            }

            Opcode::LoadConstant => {
                let x = unsafe {
                    (*value::Value::raw_array_get(heap.constants, src).unwrap()).clone()
                };
                heap.stack.push(x);
                *pc += 1;
            }

            Opcode::LoadArgument => {
                let x = heap.stack[fp + src].clone();
                heap.stack.push(x);
                *pc += 1;
            }

            Opcode::StoreArgument => {
                let x = heap.stack.pop().unwrap();
                heap.stack[fp + src] = x;
                *pc += 1;
            }

            Opcode::StoreEnvironment => {
                let to_be_stored = heap.stack.pop().unwrap();
                if heap.environment.is_null() {
                    heap.stack[src] = to_be_stored
                } else {
                    unsafe {
                        value::Value::raw_array_set(heap.environment, src, to_be_stored).unwrap()
                    }
                }
                *pc += 1;
            }

            Opcode::LoadGlobal => {
                match heap.stack.pop().map(|x| x.kind()) {
                    Some(Kind::Symbol(ptr)) => heap.stack.push((unsafe { &*ptr }).value.clone()),
                    _ => return Err("Attempt to get the value of a non-symbol".to_owned()),
                }
                *pc += 1;
            }

            Opcode::StoreGlobal => {
                match heap.stack.pop().map(|x| x.kind()) {
                    Some(Kind::Symbol(ptr)) => {
                        (unsafe { &mut *ptr }).value = heap.stack.pop().unwrap()
                    }
                    _ => return Err("Attempt to get the value of a non-symbol".to_owned()),
                }
                *pc += 1;
            }
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use value::Value;
    use std::cell::Cell;
    use bytecode::{Opcode, Bytecode};
    #[test]
    fn can_cons() {
        let mut bco = super::new();
        bco.heap.stack.push(Value { contents: Cell::new(0) });
        bco.heap.stack.push(Value { contents: Cell::new(0) });
        assert!(bco.heap.stack.len() == 2);
        bco.bytecode.push(Bytecode {
            opcode: Opcode::Cons,
            src: 0,
            src2: 1,
            dst: 1,
        });
        bco.bytecode.push(Bytecode {
            opcode: Opcode::Return,
            src: 0,
            src2: 0,
            dst: 0,
        });
        assert!(super::interpret_bytecode(&mut bco).is_ok());
    }
}
