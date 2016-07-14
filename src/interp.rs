use std::ptr;
use value;
use std::mem;
use alloc;
use arith;
#[repr(u8)]
pub enum Opcode {
    /// `cons`
    Cons,

    /// `car`
    Car,

    /// `cdr`
    Cdr,

    /// `set-car!`
    SetCar,

    /// `set-cdr!`
    SetCdr,

    /// Addition
    Add,

    /// Subtraction
    Subtract,

    /// Multiplication
    Multiply,

    /// Division
    Divide,

    /// Exponentiation
    Power,

    /// Create an array
    MakeArray,

    /// Store to an array
    SetArray,

    /// Load from an array
    GetArray,

    /// Function call
    Call,

    /// Tail call
    TailCall,

    /// Return from a function
    Return,

    /// Create a closure
    Closure,

    /// Mutation of stack slots
    Set,

    /// Load from constant vector
    LoadConstant,

    /// Load from environment
    LoadEnvironment,

    /// Load from global
    LoadGlobal,

    /// Store to environment
    StoreEnvironment,

    /// Store to global
    StoreGlobal,
}


pub struct ActivationRecord {
    stack_pointer: usize,
    return_address: usize,
    captured: bool,
}

use value::Instruction;
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
    // registers: Vec<Instruction>,
    pub heap: alloc::Heap,
    bytecode: Vec<Instruction>,
}

/// Unwind the Scheme stack in case of exception.
fn unwind(_stack: &mut alloc::Stack) -> () {
    unimplemented!()
}

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
fn interpret_bytecode(s: &mut State) {
    let pc = &mut s.program_counter;
    let heap = &mut s.heap;
    heap.environment = ptr::null_mut();
    let sp = &mut s.sp;
    let fp = 0;
    'main: loop {
        macro_rules! interp_try {
            ($exp: expr) => {
                {
                    let val: Result<_, _> = $exp;
                    match val {
                        Ok(x) => x,
                        Err(_) => {
                            unwind(&mut heap.stack);
                            break 'main
                        }
                    }
                }
            }
        }
        let Instruction { opcode, src, src2, dst } = s.bytecode[*sp];
        let (src, src2, dst): (usize, usize, usize) = (src.into(), src2.into(), dst.into());
        let opcode = unsafe {
            if opcode <= mem::transmute(Opcode::StoreGlobal) {
                mem::transmute(opcode)
            } else {
                unreachable!()
            }
        };
        // let len = heap.stack.len();
        match opcode {
            Opcode::Cons => {
                heap.alloc_pair(src, src2);
                heap.stack[dst] = heap.stack.pop().unwrap()
            }
            Opcode::Car => heap.stack[dst] = interp_try!(heap.stack[src].car()),
            Opcode::Cdr => heap.stack[dst] = interp_try!(heap.stack[src].cdr()),
            Opcode::SetCar => interp_try!(heap.stack[dst].set_car(heap.stack[src].clone())),
            Opcode::SetCdr => interp_try!(heap.stack[dst].set_cdr(heap.stack[src].clone())),
            Opcode::Set => heap.stack[dst] = heap.stack[src].clone(),
            Opcode::Add => {
                // The hot paths are fixnums and flonums.  They are inlined.
                // Most scripts probably do not heavily use complex numbers.
                // Bignums or rationals will always be slow.
                let (fst, snd) = (heap.stack[src].get(),
                                  heap.stack[src2].get());
                if fst & snd & 3 == 0 {
                    let res = fst.wrapping_add(snd);
                    if res < fst {
                        // Overflow
                        res // TODO: implement bignums
                    } else {
                        res
                    }
                } else {
                    unimplemented!() // TODO handle exceptions
                }
            }
            Opcode::Subtract => {
                let (fst, snd) = (heap.stack[src].clone(), heap.stack[src2].clone());
                // See above.
                heap.stack[dst] = interp_try!(arith::subtract(heap, &fst, &snd))
            }
            Opcode::Multiply => {
                // See above.
                let (fst, snd) = (heap.stack[src].clone(), heap.stack[src2].clone());
                heap.stack[dst] = interp_try!(arith::multiply(heap, &fst, &snd))
            }
            Opcode::Divide => {
                // See above.
                let (fst, snd) = (heap.stack[src].clone(),
                                  heap.stack[src2].clone());
                heap.stack[dst] = interp_try!(arith::divide(heap, &fst, &snd))
            }
            Opcode::Power => {
                // See above.
                let (fst, snd) = (heap.stack[src].clone(), heap.stack[src2].clone());
                heap.stack[dst] = arith::exponential(fst, snd)
            }
            Opcode::Closure => {
                heap.alloc_closure(src as u8, src2 as u8, dst);
                let len = heap.stack.len();
                heap.environment = Ptr_Val!(heap.stack[len - 1]) as *mut value::Vector
            }
            Opcode::MakeArray => {
                let _value = alloc::Heap::alloc_vector(heap, &[]);
            }
            Opcode::SetArray => {
                let index = interp_try!(heap.stack[src].as_fixnum().map_err(|_| ()));
                interp_try!(heap.stack[dst].array_set(index, &heap.stack[src2]))
            }
            Opcode::GetArray => {
                let index = interp_try!(heap.stack[src]
                                            .as_fixnum()
                                            .map_err(|_| ()));
                heap.stack[dst] = interp_try!(heap.stack[src2]
                                                  .array_get(index)
                                                  .map(|ptr| unsafe { (*ptr).clone() }))
            }

            // Frame layout: activation record below rest of data
            Opcode::Call => {
                // Type check: called function must be integer.
                s.control_stack.push(ActivationRecord {
                    return_address: *pc,
                    stack_pointer: fp,
                    captured: !heap.environment.is_null(),
                });
                *sp = heap.stack.len();

            }

            Opcode::TailCall => {
                let last = interp_try!(s.control_stack.pop().ok_or("\
                    control stack underflow"));
                let (first, rest) = heap.stack.split_at_mut(*sp);
                *pc = last.return_address + 1;
                first[last.stack_pointer + 1..last.stack_pointer + src + 2].clone_from_slice(rest);
                *sp = last.stack_pointer
            }

            Opcode::Return => {
                let return_frame = s.control_stack.pop().unwrap();
                *sp = return_frame.stack_pointer;
                *pc = return_frame.return_address
            }
            Opcode::LoadEnvironment => {
                let to_be_pushed =
                    (if heap.environment.is_null() {
                        &heap.stack[src]
                    } else {
                        unsafe {
                            &*value::Value::raw_array_get(heap.environment as *const _, src)
                                  .unwrap()
                        }
                    })
                    .clone();
                heap.stack.push(to_be_pushed.clone())
            }
            Opcode::LoadConstant => {
                let x = unsafe {
                    (*value::Value::raw_array_get(heap.constants, src).unwrap()).clone()
                };
                heap.stack.push(x)
            }
            Opcode::StoreEnvironment => {
                let to_be_stored = heap.stack.pop().unwrap();
                if heap.environment.is_null() {
                    heap.stack[src] = to_be_stored
                } else {
                    unsafe {
                        value::Value::raw_array_set(heap.environment, src, to_be_stored).unwrap()
                    }
                };
            }
            Opcode::LoadGlobal => {}
            Opcode::StoreGlobal => unimplemented!(),
        }
    }
}

#[cfg(none)]
#[cfg(test)]
mod tests {
    use value::{Value, Instruction};
    use std::cell::Cell;
    #[test]
    fn can_cons() {
        let mut bco = super::new();
        bco.heap.stack.push(Value { contents: Cell::new(0) });
        bco.heap.stack.push(Value { contents: Cell::new(0) });
        assert!(bco.heap.stack.len() == 2);
        bco.bytecode.push(Instruction {
            opcode: 0,
            src: 0,
            src2: 1,
            dst: 1,
        });
        super::interpret_bytecode(&mut bco);
    }
}
