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
    Constant,

    /// Load from global
    Load,
}


pub struct ActivationRecord {
    stack_pointer: usize,
    return_address: usize,
}

use alloc::Allocator;
use value::Instruction;
pub struct State {
    program_counter: usize,
    sp: usize,
    control_stack: Vec<ActivationRecord>,
    registers: Vec<Instruction>,
    heap: alloc::Heap,
    old_sp: usize,
    bytecode: value::BCO,
}

fn unwind(_stack: &mut alloc::Stack) -> () {
    unimplemented!()
}

fn interpret_bytecode<A: Allocator, H, S, L, H_>(s: &mut State) {
    let pc = &mut s.program_counter;
    let heap = &mut s.heap;
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
        let Instruction { opcode, src, src2, dst } = s.bytecode.contents[*sp];
        let (src, src2, dst): (usize, usize, usize) = (src.into(), src2.into(), dst.into());
        let opcode = unsafe {
            if opcode <= mem::transmute(Opcode::Load) {
                mem::transmute(opcode)
            } else {
                unreachable!()
            }
        };
        match opcode {
            Opcode::Cons => {
                let (fst, snd) = (heap.stack[src].clone(),
                                  heap.stack[src2].clone());
                heap.alloc_pair(fst, snd);
                heap.stack[dst] = heap.stack.pop().unwrap()
            }
            Opcode::Car => heap.stack[dst] = interp_try!(heap.stack[src].car()),
            Opcode::Cdr => heap.stack[dst] = interp_try!(heap.stack[src].cdr()),
            Opcode::SetCar =>
                interp_try!(heap.stack[dst].set_car(heap.stack[src].clone())),
            Opcode::SetCdr =>
                interp_try!(heap.stack[dst].set_cdr(heap.stack[src].clone())),
            Opcode::Set => heap.stack[dst] = heap.stack[src].clone(),
            Opcode::Add => {
                // The hot paths are fixnums and flonums.  They are in an inlined
                // function.
                // Most scripts probably do not heavily use complex numbers.
                // Bignums or rationals will always be slow.
                let (fst, snd) = (heap.stack[src].clone(),
                                  heap.stack[src2].clone());
                heap.stack[dst] = interp_try!(arith::add(heap, &fst, &snd))
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
                let (fst, snd) = (heap.stack[src].clone(), heap.stack[src2].clone());
                heap.stack[dst] = interp_try!(arith::divide(heap, &fst, &snd))
            }
            Opcode::Power => {
                // See above.
                let (fst, snd) = (heap.stack[src].clone(), heap.stack[src2].clone());
                heap.stack[dst] = arith::exponential(fst, snd)
            }
            Opcode::Closure => {
                // alloc::Heap::allocate_closure(heap);
                unimplemented!()
            }
            Opcode::MakeArray => {
                let _value = alloc::Heap::alloc_vector(heap, &[]);
            }
            Opcode::SetArray => {
                let index = interp_try!(heap.stack[src].as_fixnum().map_err(|_| ()));
                interp_try!(heap.stack[dst].array_set(index,
                                                      heap.stack[src2].clone()))
            }
            Opcode::GetArray => {
                let index = interp_try!(heap.stack[src].as_fixnum().map_err(|_| ()));
                heap.stack[dst] = interp_try!(heap.stack[src2]
                                                  .array_get(index)
                                                  .map(|ptr| unsafe { (*ptr).clone() }))
            }
            // Frame layout: activation record below rest of data
            Opcode::Call => {
                s.control_stack.push(ActivationRecord {
                    return_address: *pc,
                    stack_pointer: fp,
                });
                *sp = heap.stack.len()
            }
            Opcode::Constant => unimplemented!(),
            Opcode::TailCall => {
                let last = interp_try!(s.control_stack.pop().ok_or("\
                    control stack underflow"));
                let (first, rest) = heap.stack.split_at_mut(*sp);
                *pc = last.return_address + 1;
                first[last.stack_pointer +
                      1..last.stack_pointer + src +
                      2].clone_from_slice(rest);
                *sp = last.stack_pointer
            }
            Opcode::Return => unimplemented!(),
            Opcode::Load => unimplemented!(),
        };
    }
}
