use value;
use std::collections::HashMap;
use alloc;
use arith;
#[repr(u8)]
pub enum Opcode {
    Cons,
    Car,
    Cdr,
    Aref,
    Aset,
    Throw,
    MakeArray,
    Call,
    TailCall,
    Catch,
    Return,
    SetCar,
    SetCdr,
    Set,
    SetField,
    Closure,
    Load,
}

pub struct ActivationRecord {
    return_address: usize,
}
use alloc::Allocator;
struct Instruction {
    opcode: u8,
    src: u8,
    src1: u8,
    dst: u8,
}
pub struct State<'a> {
    pc: usize,
    sp: usize,
    registers: Vec<Instruction>,
    heap: alloc::Heap<'a>,
    bytecode: value::BCO<'a>,
    //symbol_table: HashMap<_,_>,
}

fn interpret_bytecode<'a, A: Allocator<'a>, H, S, L, H_>(s: &'a mut State<'a>) {
    let mut pc = s.pc;
    let ref mut heap = s.heap;
    let mut stack = heap.stack;
    let mut sp = s.sp;
    'main: loop {
        let inst = s.bytecode[sp];
        match inst.opcode {
            Cons => {
                stack[inst.dst] = heap.cons(s, stack[inst.src], stack[inst.src2]);
            }
            Car => {
                let src = stack[inst.src].check_cons();
                stack[inst.dst] = src.car
            }
            Cdr => {
                let src = stack[inst.src].check_cons();
                stack[inst.dst] = src.cdr
            }
            SetCar => {
                let src = stack[inst.dst].check_cons();
                src.set_car(stack[inst.src])
            }
            SetCdr => {
                let src = stack[inst.dst].check_cons();
                src.set_cdr(stack[inst.src]);
                pc += 2;
            }
            Set => {
                // NOBARRIER: the stack is a GC root
                stack[inst.dst] = stack[inst.src];
                pc += 2;
            }
            Cdr => {
                let src = stack[inst.src].check_cons();
                stack[inst.dst] = src.cdr
            }
            SetCar => {
                let src = stack[inst.dst].check_cons();
                // Includes the write barrier
                src.set_car(stack[inst.src])
            }
            SetCdr => {
                let src = stack[inst.dst].check_cons();
                // Includes the write barrier
                src.set_cdr(stack[inst.src])
            }
            Set => {
                // NOBARRIER: the stack is a GC root
                stack[inst.dst] = stack[inst.src]
            }
            Add => {
                // The hot paths are fixnums and flonums.  They are in an inlined
                // function.
                // Most scripts probably do not heavily use complex numbers.
                // Bignums or rationals will always be slow.
                stack[inst.dst] = stack[inst.src] + stack[inst.src2]
            }
            Subtract => {
                // See above.
                stack[inst.dst] = stack[inst.src] - stack[inst.src2]
            }
            Multiply => {
                // See above.
                stack[inst.dst] = stack[inst.src] * stack[inst.src2]
            }
            Divide => {
                // See above.
                stack[inst.dst] = stack[inst.src] / stack[inst.src2]
            }
            Power => {
                // See above.
                stack[inst.dst] = arith::exponential(stack[inst.src], stack[inst.src2])
            }
            MakeClosure => {
                alloc::Heap::allocate_closure(heap,
                                       inst.get_closure_pointer_or_bug(),
                                       inst.free_variables())
            }
            MakeArray => A::allocate_vector(..),
            SetArray => try!(stack[inst.dst].as_array())[stack[inst.src2]] = stack[inst.src1],
            GetArray => unimplemented!(),
            Call => unimplemented!(),
            Constant => unimplemented!(),
            TailCall => {
                sp = s.return_address;
                unimplemented!()
            }
        }
    }
}
