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
use value::Instruction;
pub struct State {
    pc: usize,
    sp: usize,
    registers: Vec<Instruction>,
    heap: alloc::Heap,
    bytecode: value::BCO,
    //symbol_table: HashMap<_,_>,
}

fn interpret_bytecode<A: Allocator, H, S, L, H_>(s: &mut State) {
    use value::EnumValue;
    let pc = &mut s.pc;
    let heap = &mut s.heap;
    let stack: &mut alloc::Stack = &mut heap.stack;
    let sp = &mut s.sp;
    'main: loop {
        let inst = s.bytecode.contents[*sp];
        match inst.opcode {
            Cons => {
                stack[inst.dst.into()] =
                    heap.alloc_pair(stack[inst.src.into()],
                                    stack[inst.src2.into()]);
                Ok(())
            }
            Car => {
                let src: value::Pair = stack[inst.src.into()].check_cons();
                stack[inst.dst.into()] = src.car.get();
                Ok(())
            }
            Cdr => {
                let src = stack[inst.src.into()].check_cons();
                stack[inst.dst.into()] = src.cdr;
                Ok(())
            }
            SetCar => {
                let src = stack[inst.dst.into()].check_cons();
                src.set_car(stack[inst.src.into()]);
                Ok(())
            }
            SetCdr => {
                let src = stack[inst.dst.into()].check_cons();
                src.set_cdr(stack[inst.src.into()]);
                *pc += 2;
                Ok(())
            }
            Set => {
                // NOBARRIER: the stack is a GC root
                stack[inst.dst.into()] = stack[inst.src.into()];
                *pc += 2;
                Ok(())
            }
            Cdr => {
                let src = stack[inst.src.into()].check_cons();
                stack[inst.dst.into()] = src.cdr;
                Ok(())
            }
            SetCar => {
                let src = stack[inst.dst.into()].check_cons();
                // Includes the write barrier
                src.set_car(stack[inst.src.into()]);
                Ok(())
            }
            SetCdr => {
                if let EnumValue::Pair(pair) =
                    stack[inst.dst.into()].enum_type() {
                    // Includes the write barrier
                    unsafe { (*pair).cdr.set(stack[inst.src.into()]) };
                    Ok(())
                } else {
                    Err(())//"Attempt to pass non-cons to set-cdr!")
                }
            }
            Set => {
                // NOBARRIER: the stack is a GC root
                stack[inst.dst.into()] = stack[inst.src.into()];
                Ok(())
            }
            Add => {
                // The hot paths are fixnums and flonums.  They are in an inlined
                // function.
                // Most scripts probably do not heavily use complex numbers.
                // Bignums or rationals will always be slow.
                stack[inst.dst.into()] = stack[inst.src.into()] +
                    stack[inst.src2.into()];
                Ok(())
            }
            Subtract => {
                // See above.
                stack[inst.dst.into()] = stack[inst.src.into()] -
                    stack[inst.src2.into()];
                Ok(())
            }
            Multiply => {
                // See above.
                stack[inst.dst.into()] = stack[inst.src.into()] *
                    stack[inst.src2.into()];
                Ok(())
            }
            Divide => {
                // See above.
                stack[inst.dst.into()] = stack[inst.src.into()] /
                    stack[inst.src2.into()];
                Ok(())
            }
            Power => {
                // See above.
                stack[inst.dst.into()] =
                    arith::exponential(stack[inst.src.into()],
                                       stack[inst.src2.into()]);
                Ok(())
            }
            MakeClosure => {
                alloc::Heap::allocate_closure(heap,
                                       inst.get_closure_pointer_or_bug(),
                                       inst.free_variables());
                Ok(())
            }
            MakeArray => A::allocate_vector(..),
            SetArray => {
                try!(stack[inst.dst.into()].as_array())[stack[inst.src2.into()]]
                = stack[inst.src2.into()];
                Ok(())
            }
            GetArray => unimplemented!(),
            Call => unimplemented!(),
            Constant => unimplemented!(),
            TailCall => {
                sp = s.return_address;
                unimplemented!()
            }
        };
    }
}
