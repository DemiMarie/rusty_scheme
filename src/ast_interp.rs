use value;
use alloc;

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

use value::Instruction;

pub struct State {
    pc: usize,
    sp: usize,
    registers: Vec<Instruction>,
    heap: alloc::Heap,
    bytecode: value::BCO,
    //symbol_table: HashMap<_,_>,
}

/// Evaluates one opcode.  Pushes the result on the stack.
///
/// Returns: `Ok(())` if everything went okay, `Err(())` if a Scheme
/// exception was thrown.
pub fn eval(s: &mut State) {
    
}
