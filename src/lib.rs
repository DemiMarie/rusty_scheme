#![feature(type_ascription)]
#![feature(static_recursion)]
#![allow(dead_code)]
#![deny(warnings)]

macro_rules! debug {
    ($($exp:expr),*) => {
        if cfg!(debug_assertions) {
            println!($($exp),*);
        } else {}
    }
}

macro_rules! bug {
    ($exp: expr) => {
        panic!(concat!("internal error: ", $exp, "
   This is a bug in RustyScheme.  Please report it \
   at https://github.com/DemiMarie/rusty_scheme/issues"))
    };
    ($exp: expr, $($exps: tt)*) => {
        panic!(concat!("internal error: ", $exp, "
   This is a bug in RustyScheme.  Please report it \
   at https://github.com/DemiMarie/rusty_scheme/issues"), $($exps)*)
    }
}

#[macro_use]
mod value;
mod state;
mod arith;
mod bytecode;
mod alloc;
mod symbol;
mod interp;
//mod read;
mod api;
pub use interp::Opcode;
pub use api::*;
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
