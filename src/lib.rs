#![allow(dead_code)]
#![deny(warnings)]

macro_rules! debug {
    ($($exp:expr),*) => {
        if cfg!(feature = "debug-logging") {
            println!($($exp),*);
        } else {}
    }
}

#[macro_use]
mod value;
mod state;
mod arith;
mod alloc;
mod interp;
//mod read;
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
