use api;

pub fn compile_list(vec: &mut Vec<Opcode>, s: &mut api::State)
                    -> Result<usize, &'static str> {
    try!(s.dup());
    let mut length = 1usize;
    while let Ok(x) = s.car() {
        try!(compile_form(vec, s));
        length += 1;
        if (s.marked()) {
            return Err("circular list not allowed")
        }
        s.mark();
        try!(s.cdr());
        try!(s.dup());
    }
    let retval = if s.null() {
        Ok(length)
    } else {
        Err("dotted list not allowed")
    };
    s.drop();
    retval
}

pub fn compile_form(vec: &mut Vec<Opcode>, s: &mut api::State)
                    -> Result<usize, &'static str> {
    match s.typeof() {
        Typeof::Pair => {
            try!(s.dup());
            try!(s.car());
            try!(s.cdr());
            match 
