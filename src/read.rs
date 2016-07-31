use std::io;
use std::io::prelude::*;
use super::{alloc, value};
#[cfg(none)]
pub fn skip_space<R: Read>(x: R) -> Option<u8> {
    for i in x.bytes() {
        if libc::isspace(i) {
            return Some(Ok(i))
        }
    }
    return None
}
use interp;

pub enum ReadError {
    EOFInList,
    EOFInVector,
    MissingCloseParen,
    EOFInString,
    EOFInSymbol,
    EOFAfterSharpBackslash,
    BadSharpMacro([char; 2]),
    UnexpectedCloseParen,
    BadCloseParen,
    BadEscape,
    EOFAfterSharp,
    CharsError(io::CharsError),
}
fn map_utf8<R: Read>(x: &mut io::Chars<R>)
                     -> Result<Option<char>, ReadError> {
    match x.next() {
        Some(Err(err)) => Err(ReadError::CharsError(err)),
        None => Ok(None),
        Some(Ok(c)) => Ok(Some(c)),
    }
}
fn map_escape<R: Read>(state: &mut R) -> ! {
    unimplemented!()
}
pub fn read<R: Read>(state: &mut interp::State, file: &mut R, end_char: char)
                     -> Result<(), ReadError> {
    let chars = file.chars().peekable();
    enum State {
        /// In a list (number of elements so far)
        InList(usize),

        /// End of dotted list (number of elements so far)
        EndDottedList(usize),

        /// In a vector (number of elements so far)
        InVec(usize),

        /// Read-time eval (not yet implemented)
        ReadEval,

        /// `quote`
        Quote,

        /// `quasiquote`
        Quasiquote,

        /// `syntax`
        Syntax,

        /// `unquote`
        Unquote,

        /// `unsyntax`
        Unsyntax,

        /// `quasisyntax`
        Quasisyntax,
    }
    let vec: Vec<State> = vec![];
    let mut depth = 1;
    let ref mut heap = state.heap;
    let mut x = file.chars();
    let run_macros_self_evaluating = |mut value| while let Some(x) = vec.pop()
    {
        match x {
            State::InList(ref x)|State::InVec(ref x) => {
                *x += 1
            }
            State::ReadEval => unimplemented!(),

            State::Quote | State::Unquote | State::Quasiquote |
            State::Syntax | State::Unsyntax | State::Quasisyntax => {
                state.push_nil();
                state.list(2)
            }
        }
    };
    macro_rules! next {
        ($exp: expr) => {
            try!($exp.into().next().map_err(ReadError::CharsError))
        }
    }
    'mainloop: while let Some(mut c) = next!(x) {
        match c {
            '(' => vec.push(State::InList(0)),
            '\'' => {
                heap.intern("quote".to_owned());
                vec.push(State::Quote)
            }
            '#' => match next!(x) {
                None => return Err(ReadError::EOFAfterSharp),
                Some(dispatch_char) => match dispatch_char {
                    '(' => vec.push(State::InVec(0)),
                    '.' => vec.push(State::ReadEval),
                    '\\' => match next!(x) {
                        None => return Err(ReadError::EOFAfterSharpBackslash),
                        Some(c) => unimplemented!()
                    },
                    'f' | 't' => {
                        let is_true = dispatch_char == 't';
                        match x.peek() {
                            Some('(') | Some(')') | None => state.push(is_true),
                            Some(next) if next.is_whitespace() =>
                                vec.push(is_true),
                            Some(q) =>
                                return Err(ReadError::BadSharpMacro([c, q]))
                        }
                    }
                },
            },
            '`' => unimplemented!(),
            ',' => unimplemented!(),
            ')' => { // End of token
                vec.pop().map_or_else(|s| {
                    match s {
                        State::InList(i) => {
                            state.list(i);
                            vec.pop().unwrap();
                        }
                        State::InVec(i) => {
                            state.vector(i);
                            vec.pop().unwrap();
                        }
                    }
                })
            }
            '"' => {
                let string = String::new();
                while let Some(Ok(new_char)) = x.next() {
                    if new_char == '\\' {
                        string.push(try!(map_escape(try!(x.next().map_err()))))
                    }
                    else if new_char == '"' {
                        break
                    } else {
                        string.push(new_char)
                    }
                }
                run_macros_self_evaluating()
            }
        }
    }
    Ok(())
}
