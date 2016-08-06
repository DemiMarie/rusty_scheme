use std::io;
use std::io::prelude::*;
use std::char;
use super::{alloc, value};

pub enum ReadError {
    EOFInList,
    EOFInVector,
    MissingCloseParen,
    IoError(io::Error),
    EOFInString,
    EOFInSymbol,
    EOFAfterSharpBackslash,

    /// Bad sharpsign read macro
    BadSharpMacro([char; 2]),

    /// Unexpected close parentheses
    UnexpectedCloseParen,

    /// Wrong close parentheses
    BadCloseParen,

    /// Bad backslash escape
    BadEscape,

    /// EOF after sharpsign
    EOFAfterSharp,

    /// Stream not valid UTF-8.  The argument is
    /// the partial sequence accumulated.
    InvalidUtf8(u32),
}

/// An event that can be emitted by the reader or tree-walker, and which
/// is part of the stream that is consumed by the tree-builder, printer,
/// and bytecode compiler.
pub enum Event {
    /// A string
    Str(String),

    /// A symbol
    Symbol(String),

    /// Boolean true `#t`
    True,

    /// Boolean false `#f`
    False,

    /// Character `#\\x`
    Char(char),

    /// Integer `12311324`
    Int(usize),

    /// Floating-point numbers (not yet implemented)
    Float(f64),

    /// Start of a list `(` (false) or `[` (true)
    StartList(bool),

    /// Start of a vector `#(`
    StartVec,

    /// End of token `)` (false) or `]` (true)
    EndList(bool),

    /// Read-time eval `#.`
    ReadEval,

    /// Quote `'`
    Quote,

    /// Quasiquote `\``
    Quasiquote,

    /// Unquote `,`
    Unquote,

    /// Unquote splicing `,@`
    UnquoteSplicing,

    /// Syntax `#'`
    Syntax,

    /// Quasisyntax `#\``
    Quasisyntax,

    /// Unsyntax `#,`
    Unsyntax,

    /// Unsyntax splicing #,@
    UnsyntaxSplicing,

    /// End of file
    EOF,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum StringOrSymbol {
    String,
    Symbol,
}

fn char_from_stream_and_byte<R: BufRead>(file: &mut Bytes<R>, unicode_char: u8)
                                         -> Result<char, ReadError> {
    if unicode_char <= 0x7F {
        return Ok(unicode_char as char);
    }
    let len = (!unicode_char).leading_zeros() as u8;
    match len {
        1 | 5...8 => Err(ReadError::InvalidUtf8((unicode_char as u32) << 24)),
        len @ 2...4 => {
            let len = len - 1;
            let mut value: u32 = (unicode_char >> (len + 2)).into();
            value <<= len * 6;
            for (count, current_byte_value) in file.take(len.into()).enumerate()  {
                let current: u32 = try!(current_byte_value.map_err(
                    ReadError::IoError)).into();
                value &= current << (len - count as u8)
            }
            char::from_u32(value).ok_or(ReadError::InvalidUtf8(value))
        }
        _ => unreachable!(),
    }
}
macro_rules! next {
    ($exp: expr, $err: expr) => {
        try!(try!($exp.next().ok_or($err)).map_err(|x|ReadError::IoError(x)))
    }
}

type ReadResult = Result<char, ReadError>;

use std::io::Bytes;
fn handle_unicode_escape<R: BufRead>(file: &mut Bytes<R>) -> ReadResult {
    loop {
        let eof = ReadError::BadEscape;
        let mut escaped_char = 0;
        let next_character = next!(file, eof);
        let subtract_amount = match next_character {
            b'a'...b'f' => 87,
            b'A'...b'F' => 55,
            b'0'...b'9' => 48,
            b';' => return char::from_u32(escaped_char).ok_or(ReadError::BadEscape),
            _ => return Err(ReadError::BadEscape),
        };
        escaped_char <<= 4;
        // No overflow is possible here – later check would
        // detect it.
        escaped_char += next_character as u32 - subtract_amount;
        if escaped_char > 0x10FFFF {
            // protect against overflow
            return Err(ReadError::BadEscape);
        }
    }
}

fn process_escape<R: BufRead>(file: &mut Bytes<R>) -> ReadResult {
    let bad = ReadError::BadEscape;
    loop {
        return Ok(match next!(file, bad) {
            b'n' => '\n',
            b't' => '\t',
            b'e' => '\x1b',
            b'b' => '\x08',
            b'v' => '\x0b',
            b'f' => '\x0c',
            b'\r' => {
                match file.next() {
                    Some(Ok(b'\n')) => continue,
                    Some(Ok(_)) | None => return Err(bad),
                    Some(Err(x)) => return Err(ReadError::IoError(x)),
                }
            }
            b'\n' => continue,
            b'x' | b'u' => try!(handle_unicode_escape(file)),
            l @ b'|' | l @ b'"' | l @ b'\\' | l @ b'#' | l @ b'`' | l @ b',' | l @ b'\'' => {
                l as char
            }
            _ => return Err(bad),
        });
    }
}



pub fn read_escaped<R: BufRead>(file: &mut Bytes<R>, delimiter: StringOrSymbol)
                             -> Result<String, ReadError> {
    use std::char;
    let premature_eof = || {
        match delimiter {
            StringOrSymbol::String => ReadError::EOFInString,
            StringOrSymbol::Symbol => ReadError::EOFInSymbol,
        }
    };

    let buf = String::new();
    loop {
        buf.push(match next!(file, premature_eof()) {
            b'\\' => try!(process_escape(file)),
            b'|' if delimiter == StringOrSymbol::Symbol => break,
            b'"' if delimiter == StringOrSymbol::String => break,
            normal_char if normal_char <= 0x7F => normal_char as char,
            unicode_char => try!(char_from_stream_and_byte(file, unicode_char)),
        })
    }
    Ok(buf)
}
// if delimiter == UndelimitedSymbol && c.is_whitespace() {
// break
// } else {
// c
// }
// }
// let bad = ReadError::BadEscape;
// loop {
// return Ok(match next!(file, bad) {
// b'n' => '\n',
// b't' => '\t',
// b'e' => '\x1b',
// b'b' => '\x08',
// b'v' => '\x0b',
// b'\r' => match next!(file, bad) {
// Some(b'\n') => continue,
// _ => return Err(bad),
// },
// b'\n' => continue,
// b'x' | b'u' => try!(handle_unicode_escape(file)),
// literal@b'|'|b'"'|b'\\'|b'#'|b'`'|b','|b'\'' => literal.into(),
// _ => return Err(bad),
// })
// }
//
pub struct Reader<'a, 'b, T: 'a + BufRead> {
    stream: &'a mut T,
    state: &'b mut interp::State,
}

//    let chars = file.chars().peekable();
// let mut depth = 1;
// let ref mut heap = state.heap;
// let run_macros_self_evaluating = |mut value| while let Some(x) = vec.pop()
// {
// match x {
// State::InList(ref x)|State::InVec(ref x) => {
// x += 1
// }
// State::ReadEval => unimplemented!(),
//
// State::Quote | State::Unquote | State::Quasiquote |
// State::Syntax | State::Unsyntax | State::Quasisyntax => {
// state.push_nil();
// state.list(2)
// }
// }
// };
//



pub fn read<R: BufRead>(file: &mut Bytes<R>) -> Result<Event, ReadError> {
    let handle_splicing = |file: &mut Bytes<R>, nosplice, splice| match file.peek() {
        Some(Ok('@')) => { file.next(); Ok(splice) }
        Some(Ok(_)) | None => Ok(nosplice),
        Some(x) => x,
    };

    loop {
        let next: Option<Result<u8, io::Error>> = file.next();
        if let Some(c) = next {
            return Ok(match try!(c.map_err(ReadError::IoError)) {
                b'(' => Event::StartList(false),
                b'[' => Event::StartList(true),
                b'\'' => Event::Quote,
                b'`' => Event::Quasiquote,
                b',' => try!(handle_splicing(file, Event::Unquote, Event::UnquoteSplicing)),
                b'#' => match next!(file, ReadError::EOFAfterSharp) {
                    b'.' => Event::ReadEval,
                    b'\\' => {
                        let byte = next!(file, ReadError::EOFAfterSharpBackslash);
                        Event::Char(try!(char_from_stream_and_byte(file, byte)))
                    }
                    b't' => Event::True,
                    b'f' => Event::False,
                    b'\'' => Event::Syntax,
                    b'`' => Event::Quasisyntax,
                    b',' => try!(handle_splicing(file, Event::Unsyntax,
                                                 Event::UnsyntaxSplicing)),
                    b'(' => Event::StartVec,
                    dispatch_char =>
                        return Err(ReadError::BadSharpMacro([dispatch_char as char,
                                                             '\0']))
                },
                b')' => Event::EndList(false),
                b']' => Event::EndList(true),
                b'"' => Event::Str(try!(read_escaped(file, StringOrSymbol::String))),
                b'|' => Event::Symbol(try!(read_escaped(file, StringOrSymbol::Symbol))),
                b'\t'...b'\r' | b' ' => continue, // ASCII whitespace
                
            })
        } else {
            return Ok(Event::EOF);
        }
    }
}
