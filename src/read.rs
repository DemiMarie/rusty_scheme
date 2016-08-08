use std::io;
use std::io::prelude::*;
use std::char;
use std::iter::Peekable;
use super::interp;

#[derive(Debug)]
pub enum ReadError {
    /// EOF in list
    EOFInList,

    /// EOF in vector
    EOFInVector,

    /// Missing `)`
    MissingCloseParen,

    /// Input/output error
    IoError(io::Error),

    /// EOF in string
    EOFInString,

    /// EOF in symbol
    EOFInSymbol,

    /// EOF after `#\\`
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

    /// `|` in symbol unescaped
    PipeInSymbol,
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

    /// Dot `.`
    Dot,

    /// End of file
    EOF,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum StringOrSymbol {
    String,
    Symbol,
}

fn char_from_stream_and_byte<R: BufRead>(file: &mut Peekable<Bytes<R>>,
                                         unicode_char: u8)
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
            for (count, current_byte_value) in file.take(len.into()).enumerate() {
                let current: u32 = try!(current_byte_value.map_err(ReadError::IoError)).into();
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
fn handle_unicode_escape<R: BufRead>(file: &mut Peekable<Bytes<R>>) -> ReadResult {
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

fn process_escape<R: BufRead>(file: &mut Peekable<Bytes<R>>) -> ReadResult {
    let bad = ReadError::BadEscape;
    loop {
        return Ok(match next!(file, ReadError::BadEscape) {
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



fn read_escaped<R: BufRead>(file: &mut Peekable<Bytes<R>>,
                            delimiter: StringOrSymbol)
                            -> Result<String, ReadError> {
    use std::char;
    let premature_eof = || {
        match delimiter {
            StringOrSymbol::String => ReadError::EOFInString,
            StringOrSymbol::Symbol => ReadError::EOFInSymbol,
        }
    };

    let mut buf = String::new();
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

pub struct Reader<'a, 'b, T: 'a + BufRead> {
    stream: &'a mut T,
    state: &'b mut interp::State,
}

pub struct EventSource<'a, R: 'a + BufRead> {
    file: &'a mut Peekable<Bytes<R>>,
    last_chr: Option<u8>,
}

impl<'a, R: BufRead> EventSource<'a, R> {
    pub fn new(reader: &'a mut Peekable<Bytes<R>>) -> Self {
        EventSource {
            file: reader,
            last_chr: Default::default(),
        }
    }
}
impl<'a, R: BufRead> Iterator for EventSource<'a, R> {
    type Item = Result<Event, ReadError>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        let handle_splicing = |file: &mut Peekable<Bytes<R>>, nosplice: Event, splice: Event| {
            let res1 = {
                let q = file.peek();
                match q {
                    Some(&Ok(b'@')) => (Ok(splice), true),
                    Some(&Ok(_)) | None => (Ok(nosplice), false),
                    Some(&Err(ref _a)) => (Err(()), true),
                }
            };
            match res1 {
                (Ok(good), take_char) => {
                    if take_char {
                        file.next();
                    }
                    Ok(good)
                }
                (Err(()), false) => unreachable!(),
                (Err(()), true) => Err(file.next().unwrap().unwrap_err()),
            }
        };

        macro_rules! my_try {
            ($exp: expr) => {
                match $exp {
                    Ok(x) => x,
                    Err(x) => return Some(Err(x)),
                }
            }
        }

        macro_rules! iter_next {
            ($exp: expr, $err: expr) => {
                my_try!(my_try!($exp.next().ok_or($err)).map_err(|x|ReadError::IoError(x)))
            }
        }
        loop {
            let chr = if let Some(chr) = self.last_chr {
                chr
            } else {
                let next: Option<Result<u8, io::Error>> = self.file.next();
                if let Some(c) = next {
                    my_try!(c.map_err(ReadError::IoError))
                } else {
                    return None;
                }
            };
            return Some(Ok(match chr {
                b'(' => Event::StartList(false),
                b'[' => Event::StartList(true),
                b'\'' => Event::Quote,
                b'`' => Event::Quasiquote,
                b',' => {
                    my_try!(handle_splicing(self.file, Event::Unquote, Event::UnquoteSplicing)
                                .map_err(ReadError::IoError))
                }
                b'#' => {
                    match iter_next!(self.file, ReadError::EOFAfterSharp) {
                        b'.' => Event::ReadEval,
                        b'\\' => {
                            let byte = iter_next!(self.file, ReadError::EOFAfterSharpBackslash);
                            Event::Char(my_try!(char_from_stream_and_byte(self.file, byte)))
                        }
                        b't' => Event::True,
                        b'f' => Event::False,
                        b'\'' => Event::Syntax,
                        b'`' => Event::Quasisyntax,
                        b',' => {
                            my_try!(handle_splicing(self.file,
                                                    Event::Unsyntax,
                                                    Event::UnsyntaxSplicing)
                                        .map_err(ReadError::IoError))
                        }
                        b'(' => Event::StartVec,
                        dispatch_char => {
                            return Some(Err(ReadError::BadSharpMacro([dispatch_char as char,
                                                                      '\0'])))
                        }
                    }
                }
                b')' => Event::EndList(false),
                b']' => Event::EndList(true),
                b'"' => Event::Str(my_try!(read_escaped(self.file, StringOrSymbol::String))),
                b'|' => Event::Symbol(my_try!(read_escaped(self.file, StringOrSymbol::Symbol))),
                b'\t'...b'\r' | b' ' => continue, // ASCII whitespace
                val => {
                    let chr = if val < 0x7F {
                        val as char
                    } else {
                        my_try!(char_from_stream_and_byte(self.file, val))
                    };
                    if chr.is_whitespace() {
                        continue;
                    }
                    let mut buf = String::new();
                    buf.push(chr);
                    loop {
                        let chr = if let Some(x) = self.file.next() {
                            my_try!(x.map_err(ReadError::IoError))
                        } else {
                            break;
                        };
                        match chr {
                            b'\\' => buf.push(my_try!(process_escape(self.file))),
                            b'|' => return Some(Err(ReadError::PipeInSymbol)),
                            a @ b'"' |
                            a @ b'\'' |
                            a @ b'`' |
                            a @ b',' |
                            a @ b'(' |
                            a @ b'[' |
                            a @ b']' |
                            a @ b')' |
                            a @ b'{' |
                            a @ b'}' => {
                                self.last_chr = Some(a);
                                break;
                            }
                            b'\t'...b'\r' | b' ' => break, // ASCII whitespace
                            chr => {
                                let unicode_char = my_try!(char_from_stream_and_byte(self.file,
                                                                                     chr));
                                if unicode_char.is_whitespace() {
                                    break;
                                }
                                buf.push(unicode_char)
                            }
                        }
                    }
                    return Some(Ok(if &buf == "." {
                        Event::Dot
                    } else {
                        Event::Symbol(buf)
                    }));
                }
            }));
        }
    }
}
