use std::io;
use std::io::prelude::*;
use std::char;
use std::iter::Peekable;
use super::interp;
use super::api;
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

    /// Bad hex number
    BadHexNumber,

    /// Integer overflow
    Overflow,

    /// Bad use of `.`
    BadDot,

    /// Mismatched parentheses
    ParenMismatch,

    /// Host-set memory limit exceeded
    MemLimitExceeded,

    /// Not yet implemented
    NYI,
}

/// An event that can be emitted by the reader or tree-walker, and which
/// is part of the stream that is consumed by the tree-builder, printer,
/// and bytecode compiler.
#[derive(Clone, Debug)]
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
use self::ReadError::IoError;
fn finish_char<R: BufRead>(file: &mut Peekable<Bytes<R>>,
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
            for (count, val) in &mut file.take(len.into()).enumerate() {
                value &= (try!(val.map_err(IoError)) as u32) << (len - count as u8)
            }
            char::from_u32(value).ok_or_else(|| ReadError::InvalidUtf8(value))
        }
        _ => unreachable!(),
    }
}
macro_rules! next {
    ($exp: expr, $err: expr) => {
        try!(try!($exp.next().ok_or($err)).map_err(ReadError::IoError))
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
            normal_char => try!(finish_char(file, normal_char)),
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
        my_try!(
            my_try!($exp.next().ok_or($err)).map_err(ReadError::IoError))
    }
}

type Item<'a, R> = <EventSource<'a, R> as Iterator>::Item;
type ItemOption<'a, R> = Option<Item<'a, R>>;


impl<'a, R: BufRead> EventSource<'a, R> {
    pub fn new(reader: &'a mut Peekable<Bytes<R>>) -> Self {
        EventSource {
            file: reader,
            last_chr: Default::default(),
        }
    }

    fn handle_splicing(&mut self, nosplice: Event, splice: Event) -> Item<R> {
        match self.file.next() {
            Some(Ok(b'@')) => Ok(splice),
            Some(Ok(l)) => {
                self.last_chr = Some(l);
                Ok(nosplice)
            }
            None => {
                self.last_chr = None;
                Ok(nosplice)
            }
            Some(Err(a)) => Err(ReadError::IoError(a)),
        }
    }
    fn read_hex(&mut self) -> Item<R> {
        let mut buf = String::new();
        for i in &mut self.file {
            match try!(i.map_err(ReadError::IoError)) {
                i @ b'0'...b'9' | i @ b'A'...b'F' | i @ b'a'...b'f' => buf.push(i as char),
                _ => return Err(ReadError::BadHexNumber),
            }
        }
        if let Ok(x) = buf.parse() {
            Ok(Event::Int(x))
        } else {
            Err(ReadError::Overflow)
        }
    }
    fn process_sharpsign(&mut self) -> ItemOption<R> {
        Some(Ok(match iter_next!(self.file, ReadError::EOFAfterSharp) {
            b'.' => Event::ReadEval,
            b'\\' => {
                let byte = iter_next!(self.file, ReadError::EOFAfterSharpBackslash);
                Event::Char(my_try!(finish_char(self.file, byte)))
            }
            b't' => Event::True,
            b'f' => Event::False,
            b'x' => my_try!(self.read_hex()),
            b'\'' => Event::Syntax,
            b'`' => Event::Quasisyntax,
            b',' => my_try!(self.handle_splicing(Event::Unsyntax, Event::UnsyntaxSplicing)),
            b'(' => Event::StartVec,
            dispatch_char => {
                return Some(Err(ReadError::BadSharpMacro([dispatch_char as char, '\0'])))
            }
        }))
    }
    #[cfg_attr(feature = "clippy", allow(while_let_on_iterator))]
    fn read_symbol(&mut self, start: char) -> Result<Event, ReadError> {
        let mut buf = String::new();
        buf.push(start);
        while let Some(x) = self.file.next() {
            match try!(x.map_err(ReadError::IoError)) {
                b'\\' => buf.push(try!(process_escape(self.file))),
                b'|' => return Err(ReadError::PipeInSymbol),
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
                    let unicode_char = try!(finish_char(self.file, chr));
                    if unicode_char.is_whitespace() {
                        break;
                    }
                    buf.push(unicode_char)
                }
            }
        }
        Ok(if &buf == "." {
            Event::Dot
        } else {
            Event::Symbol(buf)
        })
    }
}


impl<'a, R: BufRead> Iterator for EventSource<'a, R> {
    type Item = Result<Event, ReadError>;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        loop {
            let chr = if let Some(chr) = self.last_chr {
                self.last_chr = None;
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
                b',' => my_try!(self.handle_splicing(Event::Unquote, Event::UnquoteSplicing)),
                b'#' => return self.process_sharpsign(),
                b')' => Event::EndList(false),
                b']' => Event::EndList(true),
                b'"' => Event::Str(my_try!(read_escaped(self.file, StringOrSymbol::String))),
                b'|' => Event::Symbol(my_try!(read_escaped(self.file, StringOrSymbol::Symbol))),
                b'\t'...b'\r' | b' ' => continue, // ASCII whitespace
                val => {
                    let chr = if val < 0x7F {
                        val as char
                    } else {
                        my_try!(finish_char(self.file, val))
                    };
                    if chr.is_whitespace() {
                        continue;
                    }
                    return Some(self.read_symbol(chr));
                }
            }));
        }
    }
}

pub fn read<R: BufRead>(s: &mut api::State, r: &mut Peekable<Bytes<R>>) -> Result<(), ReadError> {
    #[derive(Copy, Clone, Debug)]
    enum State {
        List {
            is_square: bool,
            depth: usize,
        },
        DottedList {
            is_square: bool,
            depth: usize,
        },
        Vec {
            depth: usize,
        },
        ReaderMacro,
    }
    let mut read_stack: Vec<State> = Vec::new();
    let mut source = EventSource::new(r);
    loop {
        let i = match source.next() {
            None => return Ok(()),
            Some(x) => x,
        };
        match try!(i) {
            Event::Char(_) => unimplemented!(),
            Event::Int(x) => {
                s.push(x).unwrap();
                // try!(execute_macros(source))
            }
            Event::Str(st) => {
                s.push(st).unwrap();
                // try!(execute_macros(source))
            }
            Event::Symbol(st) => {
                s.intern(&st).unwrap();
                // try!(execute_macros(source))
            }
            Event::Dot => {
                let len = read_stack.len().wrapping_sub(1);
                if let Some(x) = read_stack.get_mut(len) {
                    match *x {
                        State::List { depth, is_square } => {
                            *x = State::DottedList {
                                depth: depth,
                                is_square: is_square,
                            };
                            continue
                        }
                        _ => return Err(ReadError::BadDot),
                    }
                } else {
                    return Err(ReadError::BadDot);
                }
                continue;
            }
            Event::EndList(is_square) => {
                if let Some(state) = read_stack.pop() {
                    match state {
                        State::DottedList { .. } | State::ReaderMacro =>
                            return Err(ReadError::UnexpectedCloseParen),
                        State::Vec { depth } => {
                            debug_assert!(depth > 0);
                            if is_square {
                                return Err(ReadError::BadCloseParen)
                            } else {
                                s.vector(1, depth).expect("Out of mem!")
                            }
                        }
                        State::List { is_square: square, depth } => {
                            if square == is_square {
                                s.list(depth).expect("Out of mem!")
                            } else {
                                return Err(ReadError::BadCloseParen)
                            }
                        }
                    }
                } else {
                    return Ok(())
                }
            }
            Event::StartVec => {
                read_stack.push(State::Vec { depth: 0 });
                continue;
            }
            Event::StartList(x) => {
                read_stack.push(State::List {
                    is_square: x,
                    depth: 0,
                });
                continue;
            }
            Event::Quote => {
                try!(s.push("quote".to_owned()).map_err(|()| ReadError::MemLimitExceeded));
                read_stack.push(State::ReaderMacro);
                continue;
            }
            Event::Quasiquote => {
                try!(s.push("backquote".to_owned()).map_err(|()| ReadError::MemLimitExceeded));
                read_stack.push(State::ReaderMacro);
                continue;
            }
            Event::Unquote => {
                try!(s.push("unquote".to_owned()).map_err(|()| ReadError::MemLimitExceeded));
                read_stack.push(State::ReaderMacro);
                continue;
            }
            _ => return Err(ReadError::NYI),
        }
        let last = read_stack.len().wrapping_sub(1);
        if let Some(&x) = read_stack.get(last) {
            match x {
                State::ReaderMacro => {
                    try!(s.list(2).map_err(|_| ReadError::MemLimitExceeded));
                    read_stack.pop();
                }
                State::List { depth, is_square } => {
                    read_stack[last] = State::List {
                        depth: depth + 1,
                        is_square: is_square,
                    }
                }
                State::Vec { depth } => {
                    read_stack[last] = State::Vec {
                        depth: depth + 1,
                    }
                }
                State::DottedList { depth, is_square } => {
                    try!(s.list_with_tail(depth).map_err(|_| ReadError::MemLimitExceeded));
                    if let Some(token) = source.next() {
                        debug!("Token that must be close paren: {:?}\n", token);
                        match try!(token) {
                            Event::EndList(x) if x == is_square => continue,
                            Event::EndList(_) => return Err(ReadError::ParenMismatch),
                            _ => return Err(ReadError::MissingCloseParen),
                        }
                    } else {
                        s.drop().expect("Empty stack after list_with_tail?");
                        return Err(ReadError::BadDot);
                    }
                }
            }
        } else {
            return Ok(());
        }
    }
}

#[cfg(test)]
mod test {
    use std::io::Read;
    use env_logger;
    use api;
    #[test]
    fn read_from_bytes() {
        let _ = env_logger::init();
        let mut interp = api::State::new();
        let iter = b"(a b c . d)";
        super::read(&mut interp, &mut iter.bytes().peekable()).unwrap();
        assert_eq!(interp.len(), 1);
    }

    #[test]
    fn read_to_vec() {
        let _ = env_logger::init();
        let mut interp = api::State::new();
        let mut iter = b"#(a b c d)".bytes().peekable();
        super::read(&mut interp, &mut iter).unwrap();
    }
}
