use alloc;

/// A bytecode object.  Consists of a header, the length of the bytecodes,
/// the actual bytecodes, and finally the constants vector (not actually part
/// of the BCO, but always allocated after it).
pub struct BCO {
    /// The standard header object
    header: usize,

    /// The length of the bytecodes
    bytecode_length: usize,

    /// Bytecodes.  Followed by the constants.
    bytecodes: [u8],
}

