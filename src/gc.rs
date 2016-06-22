//! A simple copying garbage collector.
//!
//! This module implements the GC.

use value;
use value::Value;
use ::std::mem;
