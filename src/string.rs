use std::ptr;
use std::slice;
use std::str;

use api;
use value;
use alloc;
#[repr(C)]
pub struct SchemeStr {
    header: usize,

    /// The type of the object.  Always zero.
    ty: usize,

    /// The length in bytes of the following `str`
    len: usize,
}

unsafe impl api::SchemeValue for String {
    fn to_value(&self, heap: &mut alloc::Heap) -> value::Value {
        assert!(size_of!(SchemeStr) == 3 * size_of!(usize));
        let object_len: usize = ((size_of!(SchemeStr) + self.len() +
                          0b111) & !0b111)/size_of!(usize);
        let (value_ptr, _) = heap.alloc_raw(object_len,
                                                    value::HeaderTag::RustData);
        let ptr = value_ptr as usize | value::RUST_DATA_TAG;
        unsafe {
            let real_ptr = value_ptr as *mut usize;
            ptr::copy_nonoverlapping(
                self.as_ptr(),
                (value_ptr as usize + size_of!(SchemeStr)) as *mut u8,
                self.len());
            (*real_ptr) = (object_len * size_of!(usize)) |
            value::HeaderTag::RustData as usize;
            (*real_ptr.offset(1)) = 0; // String
            (*real_ptr.offset(2)) = self.len();
        }
        value::Value::new(ptr)
    }
    fn of_value(val: &value::Value) -> Result<Self, String> {
        if val.raw_tag() != value::RUST_DATA_TAG {
            return Err("Value is not a string".to_owned())
        }
        unsafe {
            let scheme_str_ptr = val.as_ptr() as usize;
            if *((scheme_str_ptr + size_of!(usize)) as *const u8) != 0 {
                return Err("Value is not a string".to_owned())
            }
            let ptr = val.as_ptr() as *const u8;
            Ok(str::from_utf8(
                slice::from_raw_parts(
                    ptr.offset(size_of!(SchemeStr) as isize),
                    (*(ptr as *const SchemeStr)).len)).expect(
                "String not valid UTF-8???").to_owned())
        }
    }
}
