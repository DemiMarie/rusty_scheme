use alloc::Heap;
use value;
#[repr(C)]
pub struct SchemeStr {
    header: usize,

    /// The type of the object.  Always zero.
    ty: usize,

    /// The length in bytes of the following `str`
    len: usize,
}

unsafe impl SchemeValue for String {
    fn to_value(&self, heap: &mut alloc::Heap) -> value::Value {
        assert!(size_of!(SchemeStr) == 3 * size_of!(usize));
        let object_len = ((size_of!(SchemeStr) + self.len() +
                          0b111) & !0b111)/size_of!(usize);
        let (value_ptr, final_len) = heap.check_space(object_len);
        let ptr = value_ptr | value::RUSTDATA_TAG;
        unsafe {
            let real_ptr = value_ptr as *mut usize;
            ptr::copy_nonoverlapping(self.as_ptr(),
                                     (value_ptr + size_of!(SchemeStr)) as *mut u8,
                                     self.len());
            (*real_ptr) = object_length * size_of!(usize) | value::RUSTDATA_HEADER;
            (*real_ptr.offset(1)) = 0; // String
            (*real_ptr.offset(2)) = self.len();
        }
        return value::Value::new(ptr)
    }
    fn of_value(val: &value::Value) -> Result<Self, String> {
        if val.raw_tag() != value::RUSTDATA_TAG {
            return Err("Value is not a string".to_owned())
        }
        unsafe {
            let schemeStr_ptr = val.as_ptr() as usize;
            if *((schemeStr_ptr + size_of!(usize)) as *const u8) != 0 {
                return Err("Value is not a string".to_owned())
            }
            let ptr = val.as_ptr() as *const u8;
            str::from_raw_parts(ptr.offset(size_of!(SchemeStr)),
                                (*(ptr as *const SchemeStr)).len).to_owned()
        }
    }
}
