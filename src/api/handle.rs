use std::cell;
use value;
use libc;
pub struct Handle<'a> {
    index: usize,
    reference: &'a cell::UnsafeCell<Table>,
}
enum InternalHandle<T> {
    SchemeObject(T),
    AnotherHandle(usize),
}

struct Table {
    table: Vec<Handle>,
    first_free: usize,
}

impl<'a> Drop for Handle<'a> {
    fn Drop(&mut self) {
        self.reference.

impl Index<Handle, Output=T> for Table<T> {
    fn index(&self, index: &Handle) -> &T {
        match self[index.0] {
            SchemeObject(ref x) => x,
            AnotherHandle(n) => panic!("Expected object, found link {:?}", n),
        }
    }
}

impl IndexMut<Handle> for Table<T> {
    fn index(&self, index: &Handle) -> &T {
        match self[index.0] {
            SchemeObject(ref mut x) => x,
            AnotherHandle(n) => panic!("Expected object, found link {:?}", n),
        }
    }
}

impl Drop for Handle

impl Iterator<Item=T> for Table<T> {
