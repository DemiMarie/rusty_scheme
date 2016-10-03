

use value;
use interp;
use api;

pub fn ast_eval(s: &mut interp::State) {
    use value::Tags;
    match s.heap.stack.len() {
        0 => return,
        depth => if s.heap.stack[depth-1].tag().self_evaluating() {
            return
        } else {
            
