mod op;
mod ops;
mod root;
mod roots;

pub use op::Op;
pub use ops::*;
pub use root::Root;
pub use roots::*;
use std::cell::{Ref, RefMut};
pub fn get_inner<T, U>(obj: Ref<T>, getter: impl Fn(&T) -> Option<&U>) -> Option<Ref<U>> {
    Ref::filter_map(obj, getter).ok()
}

pub fn get_inner_mut<T, U>(
    obj: RefMut<T>,
    getter: impl Fn(&mut T) -> Option<&mut U>,
) -> Option<RefMut<U>> {
    RefMut::filter_map(obj, getter).ok()
}
