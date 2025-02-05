use miden_diagnostics::{SourceSpan, Spanned};
use std::cell::RefCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::{Rc, Weak};

pub struct Link<T>
where
    T: Sized,
{
    pub link: Rc<RefCell<T>>,
}

impl<T> Link<T> {
    pub fn new(data: T) -> Self {
        Self {
            link: Rc::new(RefCell::new(data)),
        }
    }
    pub fn borrow(&self) -> std::cell::Ref<T> {
        self.link.borrow()
    }
    pub fn borrow_mut(&self) -> std::cell::RefMut<T> {
        self.link.borrow_mut()
    }
    pub fn update(&self, other: &Self)
    where
        T: Clone + Debug,
    {
        //eprintln!("update:\n    {:#?}\n  ->{:#?}", self, other);
        //eprintln!("old_ptr: {}", self.get_ptr());
        *self.borrow_mut() = other.borrow().clone();
        //eprintln!("new_ptr: {}", self.get_ptr());
    }
    pub fn get_ptr(&self) -> usize {
        Rc::as_ptr(&self.link) as usize
    }
}

impl<T: Debug> Debug for Link<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.link.borrow().fmt(f)
    }
}

impl<T> Default for Link<T>
where
    T: Default,
{
    fn default() -> Self {
        Self {
            link: Rc::new(RefCell::new(T::default())),
        }
    }
}

impl<T> Clone for Link<T> {
    fn clone(&self) -> Self {
        Self {
            link: self.link.clone(),
        }
    }
}

impl<T: PartialEq> PartialEq for Link<T> {
    fn eq(&self, other: &Self) -> bool {
        self.link == other.link
    }
}

impl<T> Eq for Link<T> where T: Eq {}

impl<T> From<T> for Link<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<T> From<BackLink<T>> for Link<T> {
    fn from(value: BackLink<T>) -> Self {
        value.to_link().unwrap()
    }
}

impl<T> From<Rc<RefCell<T>>> for Link<T> {
    fn from(value: Rc<RefCell<T>>) -> Self {
        Self { link: value }
    }
}

impl<T> Spanned for Link<T>
where
    T: Spanned,
{
    fn span(&self) -> SourceSpan {
        self.borrow().span()
    }
}

pub struct BackLink<T> {
    pub link: Option<Weak<RefCell<T>>>,
}

impl<T> BackLink<T> {
    pub fn none() -> Self {
        Self { link: None }
    }
    pub fn to_link(&self) -> Option<Link<T>> {
        match self.link.as_ref() {
            Some(link) => link.upgrade().map(|link| Link { link }),
            None => None,
        }
    }
}

impl<T> Default for BackLink<T> {
    fn default() -> Self {
        Self { link: None }
    }
}

impl<T: std::fmt::Debug> Debug for BackLink<T> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.link.as_ref() {
            Some(_) => match self.to_link() {
                Some(link) => write!(_f, "BackLink@{:?}", link.get_ptr()),
                None => write!(_f, "BackLink@None"),
            },
            None => write!(_f, "BackLink@None"),
        }
    }
}

impl<T> Clone for BackLink<T> {
    fn clone(&self) -> Self {
        Self {
            link: self.link.clone(),
        }
    }
}

impl<T> PartialEq for BackLink<T> {
    /// Always returns true because the field should be ignored in comparisons.
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T> Eq for BackLink<T> {}

impl<T> From<Link<T>> for BackLink<T> {
    fn from(parent: Link<T>) -> Self {
        Self {
            link: Some(Rc::downgrade(&parent.link)),
        }
    }
}

impl<T> Hash for Link<T>
where
    T: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.link.borrow().hash(state)
    }
}

impl<T> From<Rc<RefCell<T>>> for BackLink<T> {
    fn from(parent: Rc<RefCell<T>>) -> Self {
        Self {
            link: Some(Rc::downgrade(&parent)),
        }
    }
}

impl<T> Hash for BackLink<T> {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

impl<T> Spanned for BackLink<T>
where
    T: Spanned,
{
    fn span(&self) -> SourceSpan {
        match self.to_link() {
            Some(link) => link.span(),
            None => SourceSpan::default(),
        }
    }
}
