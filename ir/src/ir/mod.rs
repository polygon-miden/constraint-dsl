mod constraints;
mod graph;
mod leaf;
mod link;
mod mir;
mod node;
mod nodes;
mod owner;
mod trace;
pub extern crate derive_ir;

pub use constraints::{ConstraintDomain, ConstraintError, ConstraintRoot, Constraints};
pub use derive_ir::Builder;
pub use graph::Graph;
pub use leaf::Leaf;
pub use link::{BackLink, Link};
pub use mir::Mir;
pub use node::Node;
pub use nodes::*;
pub use owner::Owner;
pub use trace::TraceAccess;

/// A trait for nodes that can have children
/// This is used with the Child trait to allow for easy traversal and manipulation of the graph
pub trait Parent {
    type Child;
    fn children(&self) -> Link<Vec<Link<Self::Child>>>;
}

impl<T> Parent for Link<T>
where
    T: Parent,
{
    type Child = T::Child;
    fn children(&self) -> Link<Vec<Link<Self::Child>>> {
        self.borrow().children()
    }
}

impl<T> Parent for BackLink<T>
where
    Link<T>: Parent,
{
    type Child = <Link<T> as Parent>::Child;
    fn children(&self) -> Link<Vec<Link<Self::Child>>> {
        match self.to_link() {
            Some(link) => link.children(),
            None => Link::new(Vec::new()),
        }
    }
}

/// A trait for nodes that can have a parent
/// This is used with the Parent trait to allow for easy traversal and manipulation of the graph
pub trait Child: Clone + Into<Link<Self>> + PartialEq {
    type Parent;
    fn get_parents(&self) -> Vec<BackLink<Self::Parent>>;
    fn add_parent(&mut self, parent: Link<Self::Parent>);
    fn remove_parent(&mut self, parent: Link<Self::Parent>);
}

impl<T> Child for Link<T>
where
    T: Child,
{
    type Parent = T::Parent;

    fn get_parents(&self) -> Vec<BackLink<Self::Parent>> {
        self.borrow().get_parents()
    }
    fn add_parent(&mut self, parent: Link<Self::Parent>) {
        self.borrow_mut().add_parent(parent)
    }
    fn remove_parent(&mut self, parent: Link<Self::Parent>) {
        self.borrow_mut().remove_parent(parent)
    }
}

impl<T> Child for BackLink<T>
where
    Link<T>: Child,
{
    type Parent = <Link<T> as Child>::Parent;
    fn get_parents(&self) -> Vec<BackLink<Self::Parent>> {
        match self.to_link() {
            Some(link) => link.get_parents(),
            None => Vec::new(),
        }
    }
    fn add_parent(&mut self, parent: Link<Self::Parent>) {
        if let Some(ref mut link) = self.to_link() {
            link.add_parent(parent)
        }
    }
    fn remove_parent(&mut self, parent: Link<Self::Parent>) {
        if let Some(ref mut link) = self.to_link() {
            link.remove_parent(parent)
        }
    }
}

/// A trait implemented by all nodes.
/// Will be derivable later. The implementation and type-safe builder is currently manual while we tweak the design
pub trait Builder {
    type Empty;
    type Full;
    /// Create a new empty builder that exposes all fields
    fn builder() -> Self::Empty;
}
