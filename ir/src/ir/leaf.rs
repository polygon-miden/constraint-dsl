use crate::ir::{BackLink, Child, Link, Op, Owner};

/// The Final nodes of the MIR Graph.
/// Currently unused in the structure but will be used in the next visitor pattern implementation.
#[derive(Default, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Leaf {
    Parameter(BackLink<Op>),
    Value(BackLink<Op>),
    #[default]
    None,
}

impl Child for Leaf {
    type Parent = Owner;
    fn get_parents(&self) -> Vec<BackLink<Self::Parent>> {
        match self {
            Leaf::Parameter(p) => p.get_parents(),
            Leaf::Value(v) => v.get_parents(),
            Leaf::None => vec![],
        }
    }
    fn add_parent(&mut self, parent: Link<Self::Parent>) {
        match self {
            Leaf::Parameter(p) => p.add_parent(parent),
            Leaf::Value(v) => v.add_parent(parent),
            Leaf::None => (),
        }
    }
    fn remove_parent(&mut self, parent: Link<Self::Parent>) {
        match self {
            Leaf::Parameter(p) => p.remove_parent(parent),
            Leaf::Value(v) => v.remove_parent(parent),
            Leaf::None => (),
        }
    }
}
