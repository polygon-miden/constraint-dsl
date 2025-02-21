use crate::ir::{BackLink, Builder, Child, Link, Node, Op, Owner, Parent};
use air_parser::ast::Boundary as BoundaryKind;
use miden_diagnostics::{SourceSpan, Spanned};
use std::hash::Hash;

/// A MIR operation to represent bounding a given op, `expr`, to access either the first or last row
///
/// Note: Boundary ops are only valid to describe boundary constraints, not integrity constraints
///
#[derive(Clone, PartialEq, Eq, Debug, Builder, Spanned)]
#[enum_wrapper(Op)]
pub struct Boundary {
    pub parents: Vec<BackLink<Owner>>,
    pub kind: BoundaryKind,
    pub expr: Link<Op>,
    pub _node: Option<Link<Node>>,
    pub _owner: Option<Link<Owner>>,
    #[span]
    span: SourceSpan,
}

impl Default for Boundary {
    fn default() -> Self {
        Self {
            parents: Vec::default(),
            kind: BoundaryKind::First,
            expr: Link::default(),
            _node: None,
            _owner: None,
            span: SourceSpan::default(),
        }
    }
}

impl Hash for Boundary {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self.kind {
            BoundaryKind::First => 0.hash(state),
            BoundaryKind::Last => 1.hash(state),
        }
        self.expr.hash(state);
    }
}

impl Boundary {
    pub fn create(expr: Link<Op>, kind: BoundaryKind, span: SourceSpan) -> Link<Op> {
        Op::Boundary(Self {
            expr,
            kind,
            span,
            ..Default::default()
        })
        .into()
    }
}

impl Parent for Boundary {
    type Child = Op;
    fn children(&self) -> Link<Vec<Link<Self::Child>>> {
        Link::new(vec![self.expr.clone()])
    }
}

impl Child for Boundary {
    type Parent = Owner;
    fn get_parents(&self) -> Vec<BackLink<Self::Parent>> {
        self.parents.clone()
    }
    fn add_parent(&mut self, parent: Link<Self::Parent>) {
        self.parents.push(parent.into());
    }
    fn remove_parent(&mut self, parent: Link<Self::Parent>) {
        self.parents.retain(|p| *p != parent.clone().into());
    }
}
