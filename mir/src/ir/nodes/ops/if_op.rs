use crate::ir::{BackLink, Builder, Child, Link, Node, Op, Owner, Parent};
use miden_diagnostics::{SourceSpan, Spanned};

#[derive(Default, Clone, PartialEq, Eq, Debug, Hash, Builder, Spanned)]
#[enum_wrapper(Op)]
pub struct If {
    pub parents: Vec<BackLink<Owner>>,
    pub condition: Link<Op>,
    pub then_branch: Link<Op>,
    pub else_branch: Link<Op>,
    pub _node: Option<Link<Node>>,
    pub _owner: Option<Link<Owner>>,
    #[span]
    span: SourceSpan,
}

impl If {
    pub fn create(
        condition: Link<Op>,
        then_branch: Link<Op>,
        else_branch: Link<Op>,
        span: SourceSpan,
    ) -> Link<Op> {
        Op::If(Self {
            condition,
            then_branch,
            else_branch,
            span,
            ..Default::default()
        })
        .into()
    }
}

impl Parent for If {
    type Child = Op;
    fn children(&self) -> Link<Vec<Link<Self::Child>>> {
        Link::new(vec![
            self.condition.clone(),
            self.then_branch.clone(),
            self.else_branch.clone(),
        ])
    }
}

impl Child for If {
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
