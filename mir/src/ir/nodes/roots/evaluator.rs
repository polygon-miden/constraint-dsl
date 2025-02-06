use miden_diagnostics::{SourceSpan, Spanned};

use crate::ir::{Builder, Link, Node, Op, Owner, Parent, Root};

#[derive(Default, Clone, PartialEq, Eq, Debug, Hash, Builder, Spanned)]
#[enum_wrapper(Root)]
pub struct Evaluator {
    pub parameters: Vec<Vec<Link<Op>>>,
    pub body: Link<Vec<Link<Op>>>,
    pub _node: Option<Link<Node>>,
    pub _owner: Option<Link<Owner>>,
    #[span]
    span: SourceSpan,
}

impl Evaluator {
    pub fn create(
        parameters: Vec<Vec<Link<Op>>>,
        body: Vec<Link<Op>>,
        span: SourceSpan,
    ) -> Link<Root> {
        Root::Evaluator(Self {
            parameters,
            body: Link::new(body),
            span,
            ..Default::default()
        })
        .into()
    }
}

impl Parent for Evaluator {
    type Child = Op;
    fn children(&self) -> Link<Vec<Link<Self::Child>>> {
        self.body.clone()
    }
}
