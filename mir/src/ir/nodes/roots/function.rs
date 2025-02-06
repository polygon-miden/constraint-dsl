use crate::ir::{Builder, Link, Node, Op, Owner, Parent, Root};
use miden_diagnostics::{SourceSpan, Spanned};

#[derive(Default, Clone, PartialEq, Eq, Debug, Hash, Builder, Spanned)]
#[enum_wrapper(Root)]
pub struct Function {
    pub parameters: Vec<Link<Op>>, // Parameter
    pub return_type: Link<Op>,     // Parameter
    pub body: Link<Vec<Link<Op>>>,
    pub _node: Option<Link<Node>>,
    pub _owner: Option<Link<Owner>>,
    #[span]
    span: SourceSpan,
}

impl Function {
    pub fn create(
        parameters: Vec<Link<Op>>,
        return_type: Link<Op>,
        body: Vec<Link<Op>>,
        span: SourceSpan,
    ) -> Link<Root> {
        Root::Function(Self {
            parameters,
            return_type,
            body: Link::new(body),
            span,
            ..Default::default()
        })
        .into()
    }
}

impl Parent for Function {
    type Child = Op;
    fn children(&self) -> Link<Vec<Link<Self::Child>>> {
        self.body.clone()
    }
}
