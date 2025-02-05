use crate::{
    ir::{Graph, Link, Node, Op, Parent, Root},
    CompileError,
};

use std::ops::Deref;

pub trait Visitor {
    fn work_stack(&mut self) -> &mut Vec<Link<Node>>;
    fn root_nodes_to_visit(&self, graph: &Graph) -> Vec<Link<Node>>;
    fn run(&mut self, graph: &mut Graph) -> Result<(), CompileError> {
        for root in self.root_nodes_to_visit(graph) {
            self.scan_node(graph, root.clone())?;

            while let Some(node) = self.work_stack().pop() {
                self.visit_node(graph, node)?;
            }
        }
        Ok(())
    }
    fn scan_node(&mut self, _graph: &Graph, node: Link<Node>) -> Result<(), CompileError> {
        self.work_stack().push(node.clone());
        if let Some(_owner) = node.clone().as_owner() {
            for child in node.children().borrow().iter() {
                self.scan_node(_graph, child.clone().as_node())?;
            }
        }
        Ok(())
    }
    fn visit_node(&mut self, graph: &mut Graph, node: Link<Node>) -> Result<(), CompileError> {
        if node.is_stale() {
            return Ok(());
        }
        match node.borrow().deref() {
            Node::Function(f) => self.visit_function(graph, f.clone().into()),
            Node::Evaluator(e) => self.visit_evaluator(graph, e.clone().into()),
            Node::Enf(e) => self.visit_enf(graph, e.clone().into()),
            Node::Boundary(b) => self.visit_boundary(graph, b.clone().into()),
            Node::Add(a) => self.visit_add(graph, a.clone().into()),
            Node::Sub(s) => self.visit_sub(graph, s.clone().into()),
            Node::Mul(m) => self.visit_mul(graph, m.clone().into()),
            Node::Exp(e) => self.visit_exp(graph, e.clone().into()),
            Node::If(i) => self.visit_if(graph, i.clone().into()),
            Node::For(f) => self.visit_for(graph, f.clone().into()),
            Node::Call(c) => self.visit_call(graph, c.clone().into()),
            Node::Fold(f) => self.visit_fold(graph, f.clone().into()),
            Node::Vector(v) => self.visit_vector(graph, v.clone().into()),
            Node::Matrix(m) => self.visit_matrix(graph, m.clone().into()),
            Node::Accessor(a) => self.visit_accessor(graph, a.clone().into()),
            Node::Parameter(p) => self.visit_parameter(graph, p.clone().into()),
            Node::Value(v) => self.visit_value(graph, v.clone().into()),
            Node::None(_) => Ok(()),
        }
    }
    fn visit_function(
        &mut self,
        _graph: &mut Graph,
        _function: Link<Root>,
    ) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_evaluator(
        &mut self,
        _graph: &mut Graph,
        _evaluator: Link<Root>,
    ) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_enf(&mut self, _graph: &mut Graph, _enf: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_boundary(
        &mut self,
        _graph: &mut Graph,
        _boundary: Link<Op>,
    ) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_add(&mut self, _graph: &mut Graph, _add: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_sub(&mut self, _graph: &mut Graph, _sub: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_mul(&mut self, _graph: &mut Graph, _mul: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_exp(&mut self, _graph: &mut Graph, _exp: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_if(&mut self, _graph: &mut Graph, _if_node: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_for(&mut self, _graph: &mut Graph, _for_node: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_call(&mut self, _graph: &mut Graph, _call: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_fold(&mut self, _graph: &mut Graph, _fold: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_vector(&mut self, _graph: &mut Graph, _vector: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_matrix(&mut self, _graph: &mut Graph, _matrix: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_accessor(
        &mut self,
        _graph: &mut Graph,
        _accessor: Link<Op>,
    ) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_parameter(
        &mut self,
        _graph: &mut Graph,
        _parameter: Link<Op>,
    ) -> Result<(), CompileError> {
        Ok(())
    }
    fn visit_value(&mut self, _graph: &mut Graph, _value: Link<Op>) -> Result<(), CompileError> {
        Ok(())
    }
}
