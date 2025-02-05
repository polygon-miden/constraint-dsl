use crate::{
    ir::{Evaluator, Function, Link, Op, Root},
    CompileError,
};
use std::{
    cell::{Ref, RefMut},
    collections::BTreeMap,
};

use air_parser::ast::QualifiedIdentifier;

#[derive(Debug, Default)]
pub struct Graph {
    functions: BTreeMap<QualifiedIdentifier, Link<Root>>,
    evaluators: BTreeMap<QualifiedIdentifier, Link<Root>>,
    pub boundary_constraints_roots: Link<Vec<Link<Op>>>,
    pub integrity_constraints_roots: Link<Vec<Link<Op>>>,
}

impl Graph {
    pub fn create() -> Link<Self> {
        Graph::default().into()
    }
    pub fn insert_function(
        &mut self,
        ident: QualifiedIdentifier,
        node: Link<Root>,
    ) -> Result<(), CompileError> {
        match self.functions.insert(ident, node) {
            None => Ok(()),
            Some(link) => {
                if let Root::None(_) = *link.borrow() {
                    Ok(())
                } else {
                    Err(CompileError::Failed)
                }
            }
        }
    }

    pub fn get_function_root(&self, ident: &QualifiedIdentifier) -> Option<Link<Root>> {
        self.functions.get(ident).cloned()
    }

    pub fn get_function(&self, ident: &QualifiedIdentifier) -> Option<Ref<Function>> {
        self.functions.get(ident).map(|n| n.as_function().unwrap())
    }

    pub fn get_function_mut(&mut self, ident: &QualifiedIdentifier) -> Option<RefMut<Function>> {
        self.functions
            .get_mut(ident)
            .map(|n| n.as_function_mut().unwrap())
    }

    pub fn get_function_nodes(&self) -> Vec<Link<Root>> {
        self.functions.values().cloned().collect()
    }

    pub fn insert_evaluator(
        &mut self,
        ident: QualifiedIdentifier,
        node: Link<Root>,
    ) -> Result<(), CompileError> {
        match self.evaluators.insert(ident, node) {
            None => Ok(()),
            Some(link) => {
                if let Root::None(_) = *link.borrow() {
                    Ok(())
                } else {
                    Err(CompileError::Failed)
                }
            }
        }
    }

    pub fn get_evaluator_root(&self, ident: &QualifiedIdentifier) -> Option<Link<Root>> {
        self.evaluators.get(ident).cloned()
    }

    pub fn get_evaluator(&self, ident: &QualifiedIdentifier) -> Option<Ref<Evaluator>> {
        self.evaluators
            .get(ident)
            .map(|n| n.as_evaluator().unwrap())
    }

    pub fn get_evaluator_mut(&mut self, ident: &QualifiedIdentifier) -> Option<RefMut<Evaluator>> {
        self.evaluators
            .get_mut(ident)
            .map(|n| n.as_evaluator_mut().unwrap())
    }

    pub fn get_evaluator_nodes(&self) -> Vec<Link<Root>> {
        self.evaluators.values().cloned().collect()
    }

    pub fn insert_boundary_constraints_root(&mut self, root: Link<Op>) {
        if !self.boundary_constraints_roots.borrow().contains(&root) {
            self.boundary_constraints_roots
                .borrow_mut()
                .push(root.clone());
        }
    }

    pub fn remove_boundary_constraints_root(&mut self, root: Link<Op>) {
        self.boundary_constraints_roots
            .borrow_mut()
            .retain(|n| *n != root);
    }

    pub fn insert_integrity_constraints_root(&mut self, root: Link<Op>) {
        if !self.integrity_constraints_roots.borrow().contains(&root) {
            self.integrity_constraints_roots
                .borrow_mut()
                .push(root.clone());
        }
    }

    pub fn remove_integrity_constraints_root(&mut self, root: Link<Op>) {
        self.boundary_constraints_roots
            .borrow_mut()
            .retain(|n| *n != root);
    }
}
