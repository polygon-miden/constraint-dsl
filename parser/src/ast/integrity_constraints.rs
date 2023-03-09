use super::{EvaluatorFunctionCall, Expression, Variable};

// INTEGRITY STATEMENTS
// ================================================================================================

#[derive(Debug, Eq, PartialEq)]
pub enum IntegrityStmt {
    Constraint(IntegrityConstraint),
    ConstraintWithSelector(IntegrityConstraint, Expression),
    EvaluatorFunctionCall(EvaluatorFunctionCall),
    EvaluatorFunctionCallWithSelector(EvaluatorFunctionCall, Expression),
    Variable(Variable),
}

/// Stores the expression corresponding to the integrity constraint.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IntegrityConstraint {
    lhs: Expression,
    rhs: Expression,
}

impl IntegrityConstraint {
    /// Creates a new integrity constraint.
    pub fn new(lhs: Expression, rhs: Expression) -> Self {
        Self { lhs, rhs }
    }

    /// Returns the left-hand side of the integrity constraint.
    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    /// Returns the right-hand side of the integrity constraint.
    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}
