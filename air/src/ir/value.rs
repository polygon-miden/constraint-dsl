use super::*;

/// Represents a scalar value in the [AlgebraicGraph]
///
/// Values are either constant, or evaluated at runtime using the context
/// provided to an AirScript program (i.e. random values, public inputs, etc.).
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Value {
    /// A constant value.
    Constant(u64),
    /// A reference to a specific column in the trace segment, with an optional offset.
    TraceAccess(TraceAccess), // first, respect order
    /// A reference to a periodic column
    ///
    /// The value this corresponds to is determined by the current row of the trace.
    PeriodicColumn(PeriodicColumnAccess),
    /// A reference to a specific element of a given public input
    PublicInput(PublicInputAccess), // TODO example for boundary contraint, second
    /// A reference to the `random_values` array, specifically the element at the given index
    RandomValue(usize), //only in the aux, third, not the alpha
} // alpha 4th //z TODO // constant 5th after all the reads

/// Represents an access of a [PeriodicColumn], similar in nature to [TraceAccess]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PeriodicColumnAccess {
    pub name: QualifiedIdentifier,
    pub cycle: usize,
}
impl PeriodicColumnAccess {
    pub const fn new(name: QualifiedIdentifier, cycle: usize) -> Self {
        Self { name, cycle }
    }
}

/// Represents an access of a [PublicInput], similar in nature to [TraceAccess]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PublicInputAccess {
    /// The name of the public input to access
    pub name: Identifier,
    /// The index of the element in the public input to access
    pub index: usize,
}
impl PublicInputAccess {
    pub const fn new(name: Identifier, index: usize) -> Self {
        Self { name, index }
    }
}
