use air_ir::{Air, AlgebraicGraph, ConstraintDomain, NodeIndex, Operation, TraceAccess, Value};
use std::cmp::Ordering;
use std::collections::btree_map::BTreeMap;

/// A row in the table is an operation with its output index
struct Row(NodeIndex, Operation);
struct Table(Vec<Row>);
impl Table {
    fn append(&mut self, op: Operation) -> NodeIndex {
        let out: NodeIndex = self.0.len().into();
        self.0.push(Row(out, op));
        out
    }
}

/// Dot printer, display on https://dreampuf.github.io/GraphvizOnline/
/// or using dot -Tsvg ace.dot > ace.svg

fn display_value(v: &Value) -> String {
    match v {
        Value::Constant(c) => format!("C {}", c),
        Value::PeriodicColumn(_) => "Per".to_string(),
        Value::PublicInput(acc) => format!("PI {} {}", acc.name, acc.index),
        Value::RandomValue(v) => format!("Random {}", v),
        Value::TraceAccess(acc) => {
            format!("Trace {} {} {}", acc.segment, acc.column, acc.row_offset)
        }
    }
}

impl std::fmt::Debug for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out: usize = self.0.into();
        match &self.1 {
            Operation::Value(v) => {
                writeln!(f, "{} [label=\"{}\"]", out, display_value(v))
            }
            Operation::Add(l, r) => {
                let l: usize = NodeIndex::into(*l);
                let r: usize = NodeIndex::into(*r);
                writeln!(f, "{} [label=\"add\"]", out)?;
                writeln!(f, "{} -> {} [label=\"{}\"]", out, l, l)?;
                writeln!(f, "{} -> {} [label=\"{}\"]", out, r, r)
            }
            Operation::Sub(l, r) => {
                let l: usize = NodeIndex::into(*l);
                let r: usize = NodeIndex::into(*r);
                writeln!(f, "{} [label=\"sub\"]", out)?;
                writeln!(f, "{} -> {} [label=\"{}\"]", out, l, l)?;
                writeln!(f, "{} -> {} [label=\"{}\"]", out, r, r)
            }
            Operation::Mul(l, r) => {
                let l: usize = NodeIndex::into(*l);
                let r: usize = NodeIndex::into(*r);
                writeln!(f, "{} [label=\"mul\"]", out)?;
                writeln!(f, "{} -> {} [label=\"{}\"]", out, l, l)?;
                writeln!(f, "{} -> {} [label=\"{}\"]", out, r, r)
            }
        }
    }
}

impl std::fmt::Debug for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph G {{")?;
        for row in self.0.iter() {
            write!(f, "{:?}", row)?;
        }
        writeln!(f, "}}")
    }
}

/// Compare functions used to sort the operations as needed by the ace chiplet.

// first offset, then column
fn compare_access(a: &TraceAccess, b: &TraceAccess) -> Ordering {
    let segment = a.segment.cmp(&b.segment);
    if segment == Ordering::Equal {
        let row = a.row_offset.cmp(&b.row_offset);
        if row == Ordering::Equal {
            a.column.cmp(&b.column)
        } else {
            row
        }
    } else {
        segment
    }
}

fn value_order(a: &Value) -> usize {
    match a {
        Value::TraceAccess(_) => 0,
        Value::PublicInput(_) => 1,
        Value::PeriodicColumn(_) => unreachable!(""),
        Value::RandomValue(_) => 2,
        Value::Constant(_) => 3,
    }
}

fn compare_value(a: &Value, b: &Value) -> Ordering {
    let cmp = value_order(a).cmp(&value_order(b));
    if cmp == Ordering::Equal {
        match (a, b) {
            (Value::Constant(a), Value::Constant(b)) => a.cmp(b),
            (Value::PeriodicColumn(_), Value::PeriodicColumn(_)) => unreachable!(""),
            (Value::PublicInput(a), Value::PublicInput(b)) => a.index.cmp(&b.index),
            (Value::RandomValue(a), Value::RandomValue(b)) => a.cmp(b),
            (Value::TraceAccess(a), Value::TraceAccess(b)) => compare_access(a, b),
            _ => unreachable!(""),
        }
    } else {
        cmp
    }
}

// We just require values, which are leaves in the graph, to be first.
fn value_operation(a: &Operation) -> usize {
    match a {
        Operation::Value(_) => 0,
        Operation::Add(_, _) | Operation::Sub(_, _) | Operation::Mul(_, _) => 1,
    }
}

fn compare_operation(a: &Operation, b: &Operation) -> Ordering {
    let cmp = value_operation(a).cmp(&value_operation(b));
    if cmp == Ordering::Equal {
        match (a, b) {
            (Operation::Value(a), Operation::Value(b)) => compare_value(a, b),
            _ => Ordering::Equal,
        }
    } else {
        cmp
    }
}

// TODO remove kind
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Kind {
    Leaf = 0,
    Node = 1,
    IRoot = 2,
    BFRoot = 3,
    BLRoot = 4,
}

/// Iterate over all the nodes, in pre-order.
/// Properties of the result:
/// - parents come after their children
/// - all nodes are returned
/// - no duplicates (e.g. when two contraints share a subexpression)
pub struct SubtreeIterator<'a> {
    graph: &'a AlgebraicGraph,
    visited: Vec<bool>,
    unvisited: Vec<(Kind, NodeIndex)>,
}
impl<'a> SubtreeIterator<'a> {
    pub fn new(ir: &'a Air) -> Self {
        let graph = ir.constraint_graph();
        let n_segments = ir.trace_segment_widths.len();
        let mut unvisited = vec![];
        for seg in 0..n_segments {
            for root in ir.integrity_constraints(seg) {
                unvisited.push((Kind::IRoot, *root.node_index()));
            }
        }
        for seg in 0..n_segments {
            for root in ir.boundary_constraints(seg) {
                let kind = match root.domain() {
                    ConstraintDomain::FirstRow => Kind::BFRoot,
                    ConstraintDomain::LastRow => Kind::BLRoot,
                    _ => unreachable!(""),
                };
                unvisited.push((kind, *root.node_index()));
            }
        }
        let visited = vec![false; graph.num_nodes()];
        SubtreeIterator {
            graph,
            visited,
            unvisited,
        }
    }
}
impl Iterator for SubtreeIterator<'_> {
    type Item = (Kind, NodeIndex);
    fn next(&mut self) -> Option<Self::Item> {
        use Operation::*;

        while let Some((kind, id)) = self.unvisited.pop() {
            if self.visited[Into::<usize>::into(id)] {
                return Some((kind, id));
            } else {
                self.visited[Into::<usize>::into(id)] = true;
                match self.graph.node(&id).op() {
                    Value(_) => {
                        return Some((Kind::Leaf, id));
                    }
                    Add(l, r) | Sub(l, r) | Mul(l, r) => {
                        self.unvisited.push((kind, id));
                        if !self.visited[Into::<usize>::into(*r)] {
                            self.unvisited.push((Kind::Node, *r))
                        };
                        if !self.visited[Into::<usize>::into(*l)] {
                            self.unvisited.push((Kind::Node, *l))
                        };
                    }
                };
            }
        }
        None
    }
}

struct Backend<'ast> {
    ir: &'ast Air,
    table: Table,
    mapping: BTreeMap<NodeIndex, NodeIndex>,
    one: Option<NodeIndex>,
    alpha: Option<NodeIndex>,
    last_alpha: Option<NodeIndex>,
}

impl<'ast> Backend<'ast> {
    fn new(ir: &'ast Air) -> Self {
        Self {
            ir,
            table: Table(vec![]),
            mapping: BTreeMap::new(),
            one: None,
            alpha: None,
            last_alpha: None,
        }
    }

    fn add(&mut self, l: NodeIndex, r: NodeIndex) -> NodeIndex {
        self.table.append(Operation::Add(l, r))
    }
    fn mul(&mut self, l: NodeIndex, r: NodeIndex) -> NodeIndex {
        let one = self.one.unwrap();
        if l == one {
            r
        } else if r == one {
            l
        } else {
            self.table.append(Operation::Mul(l, r))
        }
    }
    fn sub(&mut self, l: NodeIndex, r: NodeIndex) -> NodeIndex {
        self.table.append(Operation::Sub(l, r))
    }
    fn val(&mut self, v: Value) -> NodeIndex {
        self.table.append(Operation::Value(v))
    }

    // \sum_i vec[i]
    fn sum(&mut self, vec: Vec<NodeIndex>) -> NodeIndex {
        assert!(!vec.is_empty());
        vec.iter()
            .enumerate()
            .fold(NodeIndex::default(), |acc, (i, r)| {
                if i == 0 {
                    // we ignore the first acc
                    *r
                } else {
                    self.add(acc, *r)
                }
            })
    }

    // \prod_i vec[i]
    fn prod(&mut self, vec: Vec<NodeIndex>) -> NodeIndex {
        assert!(!vec.is_empty());
        vec.iter()
            .enumerate()
            .fold(NodeIndex::default(), |acc, (i, r)| {
                if i == 0 {
                    // we ignore the first acc
                    *r
                } else {
                    self.mul(acc, *r)
                }
            })
    }

    // fn double_and_add(&mut self, one: u32, z_point: u32, n: u32) -> u32 {
    //     // Double and add
    //     // fn exp(z:u32,n:u32) -> u32 {
    //     //     let mut tmp = z;
    //     //     let mut res = 1;
    //     //     for i in 0..(32-n.leading_zeros()) {
    //     //         if n & (1 << i) != 0 {res *= tmp}
    //     //         tmp *= tmp;
    //     //     }
    //     //     res
    //     // }

    //     let mut tmp = z_point;
    //     let mut res = one;
    //     for i in 0..(32 - n.leading_zeros()) {
    //         if n & (1 << i) != 0 {
    //             if res == one {
    //                 res = tmp; // avoid the first multiplication by one
    //             } else {
    //                 res = self.table.append(Op::Binary(BinOp::Mul, res, tmp));
    //             }
    //         }
    //         tmp = self.table.append(Op::Binary(BinOp::Mul, tmp, tmp));
    //     }
    //     // remove the last tmp if unused
    //     let last_idx = self.table.0.len() - 1;
    //     let Row(out, _) = self.table.0[last_idx];
    //     if out == tmp {
    //         self.table.0.remove(last_idx);
    //     };
    //     res
    // }

    /// Insert the operation in the table after remapping its indexes
    fn row_of_op(&mut self, id: NodeIndex) -> NodeIndex {
        let op = self.ir.constraints.graph().node(&id).op();
        let out = match op {
            Operation::Value(v) => self.val(*v),
            Operation::Add(l, r) => self.add(self.mapping[l], self.mapping[r]),
            Operation::Sub(l, r) => self.sub(self.mapping[l], self.mapping[r]),
            Operation::Mul(l, r) => self.mul(self.mapping[l], self.mapping[r]),
        };
        self.mapping.insert(id, out);
        out
    }

    /// Generate the next power of alpha
    fn next_alpha(&mut self) -> NodeIndex {
        let alpha = self.alpha.unwrap();
        let next_alpha = match self.last_alpha {
            Some(last_alpha) => self.mul(alpha, last_alpha),
            None => self.one.unwrap(),
        };
        self.last_alpha = Some(next_alpha);
        next_alpha
    }

    /// \sum_i alpha^(offset+i) vec[i]
    /// where offset is the last power of alpha that was generated
    fn linear_combination_alpha(&mut self, vec: Vec<NodeIndex>) -> NodeIndex {
        assert!(!vec.is_empty());
        vec.iter()
            .enumerate()
            .fold(NodeIndex::default(), |acc, (i, r)| {
                let alpha = self.next_alpha();
                let root_alpha = self.mul(*r, alpha);
                if i == 0 {
                    // we ignore the first acc
                    root_alpha
                } else {
                    self.add(acc, root_alpha)
                }
            })
    }

    // int * z_g / zn_1 + bf/z_1 + bl/z_g
    // int * z_g^2 * z_1 + bf * zn_1 * z_g + bl * zn_1 * z_1 / (zn_1 * z_1 * z_g)
    fn generate(mut self) -> anyhow::Result<String> {
        // Nodes are collected in a vector in post-order and sorted
        let mut nodes = SubtreeIterator::new(self.ir).collect::<Vec<(Kind, NodeIndex)>>();
        nodes.sort_by(|(_, a), (_, b)| {
            let a = self.ir.constraints.graph().node(a).op();
            let b = self.ir.constraints.graph().node(b).op();
            compare_operation(a, b)
        });

        // The table is populated with all values except constants
        nodes
            .iter()
            .filter(|(_k, id)| {
                let op = self.ir.constraints.graph().node(id).op();
                matches!(
                    op,
                    Operation::Value(Value::TraceAccess(_))
                        | Operation::Value(Value::PublicInput(_))
                        | Operation::Value(Value::RandomValue(_))
                )
            })
            .for_each(|id| {
                self.row_of_op(id.1);
            });

        // A few additional leaves are inserted that are later used to join the constraint roots
        self.alpha = Some(self.val(Value::Constant(10)));
        let z_point = self.val(Value::Constant(11));
        let z_n = self.val(Value::Constant(12));
        let inverse_generator = self.val(Value::Constant(13));

        // The table is populated with the constants, if a `1` is not found, one is inserted.
        // Its index is noted.
        nodes
            .iter()
            .filter(|(_k, id)| {
                matches!(
                    self.ir.constraints.graph().node(id).op(),
                    Operation::Value(Value::Constant(_))
                )
            })
            .for_each(|id| {
                let out = self.row_of_op(id.1);
                if matches!(
                    self.ir.constraints.graph().node(&id.1).op(),
                    Operation::Value(Value::Constant(1))
                ) {
                    self.one = Some(out)
                }
            });
        if self.one.is_none() {
            let one = self.val(Value::Constant(1));
            self.one = Some(one);
        }

        // The table is populated with all the nodes and then roots.
        nodes
            .iter()
            .filter(|(k, _)| *k == Kind::Node)
            .for_each(|id| {
                self.row_of_op(id.1);
            });
        let integrity_roots = nodes
            .iter()
            .filter(|(k, _)| *k == Kind::IRoot)
            .map(|id| self.row_of_op(id.1))
            .collect::<Vec<NodeIndex>>();
        let boundary_first_roots = nodes
            .iter()
            .filter(|(k, _)| *k == Kind::BFRoot)
            .map(|id| self.row_of_op(id.1))
            .collect::<Vec<NodeIndex>>();
        let boundary_last_roots = nodes
            .iter()
            .filter(|(k, _)| *k == Kind::BLRoot)
            .map(|id| self.row_of_op(id.1))
            .collect::<Vec<NodeIndex>>();

        // TODO these could be lazy
        let one = self.one.unwrap();
        let z_minus_g = self.sub(z_point, inverse_generator);
        let z_minus_one = self.sub(z_point, one);
        let z_n_minus_one = self.sub(z_n, one);

        let mut els = vec![];
        // int * z_g^2 * z_1
        if !integrity_roots.is_empty() {
            let lc = self.linear_combination_alpha(integrity_roots);
            let res = self.prod(vec![lc, z_minus_g, z_minus_g, z_minus_one]);
            els.push(res)
        };
        // bf * zn_1 * z_g
        if !boundary_first_roots.is_empty() {
            let lc = self.linear_combination_alpha(boundary_first_roots);
            let res = self.prod(vec![lc, z_n_minus_one, z_minus_g]);
            els.push(res)
        };
        // bl * zn_1 * z_1
        if !boundary_last_roots.is_empty() {
            let lc = self.linear_combination_alpha(boundary_last_roots);
            let res = self.prod(vec![lc, z_n_minus_one, z_minus_one]);
            els.push(res)
        };
        self.sum(els);

        Ok(format!("{:#?}", self.table))
    }
}

#[derive(Default)]
pub struct CodeGenerator {}

impl air_ir::CodeGenerator for CodeGenerator {
    type Output = String;

    fn generate(&self, ir: &Air) -> anyhow::Result<Self::Output> {
        Backend::new(ir).generate()
    }
}
