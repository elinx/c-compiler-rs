use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::usize;

use crate::ir::*;
use crate::opt::FunctionPass;
use crate::*;

pub type Mem2reg = FunctionPass<Mem2regInner>;

#[derive(Default)]
pub struct Mem2regInner {}

#[allow(dead_code)]
fn do_post_order(
    bid: BlockId,
    graph: &HashMap<BlockId, Vec<BlockId>>,
    visited: &mut HashSet<BlockId>,
    res: &mut Vec<BlockId>,
) {
    visited.insert(bid);
    if let Some(adjs) = graph.get(&bid) {
        adjs.iter().for_each(|adj| {
            if visited.insert(*adj) {
                do_post_order(*adj, graph, visited, res);
            }
        })
    }
    res.push(bid);
}

#[allow(dead_code)]
fn reverse_postorder(bid_init: BlockId, graph: &HashMap<BlockId, Vec<BlockId>>) -> Vec<BlockId> {
    let mut res = Vec::new();
    let mut visited = HashSet::new();
    do_post_order(bid_init, graph, &mut visited, &mut res);
    res
}

#[derive(Debug)]
pub struct DomTree {
    idoms: HashMap<BlockId, BlockId>,
    frontiers: HashMap<BlockId, Vec<BlockId>>,
    reverse_post_order: Vec<BlockId>,
}

impl DomTree {
    pub fn new(
        bid_init: BlockId,
        cfg: &HashMap<BlockId, Vec<BlockId>>,
        reverse_cfg: &HashMap<BlockId, Vec<BlockId>>,
    ) -> Self {
        let mut reverse_post_order = reverse_postorder(bid_init, cfg);
        reverse_post_order.reverse();
        let inverse_reverse_post_order = reverse_post_order
            .iter()
            .enumerate()
            .map(|(i, bid)| (*bid, i))
            .collect();
        let mut idoms = HashMap::<BlockId, BlockId>::new();
        loop {
            let mut changed = false;
            for bid in &reverse_post_order {
                if *bid == bid_init {
                    continue;
                }
                let mut idom = None;
                for bid_prev in reverse_cfg.get(bid).unwrap() {
                    if *bid_prev != bid_init || idoms.get(bid_prev).is_some() {
                        idom = Some(DomTree::intersect(
                            idom,
                            *bid_prev,
                            &inverse_reverse_post_order,
                            &idoms,
                        ));
                    }
                }
                if let Some(idom) = idom {
                    idoms
                        .entry(*bid)
                        .and_modify(|v| {
                            if *v != idom {
                                changed = true;
                                *v = idom;
                            }
                        })
                        .or_insert_with(|| {
                            changed = true;
                            idom
                        });
                }
            }
            if !changed {
                break;
            }
        }

        let mut frontiers = HashMap::new();
        for (bid, prevs) in cfg {
            // only consider joinable nodes
            if prevs.len() <= 1 {
                continue;
            }
            for bid_prev in prevs {
                let mut runner = *bid_prev;
                // enter while body only if `runner` is NOT the idom of `bid`
                while !Self::dominates(&idoms, runner, *bid) {
                    frontiers.entry(runner).or_insert_with(Vec::new).push(*bid);
                    runner = *idoms.get(&runner).unwrap();
                }
            }
        }
        Self {
            idoms,
            frontiers,
            reverse_post_order,
        }
    }

    fn dominates(idoms: &HashMap<BlockId, BlockId>, runner: BlockId, bid: BlockId) -> bool {
        *idoms.get(&bid).unwrap() == runner
    }

    fn intersect(
        lhs: Option<BlockId>,
        mut rhs: BlockId,
        inverse_reverse_post_order: &HashMap<BlockId, usize>,
        idoms: &HashMap<BlockId, BlockId>,
    ) -> BlockId {
        if let Some(mut lhs) = lhs {
            loop {
                if lhs == rhs {
                    return lhs;
                }
                let lhs_index = inverse_reverse_post_order.get(&lhs).unwrap();
                let rhs_index = inverse_reverse_post_order.get(&rhs).unwrap();
                match lhs_index.cmp(rhs_index) {
                    Ordering::Less => rhs = *idoms.get(&rhs).unwrap(),
                    Ordering::Greater => lhs = *idoms.get(&lhs).unwrap(),
                    Ordering::Equal => panic!("lhs == rhs should not happend"),
                }
            }
        }
        rhs
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum OperandVar {
    Operand(Operand),
    Phi((usize, BlockId)),
}

impl OperandVar {
    pub fn lookup(
        &self,
        _dtype: &Dtype,
        _phinode_indices: &HashMap<(usize, BlockId), usize>,
    ) -> Operand {
        match self {
            OperandVar::Operand(_) => todo!(),
            OperandVar::Phi(_key) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
struct JoinTable<'s> {
    inner: HashMap<(usize, BlockId), BlockId>,
    dom_tree: &'s DomTree,
    joins: &'s HashSet<(usize, BlockId)>,
}

impl<'s> JoinTable<'s> {
    pub fn new(dom_tree: &'s DomTree, joins: &'s HashSet<(usize, BlockId)>) -> Self {
        Self {
            inner: HashMap::new(),
            dom_tree,
            joins,
        }
    }

    pub fn lookup(&mut self, aid: usize, mut bid: BlockId) -> BlockId {
        let mut bids = Vec::new();
        let ret = loop {
            if let Some(ret) = self.inner.get(&(aid, bid)) {
                break *ret;
            }
            bids.push(bid);
            if self.joins.contains(&(aid, bid)) {
                break bid;
            }
            if let Some(idom) = self.dom_tree.idoms.get(&bid) {
                bid = *idom;
            } else {
                break bid;
            }
        };
        for bid in bids {
            self.inner.insert((aid, bid), ret);
        }
        ret
    }
}

fn make_graph(func: &FunctionDefinition) -> HashMap<BlockId, Vec<BlockId>> {
    let mut graph = HashMap::new();
    func.blocks.iter().for_each(|(block_id, block)| {
        let list = match &block.exit {
            BlockExit::Jump { arg } => vec![arg.bid],
            BlockExit::ConditionalJump {
                arg_then, arg_else, ..
            } => vec![arg_then.deref().bid, arg_else.deref().bid],
            BlockExit::Switch { default, cases, .. } => {
                let mut bids = Vec::new();
                bids.push(default.deref().bid);
                cases.iter().for_each(|(_, exit)| bids.push(exit.bid));
                bids
            }
            _ => vec![],
        };
        graph.insert(*block_id, list);
    });
    // println!("graph: {:?}", graph);
    graph
}

fn reverse_cfg(graph: &HashMap<BlockId, Vec<BlockId>>) -> HashMap<BlockId, Vec<BlockId>> {
    let mut reverse = HashMap::new();
    graph.iter().for_each(|(block_from, blocks_to)| {
        for block_to in blocks_to {
            // reverse.insert(*block_to, *block_from);
            reverse
                .entry(*block_to)
                .or_insert_with(|| vec![])
                .push(*block_from);
        }
    });
    reverse
}

fn set_inpromotable_operand(inpromotable: &mut HashSet<usize>, operand: &Operand) {
    match operand {
        Operand::Constant(_) => {}
        Operand::Register { rid, .. } => match rid {
            RegisterId::Local { aid } => {
                inpromotable.insert(*aid);
            }
            _ => {}
        },
    }
}

fn get_promotable_operands(
    code: &FunctionDefinition,
) -> (HashSet<usize>, HashMap<usize, Vec<BlockId>>) {
    // for a local allocation, only it's def-use places are promotable which
    // means the `ptr` operand of `Load` instruction and `ptr` operand of
    // `Store` instruction are promotable.
    let mut inpromotable = HashSet::new();
    let mut stores = HashMap::new();
    for (bid, block) in &code.blocks {
        for instr in &block.instructions {
            match instr.deref() {
                Instruction::Nop => {}
                Instruction::BinOp { lhs, rhs, .. } => {
                    set_inpromotable_operand(&mut inpromotable, lhs);
                    set_inpromotable_operand(&mut inpromotable, rhs);
                }
                Instruction::UnaryOp { operand, .. } => {
                    set_inpromotable_operand(&mut inpromotable, operand);
                }
                Instruction::Store { ptr, value } => {
                    set_inpromotable_operand(&mut inpromotable, value);
                    if let Some((rid, _)) = ptr.get_register() {
                        if let RegisterId::Local { aid } = rid {
                            stores.entry(*aid).or_insert_with(Vec::new).push(*bid);
                        }
                    }
                }
                Instruction::Load { .. } => {}
                Instruction::Call { callee, args, .. } => {
                    set_inpromotable_operand(&mut inpromotable, callee);
                    for arg in args {
                        set_inpromotable_operand(&mut inpromotable, arg);
                    }
                }
                Instruction::TypeCast { value, .. } => {
                    set_inpromotable_operand(&mut inpromotable, value);
                }
                Instruction::GetElementPtr { .. } => todo!(),
            }
        }
    }
    (inpromotable, stores)
}

impl Optimize<FunctionDefinition> for Mem2regInner {
    fn optimize(&mut self, code: &mut FunctionDefinition) -> bool {
        println!("===== function =====");
        let (inpromotable, stores) = get_promotable_operands(code);
        // all allocations are not promotable then return directly
        if code
            .allocations
            .iter()
            .enumerate()
            .all(|(aid, _)| inpromotable.contains(&aid))
        {
            return false;
        }
        dbg!(&inpromotable);
        dbg!(&stores);

        let graph = make_graph(code);
        let reverse_cfg = reverse_cfg(&graph);
        let dom_tree = DomTree::new(code.bid_init, &graph, &reverse_cfg);
        dbg!(&graph);
        dbg!(&reverse_cfg);
        dbg!(&dom_tree);

        let joins_map: HashMap<usize, HashSet<BlockId>> = stores
            .iter()
            .filter(|(aid, _)| !inpromotable.contains(*aid))
            .map(|(aid, bids)| {
                (*aid, {
                    let mut stack = bids.clone();
                    let mut visited = HashSet::new();
                    while let Some(bid) = stack.pop() {
                        if let Some(bid_frontiers) = dom_tree.frontiers.get(&bid) {
                            for bid_frontier in bid_frontiers {
                                if visited.insert(*bid_frontier) {
                                    stack.push(*bid_frontier);
                                }
                            }
                        }
                    }
                    visited
                })
            })
            .collect();
        let mut joins = HashSet::new();
        for (aid, bids) in joins_map {
            for bid in bids {
                joins.insert((aid, bid));
            }
        }
        let mut join_table = JoinTable::new(&dom_tree, &joins);
        dbg!(&join_table);

        let mut replaces = HashMap::<RegisterId, OperandVar>::new();
        let mut phinode_indices = HashSet::<(usize, BlockId)>::new();
        let mut end_values = HashMap::<(usize, BlockId), OperandVar>::new();
        for (bid, block) in &code.blocks {
            for (i, instr) in block.instructions.iter().enumerate() {
                match instr.deref() {
                    Instruction::Store { ptr, value } => {
                        if let Some((rid, _)) = ptr.get_register() {
                            if let RegisterId::Local { aid } = rid {
                                if inpromotable.contains(aid) {
                                    continue;
                                }
                                end_values.insert((*aid, *bid), OperandVar::Operand(value.clone()));
                            }
                        }
                    }
                    Instruction::Load { ptr } => {
                        if let Some((rid, _)) = ptr.get_register() {
                            if let RegisterId::Local { aid } = rid {
                                if inpromotable.contains(aid) {
                                    continue;
                                }
                                let bid_join = join_table.lookup(*aid, *bid);
                                let end_value_join = end_values.get(&(*aid, *bid)).cloned();
                                let var = end_values.entry((*aid, *bid)).or_insert_with(|| {
                                    end_value_join.unwrap_or_else(|| {
                                        phinode_indices.insert((*aid, bid_join));
                                        OperandVar::Phi((*aid, bid_join))
                                    })
                                });
                                let result =
                                    replaces.insert(RegisterId::temp(*bid, i), var.clone());
                                assert_eq!(result, None);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // generates phinodes recursively
        let mut phinode_visited = phinode_indices;
        let mut phinode_stack = phinode_visited.iter().cloned().collect::<Vec<_>>();
        let mut phinodes =
            BTreeMap::<(usize, BlockId), (Dtype, HashMap<BlockId, OperandVar>)>::new();
        while let Some((aid, bid)) = phinode_stack.pop() {
            let mut cases = HashMap::new();
            if let Some(prevs) = reverse_cfg.get(&bid) {
                for bid_prev in prevs {
                    let var_prev = (aid, *bid_prev);
                    let end_value = end_values.get(&var_prev).cloned().unwrap_or_else(|| {
                        let bid_prev_phinode = join_table.lookup(aid, *bid_prev);
                        let var_prev_phinode = (aid, bid_prev_phinode);
                        if phinode_visited.insert(var_prev_phinode) {
                            phinode_stack.push(var_prev_phinode);
                        }
                        OperandVar::Phi(var_prev_phinode)
                    });
                    cases.insert(*bid_prev, end_value);
                }
                phinodes.insert(
                    (aid, bid),
                    (code.allocations.get(aid).unwrap().deref().clone(), cases),
                );
            }
        }

        // insert phinodes
        let mut phinode_indices = HashMap::<(usize, BlockId), usize>::new();
        for ((aid, bid), (dtype, _)) in &phinodes {
            let name = code.allocations.get(*aid).unwrap().name();
            let block = code.blocks.get_mut(bid).unwrap();
            let index = block.phinodes.len();
            block
                .phinodes
                .push(Named::new(name.cloned(), dtype.clone()));
            phinode_indices.insert((*aid, *bid), index);
        }

        // insert phinode arguments
        for ((aid, bid), (dtype, phinode)) in &phinodes {
            let index = *phinode_indices.get(&(*aid, *bid)).unwrap();
            for (bid_prev, operand_prev) in phinode {
                let block_prev = code.blocks.get_mut(bid_prev).unwrap();
                let operand_prev = operand_prev.lookup(dtype, &phinode_indices);
                block_prev.exit.walk_jump_args(|arg| {
                    if &arg.bid == bid {
                        assert_eq!(arg.args.len(), index);
                        arg.args.push(operand_prev.clone());
                    }
                });
            }
        }

        // replace the values loaded from promotable allocations
        for (_, block) in &mut code.blocks {
            block.walk(|operand| {
                if let Some((rid, dtype)) = operand.get_register() {
                    if let Some(operand_var) = replaces.get(&rid) {
                        return operand_var.lookup(dtype, &phinode_indices);
                    }
                }
                operand.clone()
            });
        }

        // remove load/store instructions
        for block in code.blocks.values_mut() {
            for instr in block.instructions.iter_mut() {
                match instr.deref().deref() {
                    Instruction::Store { ptr, .. } | Instruction::Load { ptr } => {
                        if let Some((rid, _)) = ptr.get_register() {
                            if let RegisterId::Local { aid } = rid {
                                if !inpromotable.contains(aid) {
                                    *instr.deref_mut() = Instruction::Nop;
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        true
    }
}
