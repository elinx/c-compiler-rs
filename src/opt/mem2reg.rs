use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;

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

        let graph = make_graph(code);
        let reverse_cfg = reverse_cfg(&graph);
        let dom_tree = DomTree::new(code.bid_init, &graph, &reverse_cfg);
        dbg!(&inpromotable);
        dbg!(&stores);
        dbg!(&graph);
        dbg!(&reverse_cfg);
        dbg!(&dom_tree);
        true
    }
}
