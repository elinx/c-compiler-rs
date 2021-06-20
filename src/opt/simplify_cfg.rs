use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::Deref;

use crate::ir::*;
use crate::opt::FunctionPass;
use crate::*;

pub type SimplifyCfg = FunctionPass<
    Repeat<(
        SimplifyCfgConstProp,
        (SimplifyCfgReach, (SimplifyCfgMerge, SimplifyCfgEmpty)),
    )>,
>;

/// Simplifies block exits by propagating constants.
#[derive(Default)]
pub struct SimplifyCfgConstProp {}

/// Retains only those blocks that are reachable from the init.
#[derive(Default)]
pub struct SimplifyCfgReach {}

/// Merges two blocks if a block is pointed to only by another
#[derive(Default)]
pub struct SimplifyCfgMerge {}

/// Removes empty blocks
#[derive(Default)]
pub struct SimplifyCfgEmpty {}

impl SimplifyCfgConstProp {
    fn simplify_block_exit(&self, exit: &BlockExit) -> Option<BlockExit> {
        match exit {
            BlockExit::ConditionalJump {
                condition,
                arg_then,
                arg_else,
            } => {
                if arg_then == arg_else {
                    return Some(BlockExit::Jump {
                        arg: arg_then.deref().clone(),
                    });
                }
                if let Some(c) = condition.get_constant() {
                    match c {
                        Constant::Int { value: 0, .. } => {
                            return Some(BlockExit::Jump {
                                arg: arg_else.deref().clone(),
                            });
                        }
                        Constant::Int { value: 1, .. } => {
                            return Some(BlockExit::Jump {
                                arg: arg_then.deref().clone(),
                            });
                        }
                        _ => {}
                    }
                }
                None
            }
            BlockExit::Switch {
                value,
                default,
                cases,
            } => {
                if cases.iter().all(|(_, jump_to)| default.deref() == jump_to) {
                    return Some(BlockExit::Jump {
                        arg: default.deref().clone(),
                    });
                }
                if let Some(v) = value.get_constant() {
                    return Some(BlockExit::Jump {
                        arg: if let Some((_, arg)) = cases.iter().find(|(c, _)| v == c) {
                            arg.clone()
                        } else {
                            default.deref().clone()
                        },
                    });
                }
                None
            }
            _ => None,
        }
    }
}

impl Optimize<FunctionDefinition> for SimplifyCfgConstProp {
    fn optimize(&mut self, code: &mut FunctionDefinition) -> bool {
        code.blocks
            .iter_mut()
            .map(|(_, block)| {
                if let Some(exit) = self.simplify_block_exit(&block.exit) {
                    block.exit = exit;
                    true
                } else {
                    false
                }
            })
            .any(|r| r)
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
    graph
}

impl Optimize<FunctionDefinition> for SimplifyCfgReach {
    fn optimize(&mut self, code: &mut FunctionDefinition) -> bool {
        let graph = make_graph(code);
        let mut queue = vec![code.bid_init];
        let mut visited = HashSet::new();
        visited.insert(code.bid_init);
        while let Some(bid) = queue.pop() {
            if let Some(adjs) = graph.get(&bid) {
                adjs.iter().for_each(|bid| {
                    if visited.insert(*bid) {
                        queue.push(*bid);
                    }
                });
            }
        }
        let orig_blocks_num = code.blocks.len();
        graph
            .keys()
            .filter(|bid| !visited.contains(bid))
            .for_each(|bid| {
                code.blocks.remove(bid);
            });
        orig_blocks_num != visited.len()
    }
}

impl Optimize<FunctionDefinition> for SimplifyCfgMerge {
    fn optimize(&mut self, code: &mut FunctionDefinition) -> bool {
        let graph = make_graph(code);

        let mut in_degrees = HashMap::new();
        for (_, blocks) in graph {
            for bid_out in blocks {
                *in_degrees.entry(bid_out).or_insert(0) += 1;
            }
        }

        for (bid_from, block_from) in
            unsafe { &mut *(&mut code.blocks as *mut BTreeMap<BlockId, Block>) }
        {
            if let BlockExit::Jump { arg } = &block_from.exit {
                if *bid_from != arg.bid && in_degrees.get(&bid_from).eq(&Some(&1)) {
                    let bid_to = arg.bid;
                    let _block_to = code.blocks.remove(&bid_to).expect("remove block");
                    let _args_to = arg.args.clone();
                }
            }
        }
        false
    }
}

impl Optimize<FunctionDefinition> for SimplifyCfgEmpty {
    fn optimize(&mut self, _code: &mut FunctionDefinition) -> bool {
        todo!("homework 3")
    }
}
