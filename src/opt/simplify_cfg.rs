use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};

use itertools::izip;

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

fn replace_operands(operand: &Operand, replaces: &HashMap<RegisterId, Operand>) -> Operand {
    match operand {
        Operand::Constant(_) => operand.clone(),
        Operand::Register { rid, .. } => {
            if let Some(operand) = replaces.get(rid) {
                operand.clone()
            } else {
                operand.clone()
            }
        }
    }
}

impl Block {
    fn walk<F>(&mut self, transform_operand: F)
    where
        F: Fn(&Operand) -> Operand,
    {
        for instr in &mut self.instructions {
            match &mut instr.deref_mut() {
                Instruction::BinOp { lhs, rhs, .. } => {
                    *lhs = transform_operand(lhs);
                    *rhs = transform_operand(rhs);
                }
                Instruction::UnaryOp { operand, .. } => *operand = transform_operand(operand),
                Instruction::Store { ptr, value } => {
                    *ptr = transform_operand(ptr);
                    *value = transform_operand(value);
                }
                Instruction::Load { ptr } => *ptr = transform_operand(ptr),
                Instruction::Call { callee, args, .. } => {
                    *callee = transform_operand(callee);
                    args.iter_mut()
                        .for_each(|arg| *arg = transform_operand(arg));
                }
                Instruction::TypeCast { value, .. } => *value = transform_operand(value),
                Instruction::GetElementPtr { ptr, offset, .. } => {
                    *ptr = transform_operand(ptr);
                    *offset = transform_operand(offset);
                }
                _ => {}
            }
        }
        match &mut self.exit {
            // TODO: JumpArg apply transform
            // BlockExit::Jump { arg } => todo!(),
            // BlockExit::ConditionalJump {
            //     condition,
            //     arg_then,
            //     arg_else,
            // } => todo!(),
            // BlockExit::Switch {
            //     value,
            //     default,
            //     cases,
            // } => todo!(),
            BlockExit::Return { value } => *value = transform_operand(value),
            _ => {}
        }
    }
}

impl Optimize<FunctionDefinition> for SimplifyCfgMerge {
    fn optimize(&mut self, code: &mut FunctionDefinition) -> bool {
        let graph = make_graph(code);
        let mut result = false;

        let mut in_degrees = HashMap::new();
        for (_, blocks) in graph {
            for bid_out in blocks {
                *in_degrees.entry(bid_out).or_insert(0) += 1;
            }
        }

        // collects blocks to be removed
        let blocks_remove: HashSet<_> = code
            .blocks
            .iter()
            .map(|(bid_from, block_from)| {
                if let BlockExit::Jump { arg } = &block_from.exit {
                    let bid_to = arg.bid;
                    if *bid_from != bid_to && in_degrees.get(&bid_to).eq(&Some(&1)) {
                        result = true;
                        return Some(bid_to);
                    }
                }
                None
            })
            .filter(|x| x.is_some())
            .collect();
        println!("blocks_remove: {:?}", blocks_remove);

        // merge nodes
        for (bid_from, ref mut block_from) in &code.blocks {
            if let BlockExit::Jump { arg } = &block_from.exit {
                let bid_to = arg.bid;
                if blocks_remove.get(&Some(bid_to)).is_some() {
                    let args_to = arg.args.clone();
                    let block_to = code.blocks.get(&bid_to).expect("block_to");
                    let mut replaces = HashMap::new();

                    // mappings between phinode parameter and argument, note that the argument are
                    // all constants which means all operands using phinode could be substituted directly
                    for (i, (phi, arg)) in izip!(&block_to.phinodes, &args_to).enumerate() {
                        assert_eq!(phi.deref(), &arg.dtype());
                        let phi_param = RegisterId::arg(bid_to, i);
                        replaces.insert(phi_param, arg.clone());
                    }

                    // mappings between block_to and block_from, iid and bid are fixed(by adding a offset
                    // and bid replacement)
                    let instr_from_offset = block_from.instructions.len();
                    for (i, instr_to) in block_to.instructions.iter().enumerate() {
                        let from = RegisterId::temp(bid_to, i);
                        let to = Operand::register(
                            RegisterId::temp(*bid_from, i + instr_from_offset),
                            instr_to.dtype().clone(),
                        );
                        replaces.insert(from, to);
                        block_from.instructions.push(instr_to.clone());
                    }

                    // BlockExit instruction is not a part of instructions, should handle seperatedly
                    block_from.exit = block_to.exit.clone();

                    // replace un-fixed registers
                    block_from.walk(|operand| replace_operands(&operand, &replaces));
                    result = true;
                }
            }
        }

        /*
        for (bid_from, block_from) in &code.blocks {
            if let BlockExit::Jump { arg } = &block_from.exit {
                let bid_to = arg.bid;
                if *bid_from != bid_to && in_degrees.get(&bid_to).eq(&Some(&1)) {
                    // let block_to = code.blocks.remove(&bid_to).expect("remove block");
                    let block_to = code.blocks.get(&bid_to).expect("remove block");
                    // blocks_remove.push(bid_to.clone());
                    let args_to = arg.args.clone();
                    let mut replaces = HashMap::new();

                    // mappings between phinode parameter and argument, note that the argument are
                    // all constants which means all operands using phinode could be substituted directly
                    for (i, (phi, arg)) in izip!(&block_to.phinodes, &args_to).enumerate() {
                        assert_eq!(phi.deref(), &arg.dtype());
                        let phi_param = RegisterId::arg(bid_to, i);
                        replaces.insert(phi_param, arg.clone());
                    }

                    // mappings between block_to and block_from, iid and bid are fixed(by adding a offset
                    // and bid replacement)
                    let instr_from_offset = block_from.instructions.len();
                    for (i, instr_to) in block_to.instructions.iter().enumerate() {
                        let from = RegisterId::temp(bid_to, i);
                        let to = Operand::register(
                            RegisterId::temp(*bid_from, i + instr_from_offset),
                            instr_to.dtype().clone(),
                        );
                        replaces.insert(from, to);
                        block_from.instructions.push(instr_to.clone());
                    }

                    // BlockExit instruction is not a part of instructions, should handle seperatedly
                    block_from.exit = block_to.exit.clone();

                    // replace un-fixed registers
                    block_from.walk(|operand| replace_operands(&operand, &replaces));
                    // code.walk(|operand| replace_operands(&operand, &replaces));
                    result = true;
                }
            }
        }
        */
        // delete merged blocks
        // blocks_remove.into_iter().for_each(|bid| {
        //     code.blocks.remove(&bid).expect("remove block");
        // });
        result
    }
}

fn simplify_block_exit(exit: &mut BlockExit, blocks_empty: &HashMap<BlockId, Block>) -> bool {
    match exit {
        BlockExit::Jump { arg } => {
            if let Some(block) = blocks_empty.get(&arg.bid) {
                *exit = block.exit.clone();
                true
            } else {
                false
            }
        }
        BlockExit::ConditionalJump {
            arg_then, arg_else, ..
        } => {
            let l = simplify_jump_arg(arg_then, blocks_empty);
            let r = simplify_jump_arg(arg_else, blocks_empty);
            l || r
        }
        BlockExit::Switch { default, cases, .. } => {
            let d = simplify_jump_arg(default, blocks_empty);
            let cs = cases
                .iter_mut()
                .map(|c| simplify_jump_arg(&mut c.1, blocks_empty))
                .fold(false, |l, r| l || r);
            d || cs
        }
        _ => false,
    }
}

fn simplify_jump_arg(arg: &mut JumpArg, blocks_empty: &HashMap<BlockId, Block>) -> bool {
    if let Some(block) = blocks_empty.get(&arg.bid) {
        if let BlockExit::Jump { arg: a } = &block.exit {
            *arg = a.clone();
            true
        } else {
            false
        }
    } else {
        false
    }
}
impl Optimize<FunctionDefinition> for SimplifyCfgEmpty {
    fn optimize(&mut self, code: &mut FunctionDefinition) -> bool {
        let blocks_empty = code
            .blocks
            .iter()
            .filter(|(_, block)| block.phinodes.is_empty() && block.instructions.is_empty())
            .map(|(bid, block)| (*bid, block.clone()))
            .collect::<HashMap<_, _>>();
        // Fix: can't handle following case
        // .. -> B7 -> Be8 -> Be9 -> B10 -> ..
        // blocks_empty: Be8, Be9
        // iter1: B7 -> Be9
        // iter2: Be8 -> B10
        // iter3: Be9 -> B10
        // the end: B7 -> Be9 -> B10 -> ..
        // expect: B7 -> B10 -> ..
        code.blocks
            .iter_mut()
            .map(|(_, block)| simplify_block_exit(&mut block.exit, &blocks_empty))
            .fold(false, |l, r| l || r)
    }
}
