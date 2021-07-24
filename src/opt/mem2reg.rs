use std::collections::{HashMap, HashSet};
use std::ops::Deref;

use crate::ir::*;
use crate::opt::FunctionPass;
use crate::*;

pub type Mem2reg = FunctionPass<Mem2regInner>;

#[derive(Default)]
pub struct Mem2regInner {}

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
        dbg!(&inpromotable);
        dbg!(&stores);
        dbg!(&graph);
        dbg!(&reverse_cfg);
        true
    }
}
