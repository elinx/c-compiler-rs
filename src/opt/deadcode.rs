use std::collections::{HashMap, HashSet};
use std::ops::Deref;

use crate::ir::*;
use crate::opt::FunctionPass;
use crate::*;

pub type Deadcode = FunctionPass<Repeat<DeadcodeInner>>;

#[derive(Default)]
pub struct DeadcodeInner {}

fn collect_operand(operand: &Operand, uses: &mut HashMap<RegisterId, Dtype>) {
    // if let Some((RegisterId::Temp { bid, iid }, dtype)) = operand.get_register() {
    //     uses.insert(RegisterId::temp(*bid, *iid), dtype.clone());
    // }
    if let Some((rid, dtype)) = operand.get_register() {
        uses.insert(rid.clone(), dtype.clone());
    }
}

fn collect_operands(
    code: &mut FunctionDefinition,
) -> (
    HashSet<RegisterId>,
    HashMap<RegisterId, Dtype>,
    HashSet<RegisterId>,
) {
    let mut defs = HashSet::new();
    let mut nops = HashSet::new();
    let mut uses = HashMap::new();
    for (bid, block) in &code.blocks {
        for (iid, instr) in block.instructions.iter().enumerate() {
            match &instr.deref() {
                Instruction::Nop => {
                    nops.insert(RegisterId::temp(*bid, iid));
                }
                Instruction::BinOp { lhs, rhs, .. } => {
                    collect_operand(lhs, &mut uses);
                    collect_operand(rhs, &mut uses);
                    defs.insert(RegisterId::temp(*bid, iid));
                }
                Instruction::UnaryOp { operand, .. } => {
                    collect_operand(operand, &mut uses);
                    defs.insert(RegisterId::temp(*bid, iid));
                }
                Instruction::Store { ptr, value } => {
                    collect_operand(ptr, &mut uses);
                    collect_operand(value, &mut uses);
                }
                Instruction::Load { ptr } => {
                    collect_operand(ptr, &mut uses);
                    defs.insert(RegisterId::temp(*bid, iid));
                }
                Instruction::Call { callee, args, .. } => {
                    collect_operand(callee, &mut uses);
                    for operand in args.iter() {
                        collect_operand(operand, &mut uses);
                    }
                    // defs.insert(RegisterId::temp(*bid, iid));
                }
                Instruction::TypeCast { value, .. } => {
                    collect_operand(value, &mut uses);
                    defs.insert(RegisterId::temp(*bid, iid));
                }
                Instruction::GetElementPtr { ptr, offset, .. } => {
                    collect_operand(ptr, &mut uses);
                    collect_operand(offset, &mut uses);
                    defs.insert(RegisterId::temp(*bid, iid));
                }
            }
        }
        match &block.exit {
            BlockExit::Jump { arg } => arg
                .args
                .iter()
                .for_each(|operand| collect_operand(operand, &mut uses)),
            BlockExit::ConditionalJump {
                condition,
                arg_then,
                arg_else,
            } => {
                collect_operand(&condition, &mut uses);
                arg_then
                    .args
                    .iter()
                    .for_each(|operand| collect_operand(operand, &mut uses));
                arg_else
                    .args
                    .iter()
                    .for_each(|operand| collect_operand(operand, &mut uses))
            }
            BlockExit::Switch { value, .. } => collect_operand(&value, &mut uses),
            BlockExit::Return { value } => collect_operand(&value, &mut uses),
            _ => {}
        }
    }
    (defs, uses, nops)
}

impl Optimize<FunctionDefinition> for DeadcodeInner {
    fn optimize(&mut self, code: &mut FunctionDefinition) -> bool {
        let (defs, uses, nops) = collect_operands(code);
        // dbg!(&defs, &uses, &nops);

        if self.delete_dead_allocas(code, &uses) {
            return true;
        }
        if self.delete_nops(code, &nops) {
            return true;
        }
        if self.delete_dead_instr(code, &defs, &uses) {
            return true;
        }
        false
    }
}

impl DeadcodeInner {
    fn delete_dead_allocas(
        &self,
        code: &mut FunctionDefinition,
        uses: &HashMap<RegisterId, Dtype>,
    ) -> bool {
        let mut replaces = HashMap::new();
        let mut to_delete = Vec::new();
        let defs: Vec<RegisterId> = code
            .allocations
            .iter()
            .enumerate()
            .map(|(aid, _)| RegisterId::local(aid))
            .collect();
        for rid in defs {
            if uses.get(&rid).is_none() {
                if let RegisterId::Local { aid } = rid {
                    to_delete.push(aid);
                    for i in (aid + 1)..code.allocations.len() {
                        let orig_rid = RegisterId::local(i);
                        let new_rid = RegisterId::local(i - 1);
                        replaces
                            .entry(orig_rid)
                            .and_modify(|rid| {
                                if let RegisterId::Local { aid } = rid {
                                    *aid -= 1;
                                }
                            })
                            .or_insert(new_rid);
                    }
                }
            }
        }
        self.modify_affected_operands(code, &replaces);
        self.delete_allocas(code, &mut to_delete)
    }

    fn delete_dead_instr(
        &self,
        code: &mut FunctionDefinition,
        defs: &HashSet<RegisterId>,
        uses: &HashMap<RegisterId, Dtype>,
    ) -> bool {
        let mut replaces = HashMap::new();
        let mut to_delete = Vec::new();
        for def_rid in defs {
            if uses.get(def_rid).is_none() {
                if let RegisterId::Temp { bid, iid } = def_rid {
                    to_delete.push((*bid, *iid));
                    for i in (*iid + 1)..code.blocks.get(bid).unwrap().instructions.len() {
                        let orig_rid = RegisterId::temp(*bid, i);
                        let new_rid = RegisterId::temp(*bid, i - 1);
                        replaces
                            .entry(orig_rid)
                            .and_modify(|rid| {
                                if let RegisterId::Temp { iid, .. } = rid {
                                    *iid -= 1;
                                }
                            })
                            .or_insert(new_rid);
                    }
                }
            }
        }
        self.modify_affected_operands(code, &replaces);
        self.delete_instructions(code, &mut to_delete)
    }

    fn delete_nops(&self, code: &mut FunctionDefinition, nops: &HashSet<RegisterId>) -> bool {
        let mut replaces = HashMap::new();
        let mut to_delete = Vec::new();
        for nop in nops {
            if let RegisterId::Temp { bid, iid } = nop {
                to_delete.push((*bid, *iid));
                for i in (*iid + 1)..code.blocks.get(bid).unwrap().instructions.len() {
                    let orig_rid = RegisterId::temp(*bid, i);
                    let new_rid = RegisterId::temp(*bid, i - 1);
                    replaces
                        .entry(orig_rid)
                        .and_modify(|rid| {
                            if let RegisterId::Temp { iid, .. } = rid {
                                *iid -= 1;
                            }
                        })
                        .or_insert(new_rid);
                }
                for (_, block) in &mut code.blocks {
                    block.walk(|operand| {
                        if let Some((rid, _)) = operand.get_register() {
                            if rid == nop {
                                return Operand::constant(Constant::unit());
                            }
                        }
                        operand.clone()
                    })
                }
            }
        }
        self.modify_affected_operands(code, &replaces);
        self.delete_instructions(code, &mut to_delete)
    }

    fn modify_affected_operands(
        &self,
        code: &mut FunctionDefinition,
        replaces: &HashMap<RegisterId, RegisterId>,
    ) {
        for (_, block) in &mut code.blocks {
            block.walk(|operand| {
                if let Some((orig_rid, dtype)) = operand.get_register() {
                    if let Some(new_rid) = replaces.get(orig_rid) {
                        return Operand::register(new_rid.clone(), dtype.clone());
                    }
                }
                operand.clone()
            })
        }
    }

    fn delete_instructions(
        &self,
        code: &mut FunctionDefinition,
        to_delete: &mut Vec<(BlockId, usize)>,
    ) -> bool {
        to_delete.sort();
        to_delete.iter().rev().for_each(|(bid, iid)| {
            code.blocks.get_mut(bid).unwrap().instructions.remove(*iid);
        });
        !to_delete.is_empty()
    }

    fn delete_allocas(&self, code: &mut FunctionDefinition, to_delete: &mut Vec<usize>) -> bool {
        to_delete.sort();
        to_delete.iter().rev().for_each(|aid| {
            code.allocations.remove(*aid);
        });
        !to_delete.is_empty()
    }
}
