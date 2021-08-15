use core::panic;
use std::collections::HashMap;
use std::ops::Deref;
use std::{mem, vec};

use crate::asm::{
    self, BType, Function, IType, Immediate, Pseudo, RType, Register, SType, Section, SymbolType,
    Variable,
};
use crate::asm::{Directive, SectionType};
use crate::ir::{self, HasDtype, Named, RegisterId};
use crate::ir::{Dtype, Operand};
use crate::Translate;
use lang_c::ast::{self, BinaryOperator, Float, Integer};

#[derive(Default)]
pub struct Asmgen {}

struct FunctionContext {
    temp_register_offset: HashMap<RegisterId, usize>,
    instrs: Vec<asm::Instruction>,
    stack_offset: usize,
    stack_frame_size: usize,
    rid: Option<RegisterId>,
    returns: Vec<usize>,
}

enum Value {
    Constant(usize),
    Register(Register),
    Function(asm::Label),
    // RegImm(Register, Immediate),
}

impl Translate<ir::TranslationUnit> for Asmgen {
    type Target = asm::Asm;
    type Error = ();

    fn translate(&mut self, source: &ir::TranslationUnit) -> Result<Self::Target, Self::Error> {
        let mut functions = Vec::new();
        let mut variables = Vec::new();
        for (name, decl) in &source.decls {
            let label = asm::Label(name.to_owned());
            match decl {
                ir::Declaration::Variable { dtype, initializer } => {
                    variables.push(Section::new(
                        self.create_global_data_section_header(&label),
                        self.translate_global_variable(&label, dtype, initializer),
                    ));
                }
                ir::Declaration::Function {
                    signature,
                    definition,
                } => {
                    functions.push(Section::new(
                        self.create_function_section_header(&label),
                        self.translate_function(name, signature, definition),
                    ));
                }
            }
        }
        // let functions = Section::new
        Ok(asm::Asm {
            unit: asm::TranslationUnit {
                functions,
                variables,
            },
        })
    }
}

impl Asmgen {
    fn create_global_data_section_header(&self, label: &asm::Label) -> Vec<Directive> {
        vec![
            Directive::Globl(label.to_owned()),
            Directive::Section(SectionType::Data),
            Directive::Type(label.to_owned(), SymbolType::Object),
        ]
    }

    fn create_function_section_header(&self, label: &asm::Label) -> Vec<Directive> {
        vec![
            Directive::Globl(label.to_owned()),
            Directive::Section(SectionType::Text),
            Directive::Type(label.to_owned(), SymbolType::Function),
        ]
    }
    fn translate_global_variable(
        &self,
        label: &asm::Label,
        dtype: &Dtype,
        initializer: &Option<ast::Initializer>,
    ) -> Variable {
        let val = match initializer {
            Some(initializer) => match initializer {
                ast::Initializer::Expression(expr) => match &expr.node {
                    ast::Expression::Constant(constant) => match &constant.deref().node {
                        ast::Constant::Integer(Integer { number, .. }) => {
                            number.deref().parse::<i32>().unwrap() as f32
                        }
                        ast::Constant::Float(Float { number, .. }) => {
                            number.deref().parse::<f32>().unwrap()
                        }
                        ast::Constant::Character(_) => todo!(),
                    },
                    _ => todo!(),
                },
                _ => todo!(),
            },
            _ => todo!(),
        };
        let decl = if let Some(size) = dtype.get_int_width() {
            match size {
                8 => Directive::Byte(val as u8),
                16 => Directive::Half(val as u16),
                32 => Directive::Word(val as u32),
                64 => Directive::Quad(val as u64),
                _ => panic!("illegal length"),
            }
        } else {
            todo!()
        };
        Variable::new(label.to_owned(), vec![decl])
    }

    fn translate_function(
        &self,
        name: &str,
        _signature: &ir::FunctionSignature,
        definition: &Option<ir::FunctionDefinition>,
    ) -> Function {
        let mut func_context = FunctionContext {
            temp_register_offset: HashMap::new(),
            instrs: Vec::new(),
            stack_offset: 16, // ra + fp
            stack_frame_size: 16,
            rid: None,
            returns: Vec::new(),
        };
        let mut blocks = Vec::new();

        if let Some(definition) = definition {
            func_context.translate_allocations(&definition.allocations);
            for (bid, block) in &definition.blocks {
                let block_label = asm::Label::new(name, *bid);
                func_context.translate_phinodes(bid, &block.phinodes);
                func_context.translate_instructions(bid, &block.instructions);
                func_context.translate_block_exit(blocks.len(), name, &block.exit);
                blocks.push(asm::Block::new(
                    Some(block_label),
                    mem::replace(&mut func_context.instrs, Vec::new()),
                ));
            }
        }
        func_context.translate_epilogue(&mut blocks);
        func_context.translate_prologue(name, &mut blocks);
        Function::new(blocks)
    }
}

impl FunctionContext {
    pub const STACK_ALIGNMENT_BYTE: usize = 16;

    fn align_to(base: usize, align: usize) -> usize {
        ((base + align - 1) / align) * align
    }

    fn set_rid(&mut self, rid: RegisterId) {
        self.rid = Some(rid);
    }

    fn push_instr(&mut self, instr: asm::Instruction) {
        self.instrs.push(instr)
    }

    #[allow(dead_code)]
    fn insert_instr(&mut self, index: usize, instr: asm::Instruction) {
        self.instrs.insert(index, instr)
    }

    fn push_accumulator(&mut self, dtype: Dtype) {
        let size = match dtype {
            ir::Dtype::Int { width, .. } => (width - 1) / ir::Dtype::BITS_OF_BYTE + 1,
            ir::Dtype::Float { width, .. } => (width - 1) / ir::Dtype::BITS_OF_BYTE + 1,
            ir::Dtype::Pointer { .. } => ir::Dtype::SIZE_OF_POINTER,
            _ => todo!("data size failed: {:?}", dtype),
        };
        let size = FunctionContext::align_to(size, 4); // align to word boundary
        let offset = self.stack_offset(size);
        self.temp_register_offset
            .insert(self.rid.as_ref().unwrap().clone(), offset);
        self.instrs.push(asm::Instruction::SType {
            instr: SType::store(dtype),
            rs1: Register::S0,
            rs2: Register::A0,
            imm: Immediate::Value((offset as i128 * -1) as u64),
        })
    }

    fn pop_accumulator_at(&mut self, rd: Register, offset: usize, dtype: Dtype) {
        self.instrs.push(asm::Instruction::IType {
            instr: IType::load(dtype),
            rd,
            rs1: Register::S0,
            imm: Immediate::Value((offset as i128 * -1) as u64),
        })
    }

    fn stack_offset(&mut self, data_size: usize) -> usize {
        self.stack_offset += data_size;
        self.stack_offset
    }

    fn update_stack_frame_size(&mut self, size: usize) {
        self.stack_frame_size = size;
    }

    fn add_return(&mut self, bid: usize) {
        self.returns.push(bid);
    }

    fn translate_phinodes(&mut self, bid: &ir::BlockId, phinodes: &Vec<Named<Dtype>>) {
        for (aid, dtype) in phinodes.iter().enumerate() {
            let rid = RegisterId::arg(bid.clone(), aid);
            self.set_rid(rid);
            self.push_accumulator(dtype.deref().clone());
        }
    }

    fn translate_instructions(
        &mut self,
        bid: &ir::BlockId,
        instructions: &Vec<Named<ir::Instruction>>,
    ) {
        for (iid, instr) in instructions.iter().enumerate() {
            let rid = RegisterId::temp(*bid, iid);
            self.set_rid(rid);
            self.translate_instruction(&instr.deref());
        }
    }

    fn translate_instruction(&mut self, instr: &ir::Instruction) {
        match instr {
            // ir::Instruction::Nop => todo!(),
            ir::Instruction::BinOp {
                op,
                lhs,
                rhs,
                dtype,
            } => self.translate_binop(op, lhs, rhs, dtype),
            // ir::Instruction::UnaryOp { op, operand, dtype } => todo!(),
            ir::Instruction::Store { ptr, value } => self.translate_store(ptr, value),
            ir::Instruction::Load { ptr } => self.translate_load(ptr, &instr.dtype()),
            ir::Instruction::Call {
                callee,
                args,
                return_type,
            } => self.translate_call(callee, args, return_type),
            ir::Instruction::TypeCast {
                value,
                target_dtype,
            } => self.translate_typecast(value, target_dtype),
            // ir::Instruction::GetElementPtr { ptr, offset, dtype } => todo!(),
            _ => todo!("instr: {:?}", &instr),
        }
    }

    fn translate_prologue(&mut self, name: &str, blocks: &mut Vec<asm::Block>) {
        let stack_frame_size = self.stack_frame_size as u64;
        blocks.insert(
            0,
            asm::Block::new(
                Some(asm::Label(name.to_owned())),
                vec![
                    asm::Instruction::IType {
                        instr: IType::ADDI,
                        rd: Register::Sp,
                        rs1: Register::Sp,
                        imm: Immediate::Value((stack_frame_size as i128 * -1) as u64),
                    },
                    asm::Instruction::SType {
                        instr: SType::SD,
                        rs1: Register::Sp,
                        rs2: Register::Ra,
                        imm: Immediate::Value(stack_frame_size - 8),
                    },
                    asm::Instruction::SType {
                        instr: SType::SD,
                        rs1: Register::Sp,
                        rs2: Register::S0,
                        imm: Immediate::Value(stack_frame_size - 16),
                    },
                    asm::Instruction::IType {
                        instr: IType::ADDI,
                        rd: Register::S0,
                        rs1: Register::Sp,
                        imm: Immediate::Value(stack_frame_size),
                    },
                ],
            ),
        );
    }

    fn translate_epilogue(&mut self, blocks: &mut Vec<asm::Block>) {
        let stack_frame_size = self.stack_frame_size as u64;
        let instrs = vec![
            asm::Instruction::IType {
                instr: IType::LD,
                rd: Register::S0,
                rs1: Register::Sp,
                imm: Immediate::Value(stack_frame_size - 16),
            },
            asm::Instruction::IType {
                instr: IType::LD,
                rd: Register::Ra,
                rs1: Register::Sp,
                imm: Immediate::Value(stack_frame_size - 8),
            },
            asm::Instruction::IType {
                instr: IType::ADDI,
                rd: Register::Sp,
                rs1: Register::Sp,
                imm: Immediate::Value(stack_frame_size),
            },
        ];
        for index in &self.returns {
            for instr in &instrs {
                let block = blocks.get_mut(*index).unwrap();
                block
                    .instructions
                    .insert(block.instructions.len() - 1, instr.clone());
            }
        }
    }

    fn translate_operand(&mut self, operand: &Operand) -> Value {
        match operand {
            Operand::Constant(constant) => match constant {
                ir::Constant::Undef { .. } => Value::Constant(0),
                // ir::Constant::Unit => todo!(),
                ir::Constant::Int { value, .. } => Value::Constant(*value as usize),
                // ir::Constant::Float { value, width } => todo!(),
                ir::Constant::GlobalVariable { name, dtype } => {
                    match dtype {
                        // Dtype::Unit { is_const } => todo!(),
                        // Dtype::Int { width, is_signed, is_const } => todo!(),
                        // Dtype::Float { width, is_const } => todo!(),
                        // Dtype::Pointer { inner, is_const } => todo!(),
                        // Dtype::Array { inner, size } => todo!(),
                        // Dtype::Struct { name, fields, is_const, size_align_offsets } => todo!(),
                        Dtype::Function { .. } => Value::Function(asm::Label(name.to_owned())),
                        // Dtype::Typedef { name, is_const } => todo!(),
                        _ => {
                            self.push_instr(asm::Instruction::Pseudo(Pseudo::La {
                                rd: Register::T0,
                                symbol: asm::Label(name.to_owned()),
                            }));
                            Value::Register(Register::T0)
                        }
                    }
                }
                _ => todo!("constant: {:?}", &constant),
            },
            Operand::Register { rid, dtype } => {
                if let Some(offset) = self.temp_register_offset.get(rid).cloned() {
                    match rid {
                        RegisterId::Local { .. } => {
                            self.push_instr(asm::Instruction::IType {
                                instr: IType::ADDI,
                                rd: Register::T0,
                                rs1: Register::S0,
                                imm: Immediate::Value((offset as i128 * -1) as u64),
                            });
                            Value::Register(Register::T0)
                        }
                        RegisterId::Arg { .. } => {
                            self.pop_accumulator_at(Register::T0, offset, dtype.clone());
                            Value::Register(Register::T0)
                        }
                        RegisterId::Temp { .. } => {
                            self.pop_accumulator_at(Register::T0, offset, dtype.clone());
                            Value::Register(Register::T0)
                        }
                    }
                } else {
                    panic!("can't find temp register");
                }
            }
        }
    }

    fn translate_typecast(&mut self, value: &Operand, target_type: &Dtype) {
        match self.translate_operand(value) {
            Value::Constant(val) => {
                let val = if let Some(size) = target_type.get_int_width() {
                    match size {
                        8 => val as i8 as u64,
                        16 => val as i16 as u64,
                        32 => val as i32 as u64,
                        64 => val as i64 as u64,
                        _ => panic!("illegal length"),
                    }
                } else {
                    panic!("unexpected data")
                };
                self.push_instr(asm::Instruction::IType {
                    instr: IType::ADDI,
                    rd: Register::A0,
                    rs1: Register::Zero,
                    imm: Immediate::Value(val),
                });
            }
            Value::Register(rs) => self.push_instr(asm::Instruction::Pseudo(Pseudo::Mv {
                rd: Register::A0,
                rs,
            })),
            _ => todo!(),
        }
        self.push_accumulator(target_type.to_owned());
    }

    fn translate_store(&mut self, ptr: &Operand, value: &Operand) {
        match self.translate_operand(value) {
            Value::Constant(imm) => self.push_instr(asm::Instruction::Pseudo(Pseudo::Li {
                rd: Register::A0,
                imm: imm as u64,
            })),
            Value::Register(rs) => self.push_instr(asm::Instruction::Pseudo(Pseudo::Mv {
                rd: Register::A0,
                rs,
            })),
            _ => todo!(),
        }
        match self.translate_operand(ptr) {
            Value::Register(rs1) => self.push_instr(asm::Instruction::SType {
                instr: SType::store(ptr.dtype().get_pointer_inner().unwrap().clone()),
                rs1,
                rs2: Register::A0,
                imm: Immediate::Value(0),
            }),
            _ => panic!("ptr operand of store should not be constant value"),
        }
    }

    fn translate_binop(
        &mut self,
        op: &BinaryOperator,
        lhs: &Operand,
        rhs: &Operand,
        dtype: &Dtype,
    ) {
        // value of lhs is in a0
        match self.translate_operand(lhs) {
            Value::Constant(imm) => self.push_instr(asm::Instruction::Pseudo(Pseudo::Li {
                rd: Register::A0,
                imm: imm as u64,
            })),
            Value::Register(rs) => self.push_instr(asm::Instruction::Pseudo(Pseudo::Mv {
                rd: Register::A0,
                rs,
            })),
            _ => todo!(),
        }
        // value of rhs is in t0
        match self.translate_operand(rhs) {
            Value::Constant(imm) => self.push_instr(asm::Instruction::Pseudo(Pseudo::Li {
                rd: Register::T0,
                imm: imm as u64,
            })),
            Value::Register(rs) => self.push_instr(asm::Instruction::Pseudo(Pseudo::Mv {
                rd: Register::T0,
                rs,
            })),
            _ => todo!(),
        }
        match op {
            // BinaryOperator::Index => todo!(),
            BinaryOperator::Multiply => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::mul(dtype.clone()),
                    rd: Register::A0,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
            }
            BinaryOperator::Divide => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::div(lhs.dtype(), lhs.dtype().is_int_signed()),
                    rd: Register::A0,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
            }
            BinaryOperator::Modulo => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::rem(lhs.dtype(), lhs.dtype().is_int_signed()),
                    rd: Register::A0,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
            }
            BinaryOperator::Plus => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::add(lhs.dtype()),
                    rd: Register::A0,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
            }
            BinaryOperator::Minus => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::sub(lhs.dtype()),
                    rd: Register::A0,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
            }
            // BinaryOperator::ShiftLeft => todo!(),
            // BinaryOperator::ShiftRight => todo!(),
            BinaryOperator::Less => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::Slt {
                        is_signed: lhs.dtype().is_int_signed(),
                    },
                    rd: Register::A0,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
            }
            // BinaryOperator::Greater => todo!(),
            // BinaryOperator::LessOrEqual => todo!(),
            BinaryOperator::GreaterOrEqual => {
                // a >= b => !(a < b) => (a < b) ^ 1
                self.push_instr(asm::Instruction::RType {
                    instr: RType::Slt {
                        is_signed: lhs.dtype().is_int_signed(),
                    },
                    rd: Register::A0,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
                self.push_instr(asm::Instruction::IType {
                    instr: IType::Xori,
                    rd: Register::A0,
                    rs1: Register::A0,
                    imm: Immediate::Value(1),
                })
            }
            BinaryOperator::Equals => {
                // a == b => (a ^ b) ^ 1
                self.push_instr(asm::Instruction::IType {
                    instr: IType::Xori,
                    rd: Register::A0,
                    rs1: Register::T0,
                    imm: Immediate::Value(1),
                })
            }
            // BinaryOperator::NotEquals => todo!(),
            // BinaryOperator::BitwiseAnd => todo!(),
            // BinaryOperator::BitwiseXor => todo!(),
            // BinaryOperator::BitwiseOr => todo!(),
            // BinaryOperator::LogicalAnd => todo!(),
            // BinaryOperator::LogicalOr => todo!(),
            // BinaryOperator::Assign => todo!(),
            // BinaryOperator::AssignMultiply => todo!(),
            // BinaryOperator::AssignDivide => todo!(),
            // BinaryOperator::AssignModulo => todo!(),
            // BinaryOperator::AssignPlus => todo!(),
            // BinaryOperator::AssignMinus => todo!(),
            // BinaryOperator::AssignShiftLeft => todo!(),
            // BinaryOperator::AssignShiftRight => todo!(),
            // BinaryOperator::AssignBitwiseAnd => todo!(),
            // BinaryOperator::AssignBitwiseXor => todo!(),
            // BinaryOperator::AssignBitwiseOr => todo!(),
            _ => todo!("op: {:?}", &op),
        }
        self.push_accumulator(dtype.to_owned());
    }

    fn translate_block_exit(&mut self, bid: usize, name: &str, exit: &ir::BlockExit) {
        match exit {
            ir::BlockExit::Jump { arg } => {
                self.push_instr(asm::Instruction::Pseudo(Pseudo::J {
                    offset: asm::Label::new(name, arg.deref().bid),
                }));
            }
            ir::BlockExit::ConditionalJump {
                condition,
                arg_then,
                arg_else,
            } => {
                // TODO: optimize to merge ConditionJump with previous Compare instruction
                match self.translate_operand(condition) {
                    Value::Constant(_) => todo!(),
                    Value::Register(rs1) => {
                        let then_label = asm::Label::new(name, arg_then.deref().bid);
                        let else_label = asm::Label::new(name, arg_else.deref().bid);
                        self.push_instr(asm::Instruction::Pseudo(Pseudo::Li {
                            rd: Register::A0,
                            imm: 1,
                        }));
                        self.push_instr(asm::Instruction::BType {
                            instr: BType::Beq,
                            rs1,
                            rs2: Register::A0,
                            imm: then_label,
                        });
                        self.push_instr(asm::Instruction::Pseudo(Pseudo::J { offset: else_label }));
                    }
                    Value::Function(_) => todo!(),
                }
            }
            // ir::BlockExit::Switch { value, default, cases } => todo!(),
            ir::BlockExit::Return { value } => {
                match self.translate_operand(value) {
                    Value::Constant(imm) => self.push_instr(asm::Instruction::Pseudo(Pseudo::Li {
                        rd: Register::A0,
                        imm: imm as u64,
                    })),
                    Value::Register(rs) => self.push_instr(asm::Instruction::Pseudo(Pseudo::Mv {
                        rd: Register::A0,
                        rs,
                    })),
                    _ => todo!(),
                }
                self.update_stack_frame_size(FunctionContext::align_to(
                    self.stack_offset,
                    FunctionContext::STACK_ALIGNMENT_BYTE,
                ));
                self.add_return(bid);
                self.push_instr(asm::Instruction::Pseudo(Pseudo::Ret));
            }
            // ir::BlockExit::Unreachable => todo!(),
            _ => todo!("exit: {:?}", &exit),
        }
    }

    fn translate_allocations(&mut self, allocations: &Vec<Named<Dtype>>) {
        for (aid, dtype) in allocations.iter().enumerate() {
            let rid = RegisterId::local(aid);
            self.set_rid(rid);
            self.push_accumulator(dtype.deref().clone());
        }
    }

    fn translate_load(&mut self, ptr: &Operand, dtype: &Dtype) {
        match self.translate_operand(ptr) {
            Value::Constant(_) => panic!("can't load value from constant"),
            Value::Register(rs1) => {
                self.push_instr(asm::Instruction::IType {
                    instr: IType::load(dtype.to_owned()),
                    rd: Register::A0,
                    rs1,
                    imm: Immediate::Value(0),
                });
            }
            _ => todo!(),
        }
        self.push_accumulator(dtype.to_owned());
    }

    fn translate_call(&mut self, callee: &Operand, _args: &Vec<Operand>, return_type: &Dtype) {
        match self.translate_operand(callee) {
            Value::Function(offset) => {
                self.push_instr(asm::Instruction::Pseudo(Pseudo::Call { offset }))
            }
            _ => todo!(),
        }
        self.push_accumulator(return_type.clone());
    }
}
