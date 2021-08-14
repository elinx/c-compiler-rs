use core::panic;
use std::collections::HashMap;
use std::ops::Deref;
use std::{mem, vec};

use crate::asm::{
    self, Function, IType, Immediate, Pseudo, RType, Register, SType, Section, SymbolType, Variable,
};
use crate::asm::{Directive, SectionType};
use crate::ir::{self, HasDtype, Named, RegisterId};
use crate::ir::{Dtype, Operand};
use crate::Translate;
use lang_c::ast::{self, BinaryOperator};

#[derive(Default)]
pub struct Asmgen {}

struct FunctionContext {
    temp_register_offset: HashMap<RegisterId, usize>,
    instrs: Vec<asm::Instruction>,
    stack_offset: usize,
    rid: Option<RegisterId>,
}

enum Value {
    Constant(usize),
    Register(Register),
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
                        self.translate_function(&label, signature, definition),
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
        _initializer: &Option<ast::Initializer>,
    ) -> Variable {
        // TODO: get value from initializer
        // if let Some(initializer) = initializer {
        // match initializer {
        //     ast::Initializer::Expression(expr) => {
        //         match &expr.node {
        //             ast::Expression::Constant(constant) => {
        //                 match &constant.deref().node {
        //                     ast::Constant::Integer(_) => todo!(),
        //                     ast::Constant::Float(_) => todo!(),
        //                     ast::Constant::Character(_) => todo!(),
        //                 }
        //             }
        //             _ => todo!()
        //         }
        //             let constant = ir::Constant::try_from(&expr.node);
        //             let value = Self::try_from(constant)?;
        //     },
        //     _ => todo!()
        // }
        let decl = if let Some(size) = dtype.get_int_width() {
            match size {
                8 => Directive::Byte(0),
                16 => Directive::Half(0),
                32 => Directive::Word(0),
                64 => Directive::Quad(0),
                _ => panic!("illegal length"),
            }
        } else {
            todo!()
        };
        Variable::new(label.to_owned(), vec![decl])
    }

    fn translate_function(
        &self,
        _label: &asm::Label,
        _signature: &ir::FunctionSignature,
        definition: &Option<ir::FunctionDefinition>,
    ) -> Function {
        let mut func_context = FunctionContext {
            temp_register_offset: HashMap::new(),
            instrs: Vec::new(),
            stack_offset: 16, // ra + fp
            rid: None,
        };
        let mut blocks = Vec::new();

        // explicitly insert a main label
        blocks.push(asm::Block::new(Some(asm::Label("main".to_owned())), vec![]));
        if let Some(definition) = definition {
            func_context.translate_allocations(&definition.allocations);
            for (bid, block) in &definition.blocks {
                let block_label = asm::Label::new("", *bid);
                for (iid, instr) in block.instructions.iter().enumerate() {
                    let rid = RegisterId::temp(*bid, iid);
                    func_context.set_rid(rid);
                    func_context.translate_instruction(&instr.deref());
                }
                func_context.translate_block_exit(&block.exit);
                blocks.push(asm::Block::new(
                    Some(block_label),
                    mem::replace(&mut func_context.instrs, Vec::new()),
                ));
            }
        }
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

    fn insert_instr(&mut self, index: usize, instr: asm::Instruction) {
        self.instrs.insert(index, instr)
    }

    fn push_accumulator(&mut self, dtype: Dtype) {
        // TODO: calculate data type size using dtype
        let size = match dtype {
            ir::Dtype::Int { width, .. } => (width - 1) / ir::Dtype::BITS_OF_BYTE + 1,
            ir::Dtype::Float { width, .. } => (width - 1) / ir::Dtype::BITS_OF_BYTE + 1,
            ir::Dtype::Pointer { .. } => ir::Dtype::SIZE_OF_POINTER,
            _ => todo!("data size failed: {:?}", dtype),
        };
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

    #[allow(dead_code)]
    fn pop_accumulator(&mut self, rd: Register) {
        self.instrs.push(asm::Instruction::IType {
            instr: IType::LW,
            rd,
            rs1: Register::S0,
            imm: Immediate::Value(0),
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
            // ir::Instruction::Call { callee, args, return_type } => todo!(),
            ir::Instruction::TypeCast {
                value,
                target_dtype,
            } => self.translate_typecast(value, target_dtype),
            // ir::Instruction::GetElementPtr { ptr, offset, dtype } => todo!(),
            _ => todo!(),
        }
    }

    #[allow(dead_code)]
    fn translate_prologue(&mut self, stack_frame_size: u64) {
        self.insert_instr(
            0,
            asm::Instruction::IType {
                instr: IType::ADDI,
                rd: Register::Sp,
                rs1: Register::Sp,
                imm: Immediate::Value((stack_frame_size as i128 * -1) as u64),
            },
        );
        self.insert_instr(
            1,
            asm::Instruction::SType {
                instr: SType::SD,
                rs1: Register::Sp,
                rs2: Register::Ra,
                imm: Immediate::Value(stack_frame_size - 8),
            },
        );
        self.insert_instr(
            2,
            asm::Instruction::SType {
                instr: SType::SD,
                rs1: Register::Sp,
                rs2: Register::S0,
                imm: Immediate::Value(stack_frame_size - 16),
            },
        );
        self.insert_instr(
            3,
            asm::Instruction::IType {
                instr: IType::ADDI,
                rd: Register::S0,
                rs1: Register::Sp,
                imm: Immediate::Value(stack_frame_size),
            },
        )
    }

    #[allow(dead_code)]
    fn translate_epilogue(&mut self, stack_frame_size: u64) {
        self.push_instr(asm::Instruction::IType {
            instr: IType::LD,
            rd: Register::S0,
            rs1: Register::Sp,
            imm: Immediate::Value(stack_frame_size - 16),
        });
        self.push_instr(asm::Instruction::IType {
            instr: IType::LD,
            rd: Register::Ra,
            rs1: Register::Sp,
            imm: Immediate::Value(stack_frame_size - 8),
        });
        self.push_instr(asm::Instruction::IType {
            instr: IType::ADDI,
            rd: Register::Sp,
            rs1: Register::Sp,
            imm: Immediate::Value(stack_frame_size),
        })
    }

    fn translate_operand(&mut self, operand: &Operand) -> Value {
        match operand {
            Operand::Constant(constant) => match constant {
                // ir::Constant::Undef { .. } => todo!(),
                // ir::Constant::Unit => todo!(),
                ir::Constant::Int { value, .. } => Value::Constant(*value as usize),
                // ir::Constant::Float { value, width } => todo!(),
                ir::Constant::GlobalVariable { name, .. } => {
                    self.push_instr(asm::Instruction::Pseudo(Pseudo::La {
                        rd: Register::T0,
                        symbol: asm::Label(name.to_owned()),
                    }));
                    Value::Register(Register::T0)
                }
                _ => todo!(),
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
                        RegisterId::Arg { .. } => todo!(),
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
        match op {
            // BinaryOperator::Index => todo!(),
            // BinaryOperator::Multiply => todo!(),
            // BinaryOperator::Divide => todo!(),
            // BinaryOperator::Modulo => todo!(),
            // BinaryOperator::Plus => todo!(),
            // BinaryOperator::Minus => todo!(),
            // BinaryOperator::ShiftLeft => todo!(),
            // BinaryOperator::ShiftRight => todo!(),
            // BinaryOperator::Less => todo!(),
            // BinaryOperator::Greater => todo!(),
            // BinaryOperator::LessOrEqual => todo!(),
            BinaryOperator::GreaterOrEqual => {
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
                }
                // value of rhs is in t0
                match self.translate_operand(rhs) {
                    Value::Constant(imm) => self.push_instr(asm::Instruction::Pseudo(Pseudo::Li {
                        rd: Register::T0,
                        imm: imm as u64,
                    })),
                    Value::Register(_) => {}
                }
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
            // BinaryOperator::Equals => todo!(),
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
            _ => todo!(),
        }
        self.push_accumulator(dtype.to_owned());
    }

    fn translate_block_exit(&mut self, exit: &ir::BlockExit) {
        match exit {
            // ir::BlockExit::Jump { arg } => todo!(),
            // ir::BlockExit::ConditionalJump { condition, arg_then, arg_else } => todo!(),
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
                }
                let stack_frame_size = FunctionContext::align_to(
                    self.stack_offset,
                    FunctionContext::STACK_ALIGNMENT_BYTE,
                ) as u64;
                self.translate_prologue(stack_frame_size);
                self.translate_epilogue(stack_frame_size);
                self.push_instr(asm::Instruction::Pseudo(Pseudo::Ret));
            }
            // ir::BlockExit::Unreachable => todo!(),
            _ => todo!(),
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
        }
        self.push_accumulator(dtype.to_owned());
    }
}
