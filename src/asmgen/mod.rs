use core::panic;
use std::cmp::max;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::Deref;
use std::{mem, vec};

use crate::asm::{
    self, BType, Function, IType, Immediate, Pseudo, RType, Register, RegisterType, SType, Section,
    SymbolType, Variable,
};
use crate::asm::{Directive, SectionType};
use crate::ir::{self, HasDtype, Named, RegisterId};
use crate::ir::{Dtype, Operand};
use crate::Translate;
use lang_c::ast::{self, BinaryOperator, Float, Integer, UnaryOperator};

#[derive(Default)]
pub struct Asmgen {}

struct FunctionContext {
    temp_register_offset: HashMap<RegisterId, usize>,
    instrs: Vec<asm::Instruction>,
    stack_offset: usize,
    stack_frame_size: usize,
    rid: Option<RegisterId>,
    returns: Vec<usize>,
    max_args_size: usize,
}

// enum Value {
//     // Constant(usize),
//     Register(Register),
//     Function(asm::Label),
//     // RegImm(Register, Immediate),
// }

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

    fn translate_ast_expr(&self, expr: &ast::Expression) -> f64 {
        match expr {
            ast::Expression::Constant(constant) => match &constant.deref().node {
                ast::Constant::Integer(Integer { number, .. }) => {
                    number.deref().parse::<i32>().unwrap() as f64
                }
                ast::Constant::Float(Float { number, .. }) => {
                    number.deref().parse::<f64>().unwrap()
                }
                ast::Constant::Character(_) => todo!(),
            },
            ast::Expression::UnaryOperator(op) => {
                let operator = &op.deref().node.operator.node;
                let operand = self.translate_ast_expr(&op.deref().node.operand.node);
                match operator {
                    // UnaryOperator::PostIncrement => todo!(),
                    // UnaryOperator::PostDecrement => todo!(),
                    // UnaryOperator::PreIncrement => todo!(),
                    // UnaryOperator::PreDecrement => todo!(),
                    // UnaryOperator::Address => todo!(),
                    // UnaryOperator::Indirection => todo!(),
                    // UnaryOperator::Plus => todo!(),
                    UnaryOperator::Minus => -1.0 * operand,
                    // UnaryOperator::Complement => todo!(),
                    // UnaryOperator::Negate => todo!(),
                    // UnaryOperator::SizeOf => todo!(),
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }

    fn translate_global_variable(
        &self,
        label: &asm::Label,
        dtype: &Dtype,
        initializer: &Option<ast::Initializer>,
    ) -> Variable {
        let val = match initializer {
            Some(initializer) => match initializer {
                ast::Initializer::Expression(expr) => self.translate_ast_expr(&expr.node),
                _ => todo!(),
            },
            _ => todo!(),
        };
        let decl = match dtype {
            // Dtype::Unit { is_const } => todo!(),
            Dtype::Int { width, .. } => match width {
                8 => Directive::Byte(val as u8),
                16 => Directive::Half(val as u16),
                32 => Directive::Word(val as u32),
                64 => Directive::Quad(val as u64),
                _ => panic!("illegal length"),
            },
            Dtype::Float { width, .. } => match width {
                32 => Directive::Word((val as f32).to_bits()),
                64 => Directive::Quad(val.to_bits()),
                _ => panic!("illegal length"),
            },
            // Dtype::Pointer { inner, is_const } => todo!(),
            // Dtype::Array { inner, size } => todo!(),
            // Dtype::Struct { name, fields, is_const, size_align_offsets } => todo!(),
            // Dtype::Function { ret, params } => todo!(),
            // Dtype::Typedef { name, is_const } => todo!(),
            _ => todo!(),
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
            max_args_size: 0,
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

    fn alloc_accumulator(&self, dtype: &Dtype) -> Register {
        match dtype {
            // Dtype::Unit { is_const } => todo!(),
            Dtype::Int { .. } | Dtype::Pointer { .. } => Register::arg(RegisterType::Integer, 0),
            Dtype::Float { .. } => Register::arg(RegisterType::FloatingPoint, 0),
            // Dtype::Array { inner, size } => todo!(),
            // Dtype::Struct { name, fields, is_const, size_align_offsets } => todo!(),
            // Dtype::Function { ret, params } => todo!(),
            // Dtype::Typedef { name, is_const } => todo!(),
            _ => todo!(),
        }
    }

    fn alloc_tmp(&self, dtype: &Dtype) -> Register {
        match dtype {
            // Dtype::Unit { is_const } => todo!(),
            Dtype::Int { .. } | Dtype::Pointer { .. } => Register::temp(RegisterType::Integer, 0),
            Dtype::Float { .. } => Register::temp(RegisterType::FloatingPoint, 0),
            // Dtype::Array { inner, size } => todo!(),
            // Dtype::Struct { name, fields, is_const, size_align_offsets } => todo!(),
            // Dtype::Function { ret, params } => todo!(),
            // Dtype::Typedef { name, is_const } => todo!(),
            _ => todo!(),
        }
    }

    fn push_accumulator(&mut self, dtype: &Dtype) {
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
            instr: SType::store(dtype.clone()),
            rs1: Register::S0,
            rs2: self.alloc_accumulator(dtype),
            imm: Immediate::Value((offset as i128 * -1) as u64),
        })
    }

    fn push_phinode(&mut self, dtype: Dtype, offset: usize) -> usize {
        self.temp_register_offset.insert(
            self.rid.as_ref().unwrap().clone(),
            (offset as i128 * -1) as usize,
        );
        let size = match dtype {
            ir::Dtype::Int { width, .. } => (width - 1) / ir::Dtype::BITS_OF_BYTE + 1,
            ir::Dtype::Float { width, .. } => (width - 1) / ir::Dtype::BITS_OF_BYTE + 1,
            ir::Dtype::Pointer { .. } => ir::Dtype::SIZE_OF_POINTER,
            _ => todo!("data size failed: {:?}", dtype),
        };
        let size = FunctionContext::align_to(size, 4); // align to word boundary
        offset + size
    }

    fn push_arg(&mut self, dtype: Dtype, offset: usize) -> usize {
        let size = match dtype {
            ir::Dtype::Int { width, .. } => (width - 1) / ir::Dtype::BITS_OF_BYTE + 1,
            ir::Dtype::Float { width, .. } => (width - 1) / ir::Dtype::BITS_OF_BYTE + 1,
            ir::Dtype::Pointer { .. } => ir::Dtype::SIZE_OF_POINTER,
            _ => todo!("data size failed: {:?}", dtype),
        };
        let size = FunctionContext::align_to(size, 4); // align to word boundary
        self.instrs.push(asm::Instruction::SType {
            instr: SType::store(dtype.clone()),
            rs1: Register::Sp,
            rs2: self.alloc_accumulator(&dtype),
            imm: Immediate::Value(offset as u64),
        });
        offset + size
    }

    fn update_max_args_size(&mut self, size: usize) {
        self.max_args_size = max(self.max_args_size, size);
    }

    fn pop_accumulator(&mut self, offset: usize, dtype: &Dtype) -> Register {
        let rd = self.alloc_tmp(dtype);
        self.instrs.push(asm::Instruction::IType {
            instr: IType::load(dtype.clone()),
            rd,
            rs1: Register::S0,
            imm: Immediate::Value((offset as i128 * -1) as u64),
        });
        rd
    }

    fn move_tmp_to_accumulator(&mut self, dtype: &Dtype) {
        match dtype {
            Dtype::Unit { .. } => {}
            Dtype::Int { .. } | Dtype::Pointer { .. } => {
                self.push_instr(asm::Instruction::Pseudo(Pseudo::Mv {
                    rd: Register::A0,
                    rs: Register::T0,
                }))
            }
            Dtype::Float { .. } => self.push_instr(asm::Instruction::Pseudo(Pseudo::Fmv {
                data_size: dtype.clone().try_into().unwrap(),
                rd: Register::FA0,
                rs: Register::FT0,
            })),
            _ => todo!(),
        }
    }

    fn stack_offset(&mut self, data_size: usize) -> usize {
        self.stack_offset += data_size;
        self.stack_offset
    }

    fn update_stack_frame_size(&mut self, size: usize) {
        self.stack_frame_size = size + self.max_args_size;
    }

    fn add_return(&mut self, bid: usize) {
        self.returns.push(bid);
    }

    fn translate_phinodes(&mut self, bid: &ir::BlockId, phinodes: &Vec<Named<Dtype>>) {
        let mut offset = 0;
        for (aid, dtype) in phinodes.iter().enumerate() {
            let rid = RegisterId::arg(bid.clone(), aid);
            self.set_rid(rid);
            offset = self.push_phinode(dtype.deref().clone(), offset);
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
            ir::Instruction::UnaryOp { op, operand, dtype } => {
                self.translate_unary(op, operand, dtype)
            }
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

    fn translate_operand(&mut self, operand: &Operand) {
        match operand {
            Operand::Constant(constant) => match constant {
                ir::Constant::Undef { .. } | ir::Constant::Unit => {}
                ir::Constant::Int { value, .. } => {
                    self.push_instr(asm::Instruction::Pseudo(Pseudo::Li {
                        rd: Register::T0,
                        imm: *value as u64,
                    }));
                }
                ir::Constant::Float { value, width } => {
                    self.push_instr(asm::Instruction::Pseudo(Pseudo::Li {
                        rd: Register::A0,
                        imm: value.to_bits(),
                    }));
                    self.push_instr(asm::Instruction::RType {
                        instr: RType::fcvt_int_to_float(Dtype::int(*width), operand.dtype()),
                        rd: Register::FT0,
                        rs1: Register::A0,
                        rs2: None,
                    });
                }
                ir::Constant::GlobalVariable { name, dtype } => {
                    match dtype {
                        // Dtype::Unit { is_const } => todo!(),
                        Dtype::Int { .. } | Dtype::Float { .. } => {
                            self.push_instr(asm::Instruction::Pseudo(Pseudo::La {
                                rd: Register::T0,
                                symbol: asm::Label(name.to_owned()),
                            }));
                        }
                        // Dtype::Pointer { inner, is_const } => todo!(),
                        // Dtype::Array { inner, size } => todo!(),
                        // Dtype::Struct { name, fields, is_const, size_align_offsets } => todo!(),
                        Dtype::Function { .. } => {
                            self.push_instr(asm::Instruction::Pseudo(Pseudo::La {
                                rd: Register::T0,
                                symbol: asm::Label(name.to_owned()),
                            }))
                        }
                        // Dtype::Typedef { name, is_const } => todo!(),
                        _ => todo!(),
                    }
                }
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
                        }
                        RegisterId::Arg { .. } | RegisterId::Temp { .. } => {
                            self.pop_accumulator(offset, dtype);
                        }
                    }
                } else {
                    panic!("can't find temp register");
                }
            }
        }
    }

    fn translate_typecast(&mut self, value: &Operand, target_type: &Dtype) {
        self.translate_operand(value);
        match (value.dtype(), target_type) {
            (Dtype::Int { .. }, Dtype::Int { .. }) => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::add(target_type.clone()),
                    rd: Register::A0,
                    rs1: Register::Zero,
                    rs2: Some(Register::T0),
                });
            }
            (Dtype::Int { .. }, Dtype::Float { .. }) => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::fcvt_int_to_float(value.dtype(), target_type.clone()),
                    rd: Register::FA0,
                    rs1: Register::T0,
                    rs2: None,
                });
            }
            (Dtype::Float { .. }, Dtype::Int { .. }) => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::fcvt_float_to_int(value.dtype(), target_type.clone()),
                    rd: Register::A0,
                    rs1: Register::FT0,
                    rs2: None,
                });
            }
            (
                Dtype::Float { width, .. },
                Dtype::Float {
                    width: target_width,
                    ..
                },
            ) => {
                if width == *target_width {
                    self.push_instr(asm::Instruction::Pseudo(Pseudo::Fmv {
                        data_size: target_type.clone().try_into().unwrap(),
                        rd: Register::FA0,
                        rs: Register::FT0,
                    }));
                } else {
                    self.push_instr(asm::Instruction::RType {
                        instr: RType::FcvtFloatToFloat {
                            from: value.dtype().try_into().unwrap(),
                            to: target_type.clone().try_into().unwrap(),
                        },
                        rd: Register::FA0,
                        rs1: Register::FT0,
                        rs2: None,
                    })
                }
            }
            _ => todo!(),
        }
        self.push_accumulator(target_type);
    }

    fn translate_store(&mut self, ptr: &Operand, value: &Operand) {
        self.translate_operand(ptr);
        self.push_instr(asm::Instruction::Pseudo(Pseudo::Mv {
            rd: Register::A0,
            rs: Register::T0,
        }));
        self.translate_operand(value);
        self.push_instr(asm::Instruction::SType {
            instr: SType::store(ptr.dtype().get_pointer_inner().unwrap().clone()),
            rs1: Register::A0,
            rs2: self.alloc_tmp(&value.dtype()),
            imm: Immediate::Value(0),
        });
    }

    fn translate_binop(
        &mut self,
        op: &BinaryOperator,
        lhs: &Operand,
        rhs: &Operand,
        dtype: &Dtype,
    ) {
        // value of lhs is in a0
        self.translate_operand(lhs);
        self.move_tmp_to_accumulator(&lhs.dtype());
        let rs1 = self.alloc_accumulator(dtype);
        // value of rhs is in t0
        self.translate_operand(rhs);
        let rs2 = Some(self.alloc_tmp(dtype));
        let rd = self.alloc_accumulator(dtype);
        match op {
            // BinaryOperator::Index => todo!(),
            BinaryOperator::Multiply => {
                self.push_instr(asm::Instruction::RType {
                    instr: if matches!(dtype, Dtype::Int{..}) {
                        RType::mul(lhs.dtype())
                    } else {
                        RType::fmul(lhs.dtype())
                    },
                    rd,
                    rs1,
                    rs2,
                });
            }
            BinaryOperator::Divide => {
                self.push_instr(asm::Instruction::RType {
                    instr: if matches!(dtype, Dtype::Int{..}) {
                        RType::div(lhs.dtype(), lhs.dtype().is_int_signed())
                    } else {
                        RType::fdiv(lhs.dtype())
                    },
                    rd,
                    rs1,
                    rs2,
                });
            }
            BinaryOperator::Modulo => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::rem(lhs.dtype(), lhs.dtype().is_int_signed()),
                    rd,
                    rs1,
                    rs2,
                });
            }
            BinaryOperator::Plus => {
                self.push_instr(asm::Instruction::RType {
                    instr: if matches!(dtype, Dtype::Int{..}) {
                        RType::add(lhs.dtype())
                    } else {
                        RType::fadd(lhs.dtype())
                    },
                    rd,
                    rs1,
                    rs2,
                });
            }
            BinaryOperator::Minus => {
                self.push_instr(asm::Instruction::RType {
                    instr: if matches!(dtype, Dtype::Int{..}) {
                        RType::sub(lhs.dtype())
                    } else {
                        RType::fsub(lhs.dtype())
                    },
                    rd,
                    rs1,
                    rs2,
                });
            }
            BinaryOperator::ShiftLeft => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::sll(lhs.dtype()),
                    rd,
                    rs1,
                    rs2,
                });
            }
            BinaryOperator::ShiftRight => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::srl(lhs.dtype()),
                    rd,
                    rs1,
                    rs2,
                });
            }
            BinaryOperator::Less => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::Slt {
                        is_signed: lhs.dtype().is_int_signed(),
                    },
                    rd,
                    rs1,
                    rs2,
                });
            }
            BinaryOperator::Greater => {
                // a > b => b < a
                self.push_instr(asm::Instruction::RType {
                    instr: RType::Slt {
                        is_signed: lhs.dtype().is_int_signed(),
                    },
                    rd,
                    rs1: Register::T0,
                    rs2: Some(Register::A0),
                });
            }
            // BinaryOperator::LessOrEqual => todo!(),
            BinaryOperator::GreaterOrEqual => {
                // a >= b => !(a < b) => (a < b) ^ 1
                self.push_instr(asm::Instruction::RType {
                    instr: RType::Slt {
                        is_signed: lhs.dtype().is_int_signed(),
                    },
                    rd,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
                self.push_instr(asm::Instruction::IType {
                    instr: IType::Xori,
                    rd,
                    rs1: Register::A0,
                    imm: Immediate::Value(1),
                })
            }
            BinaryOperator::Equals => {
                // a == b => (a ^ b) == 0
                self.push_instr(asm::Instruction::RType {
                    instr: RType::Xor,
                    rd,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
                self.push_instr(asm::Instruction::Pseudo(Pseudo::Seqz {
                    rd,
                    rs: Register::A0,
                }));
            }
            BinaryOperator::NotEquals => {
                // a != b => (a ^ b) != 0
                self.push_instr(asm::Instruction::RType {
                    instr: RType::Xor,
                    rd,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
                self.push_instr(asm::Instruction::Pseudo(Pseudo::Snez {
                    rd,
                    rs: Register::A0,
                }));
            }
            BinaryOperator::BitwiseAnd => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::And,
                    rd,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
            }
            BinaryOperator::BitwiseXor => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::Xor,
                    rd,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
            }
            BinaryOperator::BitwiseOr => {
                self.push_instr(asm::Instruction::RType {
                    instr: RType::Or,
                    rd,
                    rs1: Register::A0,
                    rs2: Some(Register::T0),
                });
            }
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
        self.push_accumulator(dtype);
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
                let then_label = asm::Label::new(name, arg_then.deref().bid);
                let else_label = asm::Label::new(name, arg_else.deref().bid);
                self.translate_operand(condition);
                self.push_instr(asm::Instruction::Pseudo(Pseudo::Li {
                    rd: Register::A0,
                    imm: 1,
                }));
                self.push_instr(asm::Instruction::BType {
                    instr: BType::Beq,
                    rs1: Register::T0, // should not be floating point
                    rs2: Register::A0,
                    imm: then_label,
                });
                self.push_instr(asm::Instruction::Pseudo(Pseudo::J { offset: else_label }));
            }
            ir::BlockExit::Switch {
                value,
                default,
                cases,
            } => {
                self.translate_operand(value);
                cases.iter().for_each(|(constant, jump)| {
                    let imm = constant.get_int().unwrap().0 as u64;
                    self.push_instr(asm::Instruction::Pseudo(Pseudo::Li {
                        rd: Register::A0,
                        imm,
                    }));
                    self.push_instr(asm::Instruction::BType {
                        instr: BType::Beq,
                        rs1: Register::T0,
                        rs2: Register::A0,
                        imm: asm::Label::new(name, jump.deref().bid),
                    });
                });
                self.push_instr(asm::Instruction::Pseudo(Pseudo::J {
                    offset: asm::Label::new(name, default.deref().bid),
                }));
            }
            ir::BlockExit::Return { value } => {
                self.translate_operand(value);
                self.move_tmp_to_accumulator(&value.dtype());
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
            self.push_accumulator(dtype.deref());
        }
    }

    fn translate_load(&mut self, ptr: &Operand, dtype: &Dtype) {
        self.translate_operand(ptr);
        self.push_instr(asm::Instruction::IType {
            instr: IType::load(dtype.to_owned()),
            rd: self.alloc_accumulator(dtype),
            rs1: Register::T0,
            imm: Immediate::Value(0),
        });
        self.push_accumulator(dtype);
    }

    fn translate_call(&mut self, callee: &Operand, args: &Vec<Operand>, return_type: &Dtype) {
        // push args in reverse order, eg arg1, arg0... based on Sp
        let mut offset = 0;
        args.iter().for_each(|arg| {
            self.translate_operand(arg);
            self.move_tmp_to_accumulator(&arg.dtype());
            offset = self.push_arg(arg.dtype(), offset);
        });
        self.update_max_args_size(offset);
        self.translate_operand(callee);
        self.push_instr(asm::Instruction::Pseudo(Pseudo::Jalr { rs: Register::T0 }));
        match return_type {
            Dtype::Unit { .. } => {}
            _ => {
                self.push_accumulator(return_type);
            }
        }
    }

    fn translate_unary(&mut self, op: &ast::UnaryOperator, operand: &Operand, dtype: &Dtype) {
        self.translate_operand(operand);
        match op {
            // ast::UnaryOperator::PostIncrement => todo!(),
            // ast::UnaryOperator::PostDecrement => todo!(),
            // ast::UnaryOperator::PreIncrement => todo!(),
            // ast::UnaryOperator::PreDecrement => todo!(),
            // ast::UnaryOperator::Address => todo!(),
            // ast::UnaryOperator::Indirection => todo!(),
            // ast::UnaryOperator::Plus => todo!(),
            ast::UnaryOperator::Minus => match dtype {
                Dtype::Int { .. } => self.push_instr(asm::Instruction::Pseudo(Pseudo::neg(
                    dtype.clone(),
                    Register::A0,
                    Register::T0,
                ))),
                Dtype::Float { .. } => self.push_instr(asm::Instruction::Pseudo(Pseudo::fneg(
                    dtype.clone(),
                    Register::FA0,
                    Register::FT0,
                ))),
                _ => todo!(),
            },
            // ast::UnaryOperator::Complement => todo!(),
            ast::UnaryOperator::Negate => match dtype {
                // !a => a == 0
                Dtype::Int { .. } => self.push_instr(asm::Instruction::Pseudo(Pseudo::Seqz {
                    rd: Register::A0,
                    rs: Register::T0,
                })),
                _ => todo!(),
            },
            // ast::UnaryOperator::SizeOf => todo!(),
            _ => todo!("unary op: {:?}", op),
        }
        self.push_accumulator(dtype);
    }
}
