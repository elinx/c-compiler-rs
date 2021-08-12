use core::panic;
use std::collections::HashMap;
use std::ops::Deref;
use std::vec;

use crate::asm::{
    self, Function, IType, Immediate, Pseudo, Register, SType, Section, SymbolType, Variable,
};
use crate::asm::{Directive, SectionType};
use crate::ir::{self, RegisterId};
use crate::ir::{Dtype, Operand};
use crate::Translate;
use lang_c::ast;

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
        signature: &ir::FunctionSignature,
        definition: &Option<ir::FunctionDefinition>,
    ) -> Function {
        dbg!(signature, definition);
        let mut func_context = FunctionContext {
            temp_register_offset: HashMap::new(),
            instrs: Vec::new(),
            stack_offset: 0,
            rid: None,
        };
        let blocks = Vec::new();
        if let Some(definition) = definition {
            for (bid, block) in &definition.blocks {
                for (iid, instr) in block.instructions.iter().enumerate() {
                    let rid = RegisterId::temp(*bid, iid);
                    func_context.set_rid(rid);
                    match &instr.deref() {
                        // ir::Instruction::Nop => todo!(),
                        // ir::Instruction::BinOp { op, lhs, rhs, dtype } => todo!(),
                        // ir::Instruction::UnaryOp { op, operand, dtype } => todo!(),
                        ir::Instruction::Store { ptr, value } => {
                            func_context.translate_store(ptr, value);
                        }
                        // ir::Instruction::Load { ptr } => todo!(),
                        // ir::Instruction::Call { callee, args, return_type } => todo!(),
                        ir::Instruction::TypeCast {
                            value,
                            target_dtype,
                        } => {
                            func_context.translate_typecast(value, target_dtype);
                        }
                        // ir::Instruction::GetElementPtr { ptr, offset, dtype } => todo!(),
                        _ => todo!(),
                    }
                }
            }
        }
        Function::new(blocks)
    }
}

impl FunctionContext {
    fn set_rid(&mut self, rid: RegisterId) {
        self.rid = Some(rid);
    }

    fn insert(&mut self, instr: asm::Instruction) {
        self.instrs.push(instr)
    }

    fn push_accumulator(&mut self) {
        let offset = self.stack_offset() as u64;
        self.instrs.push(asm::Instruction::SType {
            instr: SType::SD,
            rs1: Register::Sp,
            rs2: Register::A0,
            imm: Immediate::Value(offset),
        })
    }

    #[allow(dead_code)]
    fn pop_accumulator(&mut self, rd: Register) {
        self.instrs.push(asm::Instruction::IType {
            instr: IType::LW,
            rd,
            rs1: Register::Sp,
            imm: Immediate::Value(0),
        })
    }

    fn pop_accumulator_at(&mut self, rd: Register, offset: usize) {
        self.instrs.push(asm::Instruction::IType {
            instr: IType::LW,
            rd,
            rs1: Register::Sp,
            imm: Immediate::Value(offset as u64),
        })
    }

    fn stack_offset(&mut self) -> usize {
        let offset = self.stack_offset;
        self.stack_offset += 8;
        offset
    }

    fn translate_operand(&mut self, operand: &Operand) -> Value {
        match operand {
            Operand::Constant(constant) => match constant {
                // ir::Constant::Undef { .. } => todo!(),
                // ir::Constant::Unit => todo!(),
                ir::Constant::Int { value, .. } => Value::Constant(*value as usize),
                // ir::Constant::Float { value, width } => todo!(),
                ir::Constant::GlobalVariable { name, .. } => {
                    self.insert(asm::Instruction::Pseudo(Pseudo::La {
                        rd: Register::T0,
                        symbol: asm::Label(name.to_owned()),
                    }));
                    Value::Register(Register::T0)
                }
                _ => todo!(),
            },
            Operand::Register { rid, .. } => {
                if let Some(offset) = self.temp_register_offset.get(rid).cloned() {
                    self.pop_accumulator_at(Register::T0, offset);
                    Value::Register(Register::T0)
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
                self.insert(asm::Instruction::IType {
                    instr: IType::ADDI,
                    rd: Register::A0,
                    rs1: Register::Zero,
                    imm: Immediate::Value(val),
                });
            }
            Value::Register(_) => todo!(),
            // Value::RegImm(_, _) => todo!(),
        }
        self.push_accumulator();
    }

    fn translate_store(&mut self, ptr: &Operand, value: &Operand) {
        let value = self.translate_operand(value);
        let ptr = self.translate_operand(ptr);
    }
}
