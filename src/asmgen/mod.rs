use core::panic;
use std::vec;

use crate::asm::{self, Function, Section, SymbolType, Variable};
use crate::asm::{Directive, SectionType};
use crate::ir;
use crate::ir::Dtype;
use crate::Translate;
use lang_c::ast;

#[derive(Default)]
pub struct Asmgen {}

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
        label: &asm::Label,
        signature: &ir::FunctionSignature,
        definition: &Option<ir::FunctionDefinition>,
    ) -> Function {
        let section_header = self.create_function_section_header(label);
        dbg!(section_header, signature, definition);
        Function::new(Vec::new())
    }
}
