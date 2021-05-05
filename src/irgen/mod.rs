use core::fmt;
use std::collections::{BTreeMap, HashMap};
use std::mem;
use std::ops::Deref;

use failure::Fail;
use lang_c::ast::*;

use crate::ir::BlockExit;
use crate::*;
use crate::{ir::DtypeError, ir::FunctionSignature, write_base::WriteString};

#[derive(Default)]
pub struct Irgen {
    decls: BTreeMap<String, ir::Declaration>,
}

#[derive(Debug, PartialEq)]
pub struct IrgenError {
    pub code: String,
    pub message: IrgenErrorMessage,
}

#[derive(Debug, PartialEq, Fail)]
pub enum IrgenErrorMessage {
    #[fail(display = "{}", dtype_error)]
    InvalidDtype { dtype_error: DtypeError },
}

impl IrgenError {
    pub fn new(code: String, message: IrgenErrorMessage) -> Self {
        Self { code, message }
    }
}

impl fmt::Display for IrgenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IrgenError")
    }
}

impl Translate<TranslationUnit> for Irgen {
    type Target = ir::TranslationUnit;
    type Error = IrgenError;

    fn translate(&mut self, unit: &TranslationUnit) -> Result<Self::Target, Self::Error> {
        for decl in &unit.0 {
            match decl.node {
                ExternalDeclaration::Declaration(ref var) => self.add_declaration(&var.node)?,
                ExternalDeclaration::StaticAssert(_) => unimplemented!("static assert"),
                ExternalDeclaration::FunctionDefinition(ref func) => {
                    self.add_function_definition(&func.node)?
                }
            }
        }
        let decls = mem::replace(&mut self.decls, BTreeMap::new());
        Ok(Self::Target {
            decls,
            structs: HashMap::new(),
        })
    }
}

impl Irgen {
    fn add_declaration(&mut self, decl: &Declaration) -> Result<(), IrgenError> {
        let (dtype, is_typedef) = ir::Dtype::try_from_ast_declaration_specifiers(&decl.specifiers)
            .map_err(|e| {
                IrgenError::new(
                    decl.write_string(),
                    IrgenErrorMessage::InvalidDtype { dtype_error: e },
                )
            })?;
        println!("Dtype: {}, is_typedef: {}", dtype, is_typedef);
        // [ ] if `is_typedef` is true, resolve the real type;
        // [ ] create Variable::initializer if ast::Initializer exists
        //    [ ] get array of `declarator`s from decl
        //    [ ] for each `declarator` get it's `initdeclarator`, from which
        //        retrieve `initializer` as return value.
        Ok(())
    }

    fn add_function_definition(&mut self, func: &FunctionDefinition) -> Result<(), IrgenError> {
        let specifiers = &func.specifiers;
        let declarator = &func.declarator.node;
        let func_name = self.name_of_declarator(declarator);
        let (dtype, _is_typedef) = ir::Dtype::try_from_ast_declaration_specifiers(specifiers)
            .map_err(|e| {
                IrgenError::new(
                    func.declarator.write_string(),
                    IrgenErrorMessage::InvalidDtype { dtype_error: e },
                )
            })?;
        let dtype = dtype.with_ast_declarator(declarator).map_err(|e| {
            IrgenError::new(
                func.declarator.write_string(),
                IrgenErrorMessage::InvalidDtype { dtype_error: e },
            )
        })?;
        println!("dtype: {}", dtype.name().unwrap_or(&"unamed".to_owned()));
        // create a block which only contains a `return 0;` statement
        let value = ir::Constant::Int {
            value: 1,
            width: 4,
            is_signed: false,
        };
        let exit = BlockExit::Return {
            value: ir::Operand::Constant(value),
        };
        let block = ir::Block {
            phinodes: Vec::new(),
            instructions: Vec::new(),
            exit,
        };
        let mut blocks = BTreeMap::new();
        blocks.insert(ir::BlockId(0), block);
        let signature = FunctionSignature::new(dtype.deref().clone());
        let definition = Some(ir::FunctionDefinition {
            allocations: Vec::new(),
            blocks,
            bid_init: ir::BlockId(0),
        });
        let function = ir::Declaration::Function {
            signature,
            definition,
        };
        self.decls.insert(func_name, function);
        Ok(())
    }

    fn name_of_declarator(&self, declarator: &Declarator) -> String {
        declarator.kind.write_string()
    }
}
