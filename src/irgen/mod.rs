use core::fmt;
use std::mem;
use std::ops::Deref;
use std::{
    collections::{BTreeMap, HashMap},
    convert::TryFrom,
};

use failure::Fail;
use lang_c::ast::*;

use crate::ir::{BlockExit, BlockId, Dtype, Instruction, Named, Operand, RegisterId};
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

#[derive(Debug)]
struct BBContext {
    bid: BlockId,
    instructions: Vec<Named<Instruction>>,
}

impl BBContext {
    pub fn new(bid: BlockId) -> Self {
        Self {
            bid,
            instructions: Vec::new(),
        }
    }
}

#[derive(Debug)]
struct FunctionContext {
    // return_type
    allocations: Vec<Named<Dtype>>,
    blocks: BTreeMap<ir::BlockId, ir::Block>,
    bid_counter: usize,
    tempid_counter: usize,
    // typedefs
    symbol_table: Vec<String>,
    curr_block: BBContext,
}

impl FunctionContext {
    fn new() -> Self {
        Self {
            allocations: Vec::new(),
            blocks: BTreeMap::new(),
            bid_counter: 1,
            tempid_counter: 0,
            symbol_table: Vec::new(),
            curr_block: BBContext::new(BlockId(1)),
        }
    }

    #[allow(dead_code)]
    fn alloc_bid(&mut self) -> BlockId {
        let bid = BlockId(self.bid_counter);
        self.bid_counter += 1;
        bid
    }
}
#[derive(Debug, PartialEq, Fail)]
pub enum IrgenErrorMessage {
    #[fail(display = "{}", message)]
    Misc { message: String },
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

        let signature = FunctionSignature::new(dtype.deref().clone());
        let mut func_ctx = FunctionContext::new();
        self.translate_stmt(&func.statement.node, &mut func_ctx)?;

        let definition = Some(ir::FunctionDefinition {
            allocations: func_ctx.allocations,
            blocks: func_ctx.blocks,
            bid_init: ir::BlockId(0),
        });
        self.decls.insert(
            func_name,
            ir::Declaration::Function {
                signature,
                definition,
            },
        );
        Ok(())
    }

    fn name_of_declarator(&self, declarator: &Declarator) -> String {
        declarator.kind.write_string()
    }

    fn translate_stmt(
        &self,
        statement: &Statement,
        func_ctx: &mut FunctionContext,
    ) -> Result<(), IrgenError> {
        match statement {
            Statement::Labeled(_) => {}
            Statement::Compound(block_items) => {
                for block_item in block_items {
                    self.translate_block_item(&block_item.node, func_ctx)?;
                }
            }
            Statement::Expression(_) => {}
            Statement::If(_) => {}
            Statement::Switch(_) => {}
            Statement::While(_) => {}
            Statement::DoWhile(_) => {}
            Statement::For(_) => {}
            Statement::Goto(_) => {}
            Statement::Continue => {}
            Statement::Break => {}
            Statement::Return(return_stmt) => {
                if let Some(expr) = return_stmt {
                    let value = self.translate_expression(&expr.deref().node, func_ctx)?;
                    let exit = BlockExit::Return { value };
                    let block = ir::Block {
                        phinodes: Vec::new(),
                        instructions: std::mem::replace(
                            &mut func_ctx.curr_block.instructions,
                            Vec::new(),
                        ),
                        exit,
                    };
                    func_ctx.blocks.insert(func_ctx.curr_block.bid, block);
                    println!("{:#?}", func_ctx);
                }
            }
            Statement::Asm(_) => {}
        }
        Ok(())
    }

    fn translate_block_item(
        &self,
        block_item: &BlockItem,
        func_ctx: &mut FunctionContext,
    ) -> Result<(), IrgenError> {
        match block_item {
            BlockItem::Declaration(ref decl) => {
                self.translate_declaration(&decl.node, func_ctx)?;
            }
            BlockItem::StaticAssert(_) => unimplemented!("static assert"),
            BlockItem::Statement(stmt) => {
                self.translate_stmt(&stmt.node, func_ctx)?;
            }
        }
        Ok(())
    }

    fn translate_declaration(
        &self,
        decl: &Declaration,
        func_ctx: &mut FunctionContext,
    ) -> Result<(), IrgenError> {
        let (dtype, is_typedef) = ir::Dtype::try_from_ast_declaration_specifiers(&decl.specifiers)
            .map_err(|e| {
                IrgenError::new(
                    decl.write_string(),
                    IrgenErrorMessage::InvalidDtype { dtype_error: e },
                )
            })?;
        println!("Dtype: {}, is_typedef: {}", dtype, is_typedef);
        for declarator in &decl.declarators {
            let name = declarator.node.declarator.write_string();

            let allocation = Named::new(Some(name), dtype.clone());
            func_ctx.allocations.push(allocation);
            let aid = func_ctx.allocations.len() - 1;
            let rid = RegisterId::Local { aid };
            let ptr = Operand::register(rid, Dtype::pointer(dtype.clone()));

            // translate initializer
            if let Some(ref initializer) = declarator.node.initializer {
                match initializer.node {
                    Initializer::Expression(ref expr) => {
                        let operand = self.translate_expression(&expr.deref().node, func_ctx)?;
                        let instr = Instruction::Store {
                            ptr,
                            value: operand,
                        };
                        func_ctx
                            .curr_block
                            .instructions
                            .push(Named::new(Some("abc".to_owned()), instr));
                    }
                    Initializer::List(_) => unimplemented!("list initializer"),
                }
            }
        }
        Ok(())
    }

    fn translate_expression(
        &self,
        expression: &Expression,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        match expression {
            Expression::Identifier(id) => self.translate_identifier(&id.deref().node, func_ctx),
            Expression::Constant(const_value) => ir::Constant::try_from(&const_value.deref().node)
                .map(|val| Operand::Constant(val))
                .map_err(|_| {
                    IrgenError::new(
                        const_value.write_string(),
                        IrgenErrorMessage::Misc {
                            message: "invlaid constant".to_owned(),
                        },
                    )
                }),
            Expression::StringLiteral(_) => todo!("string literal"),
            Expression::GenericSelection(_) => todo!("generic selection"),
            Expression::Member(_) => todo!("member"),
            Expression::Call(_) => todo!("call"),
            Expression::CompoundLiteral(_) => todo!("compound literal"),
            Expression::SizeOf(_) => todo!("sizeof"),
            Expression::AlignOf(_) => todo!("alignof"),
            Expression::UnaryOperator(unary) => {
                self.translate_unary_operator(&unary.deref().node, func_ctx)
            }
            Expression::Cast(_) => todo!("cast"),
            Expression::BinaryOperator(bop) => {
                self.translate_binary_operator(&bop.deref().node, func_ctx)
            }
            Expression::Conditional(_) => todo!("conditional"),
            Expression::Comma(_) => todo!("comma"),
            Expression::OffsetOf(_) => todo!("offsetof"),
            Expression::VaArg(_) => todo!("vaarg"),
            Expression::Statement(_) => todo!("stmt"),
        }
    }

    fn translate_unary_operator(
        &self,
        unary: &UnaryOperatorExpression,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        // load %l0:u8* reg:tmp:1
        // reg:tmp:2 = sub reg:tmp1 const:1
        // store reg:tmp:2 %l0:u8*
        let op = unary.operator.node.clone();
        let operand = self.translate_expression(&unary.operand.node, func_ctx)?;
        println!("operand: {:?}", operand);
        let (rid, dtype) = match operand {
            Operand::Constant(_) => {
                return Err(IrgenError::new(
                    unary.write_string(),
                    IrgenErrorMessage::Misc {
                        message: "unary operator on const value is illegal".to_owned(),
                    },
                ))
            }
            Operand::Register {
                ref rid, ref dtype, ..
            } => (rid, dtype.clone()),
        };
        match op {
            // UnaryOperator::PostIncrement => {}
            // UnaryOperator::PostDecrement => {}
            // UnaryOperator::PreIncrement => {}
            UnaryOperator::PreDecrement => {
                let one = Operand::constant(ir::Constant::Int {
                    value: 1,
                    width: 8,
                    is_signed: false,
                });
                let sub_instr = Instruction::BinOp {
                    op: BinaryOperator::Minus,
                    lhs: operand,
                    rhs: one,
                    dtype,
                };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, sub_instr));

                let iid = func_ctx.curr_block.instructions.len() - 1;
                let res_operand = RegisterId::temp(func_ctx.curr_block.bid, iid);
                let store_instr = Instruction::Store {

                }
            }
            // UnaryOperator::Address => {}
            // UnaryOperator::Indirection => {}
            // UnaryOperator::Plus => {}
            // UnaryOperator::Minus => {}
            // UnaryOperator::Complement => {}
            // UnaryOperator::Negate => {}
            // UnaryOperator::SizeOf => {}
            _ => todo!("more unary operators"),
        }

        let store_instr = Instruction::UnaryOp {
            op,
            operand: operand.clone(),
            dtype,
        };
        func_ctx
            .curr_block
            .instructions
            .push(Named::new(None, instr));
        Ok(operand)
    }

    fn translate_binary_operator(
        &self,
        binary: &BinaryOperatorExpression,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        let _op = binary.operator.node.clone();
        let _rhs = self.translate_expression(&binary.rhs.node, func_ctx)?;
        let lhs = self.translate_expression(&binary.lhs.node, func_ctx)?;
        // let instr = Instruction::BinOp { op, lhs, rhs };
        Ok(lhs)
    }

    fn translate_identifier(
        &self,
        _identifier: &Identifier,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        // TODO: lookup identifier name form symbol table.
        let aid = func_ctx.allocations.len() - 1;
        let dtype = &func_ctx.allocations[aid];
        let rid = RegisterId::Local { aid };
        let ptr = Operand::register(rid, Dtype::pointer(dtype.deref().clone()));

        let instr = Instruction::Load { ptr: ptr.clone() };
        func_ctx
            .curr_block
            .instructions
            .push(Named::new(None, instr));

        Ok(ptr)
    }
}
