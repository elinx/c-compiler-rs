use core::{fmt, panic};
use std::cmp;
use std::mem;
use std::ops::Deref;
use std::{
    collections::{BTreeMap, HashMap},
    convert::TryFrom,
};

use failure::Fail;
use lang_c::ast::*;

use crate::ir::{
    BlockExit, BlockId, Dtype, HasDtype, Instruction, JumpArg, Named, Operand, RegisterId,
};
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
    phinodes: Vec<Named<Dtype>>,
}

impl BBContext {
    pub fn new(bid: BlockId) -> Self {
        Self {
            bid,
            instructions: Vec::new(),
            phinodes: Vec::new(),
        }
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
#[derive(Debug)]
struct FunctionContext {
    return_dtype: Dtype,
    allocations: Vec<Named<Dtype>>,
    blocks: BTreeMap<ir::BlockId, ir::Block>,
    bid_counter: usize,
    tempid_counter: usize,
    // typedefs
    symbol_table: Vec<HashMap<String, Operand>>,
    curr_block: BBContext,
}

impl FunctionContext {
    fn new(ret_dtype: Dtype, global: HashMap<String, Operand>) -> Self {
        Self {
            return_dtype: ret_dtype,
            allocations: Vec::new(),
            blocks: BTreeMap::new(),
            bid_counter: 1, // 0 is used by init block by default
            tempid_counter: 0,
            symbol_table: vec![global],
            curr_block: BBContext::new(BlockId(0)),
        }
    }

    fn alloc_bid(&mut self) -> BlockId {
        let bid = BlockId(self.bid_counter);
        self.bid_counter += 1;
        bid
    }

    fn alloc_tempid(&mut self) -> usize {
        let tid = self.tempid_counter;
        self.tempid_counter += 1;
        tid
    }

    fn enter_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.symbol_table.pop().unwrap();
    }

    fn lookup_symbol_table(&mut self, symbol: String) -> Result<Operand, IrgenError> {
        log::debug!(
            "symbol_table: {:?}, symbol: {:?}",
            &self.symbol_table,
            &symbol
        );
        self.symbol_table
            .iter()
            .rev()
            .find(|symbol_table| symbol_table.contains_key(&symbol))
            .ok_or(IrgenError::new(
                symbol.clone(),
                IrgenErrorMessage::Misc {
                    message: "can not find symbol in vector fo symbol table".to_owned(),
                },
            ))?
            .get(&symbol)
            .ok_or(IrgenError::new(
                symbol,
                IrgenErrorMessage::Misc {
                    message: "can not find symbol hash map".to_owned(),
                },
            ))
            .map(|operand| operand.clone())
    }

    fn insert_symbol_table_entry(
        &mut self,
        symbol: String,
        operand: Operand,
    ) -> Result<(), IrgenError> {
        let status = self
            .symbol_table
            .last_mut()
            .ok_or(IrgenError::new(
                symbol.clone(),
                IrgenErrorMessage::Misc {
                    message: "empty symbol talbe".to_owned(),
                },
            ))?
            .insert(symbol.clone(), operand);
        if status.is_some() {
            return Err(IrgenError::new(
                symbol,
                IrgenErrorMessage::Misc {
                    message: "duplicated symbol entry in same scope".to_owned(),
                },
            ));
        }
        Ok(())
    }

    fn alloc_tmp(&mut self, name: String, dtype: Dtype) -> Result<Operand, IrgenError> {
        let tempid = self.alloc_tempid();
        let name = name + &tempid.to_string();
        let allocation = Named::new(Some(name.clone()), dtype.clone());
        self.allocations.push(allocation);
        let aid = self.allocations.len() - 1;
        let rid = RegisterId::Local { aid };
        let operand = Operand::register(rid.clone(), Dtype::pointer(dtype.clone()));
        // self.insert_symbol_table_entry(name.clone(), operand.clone())
        //     .unwrap(); // TODO: better way to throw error
        Ok(operand)
    }
}

impl Translate<TranslationUnit> for Irgen {
    type Target = ir::TranslationUnit;
    type Error = IrgenError;

    fn translate(&mut self, unit: &TranslationUnit) -> Result<Self::Target, Self::Error> {
        // env_logger::init();
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
        let (base_dtype, is_typedef) =
            ir::Dtype::try_from_ast_declaration_specifiers(&decl.specifiers).map_err(|e| {
                IrgenError::new(
                    decl.write_string(),
                    IrgenErrorMessage::InvalidDtype { dtype_error: e },
                )
            })?;
        log::debug!(
            "global variable base_dtype: {:?}, is_typedef: {}",
            base_dtype,
            is_typedef
        );
        for declarator in &decl.declarators {
            let initializer = declarator.node.initializer.as_ref();
            let declarator = &declarator.node.declarator.node;
            let name = self.name_of_declarator(&declarator);
            let dtype = base_dtype
                .clone()
                .with_ast_declarator(&declarator)
                .map_err(|e| {
                    IrgenError::new(
                        declarator.write_string(),
                        IrgenErrorMessage::InvalidDtype { dtype_error: e },
                    )
                })?
                .deref()
                .clone();

            let initializer = if let Some(init) = initializer {
                Some(init.node.clone())
            } else {
                None
            };
            self.decls
                .insert(name, ir::Declaration::Variable { dtype, initializer });
        }
        log::debug!("self.decls: {:?}", self.decls);
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
        let (ret_dtype, _is_typedef) = ir::Dtype::try_from_ast_declaration_specifiers(specifiers)
            .map_err(|e| {
            IrgenError::new(
                func.declarator.write_string(),
                IrgenErrorMessage::InvalidDtype { dtype_error: e },
            )
        })?;
        log::debug!("function dtype: {:?}", ret_dtype);

        // only return type is fetched from specifiers, try to get more information for
        // the declarators
        let dtype = ret_dtype
            .clone()
            .with_ast_declarator(declarator)
            .map_err(|e| {
                IrgenError::new(
                    func.declarator.write_string(),
                    IrgenErrorMessage::InvalidDtype { dtype_error: e },
                )
            })?;
        log::debug!("function dtype: {:?}", dtype);

        // create global symbol table and insert global variables and functions into global symbol,
        // note that the global and function definitions are added linearly which means only the former
        // added functions and variables could be found through looking up the global symbol table.
        let mut global_symbol_table = HashMap::new();
        self.decls.iter().for_each(|(name, decl)| {
            let dtype = decl.dtype();
            let pointer = ir::Constant::global_variable(name.clone(), dtype);
            let operand = ir::Operand::constant(pointer);
            global_symbol_table.insert(name.clone(), operand);
            // TODO: check duplications for new symbol
        });
        global_symbol_table.insert(
            func_name.clone(),
            Operand::Constant(ir::Constant::global_variable(
                func_name.clone(),
                dtype.deref().clone(),
            )),
        );

        let signature = FunctionSignature::new(dtype.deref().clone());
        let mut func_ctx = FunctionContext::new(
            dtype.deref().get_function_inner().unwrap().0.clone(),
            global_symbol_table,
        );

        let params_name = self.params_name_of_declarator(declarator);
        func_ctx.enter_scope();
        self.translate_params(&signature.params, &params_name, &mut func_ctx)?;
        self.translate_stmt(&func.statement.node, &mut func_ctx)?;
        func_ctx.enter_scope();

        // add an extra block for `main` function served as default return value
        let value = if ret_dtype == ir::Dtype::unit() {
            ir::Operand::constant(ir::Constant::unit())
        } else if ret_dtype == Dtype::INT {
            if func_name == "main" {
                ir::Operand::constant(ir::Constant::int(0 as u128, Dtype::INT))
            } else {
                ir::Operand::constant(ir::Constant::undef(ret_dtype))
            }
        } else {
            ir::Operand::constant(ir::Constant::undef(ret_dtype))
        };
        let block = ir::Block {
            phinodes: Vec::new(),
            instructions: Vec::new(),
            exit: BlockExit::Return { value },
        };
        let bid = if func_ctx.blocks.is_empty() {
            func_ctx.curr_block.bid
        } else {
            func_ctx.alloc_bid()
        };
        func_ctx.blocks.insert(bid, block);

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

    fn translate_params(
        &self,
        params: &[Dtype],
        params_name: &[String],
        func_ctx: &mut FunctionContext,
    ) -> Result<(), IrgenError> {
        params
            .iter()
            .zip(params_name.iter())
            .for_each(|(dtype, name)| {
                let allocation = Named::new(Some(name.clone()), dtype.clone());
                func_ctx.allocations.push(allocation);
                let aid = func_ctx.allocations.len() - 1;
                let rid = RegisterId::Local { aid };
                let ptr = Operand::register(rid.clone(), Dtype::pointer(dtype.clone()));
                func_ctx
                    .insert_symbol_table_entry(name.clone(), ptr.clone())
                    .unwrap(); // TODO: better way to throw error

                // initialize allocated memory by store parameter value into it.
                let phinode = Named::new(Some(name.clone()), dtype.clone());
                func_ctx.curr_block.phinodes.push(phinode);

                // store value from phinode into stack pointer(not from local register)
                let rid = RegisterId::Arg {
                    bid: func_ctx.curr_block.bid,
                    aid,
                };
                let value = Operand::register(rid, dtype.clone());
                let instr = Instruction::Store { ptr, value };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, instr));
            });
        Ok(())
    }

    fn name_of_declarator(&self, declarator: &Declarator) -> String {
        log::debug!("declarator: {:?}", &declarator);
        match &declarator.kind.node {
            DeclaratorKind::Abstract => "".to_owned(),
            DeclaratorKind::Identifier(id) => id.write_string(),
            DeclaratorKind::Declarator(decl) => decl.write_string(),
        }
    }

    fn params_name_of_declarator(&self, declarator: &Declarator) -> Vec<String> {
        for derived_decl in &declarator.derived {
            match &derived_decl.node {
                DerivedDeclarator::Function(func_decl) => {
                    return func_decl
                        .node
                        .parameters
                        .iter()
                        .map(|p| p.node.declarator.as_ref().unwrap().node.kind.write_string())
                        .collect::<Vec<_>>();
                }
                DerivedDeclarator::KRFunction(_) => {
                    return Vec::new();
                }

                DerivedDeclarator::Pointer(_) => {}
                DerivedDeclarator::Array(_) => {}
            }
            // TODO: can I safely return from the loop with lose any information?
        }
        Vec::new()
    }

    fn insert_instruction(
        &self,
        instruction: ir::Instruction,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        func_ctx
            .curr_block
            .instructions
            .push(Named::new(None, instruction.clone()));

        let iid = func_ctx.curr_block.instructions.len() - 1;
        let bid = func_ctx.curr_block.bid;
        let rid = RegisterId::Temp { bid, iid };
        let operand = Operand::Register {
            rid,
            dtype: instruction.dtype().clone(),
        };
        Ok(operand)
    }

    fn translate_typecast_to_bool(
        &self,
        condition: &Operand,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        if condition.dtype() != Dtype::BOOL {
            self.insert_instruction(
                ir::Instruction::BinOp {
                    op: BinaryOperator::NotEquals,
                    lhs: condition.clone(),
                    rhs: Operand::Constant(ir::Constant::int(0 as u128, condition.dtype().clone())),
                    dtype: Dtype::BOOL,
                },
                func_ctx,
            )
        } else {
            Ok(condition.clone())
        }
    }

    fn translate_conditional(
        &self,
        expr: &ConditionalExpression,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        let then_bid = func_ctx.alloc_bid();
        let else_bid = func_ctx.alloc_bid();
        let exit_bid = func_ctx.alloc_bid();

        let condition = &expr.condition.deref().node;
        self.translate_condition(condition, then_bid, else_bid, func_ctx)?;

        std::mem::replace(&mut func_ctx.curr_block, BBContext::new(then_bid));
        let then_expr = &expr.then_expression.deref().node;
        let then_operand = self.translate_expression_rvalue(then_expr, func_ctx)?;
        self.insert_jump_block(exit_bid, func_ctx)?;

        std::mem::replace(&mut func_ctx.curr_block, BBContext::new(else_bid));
        let else_expr = &expr.else_expression.deref().node;
        let else_operand = self.translate_expression_rvalue(else_expr, func_ctx)?;
        self.insert_jump_block(exit_bid, func_ctx)?;

        let dtype = self.translate_merge_type(&then_operand.dtype(), &else_operand.dtype())?;
        let res = func_ctx.alloc_tmp("t".to_owned(), dtype)?;

        // store result from then/else to result
        func_ctx
            .blocks
            .get_mut(&then_bid)
            .unwrap()
            .instructions
            .push(Named::new(
                None,
                ir::Instruction::Store {
                    ptr: res.clone(),
                    value: then_operand,
                },
            ));
        func_ctx
            .blocks
            .get_mut(&else_bid)
            .unwrap()
            .instructions
            .push(Named::new(
                None,
                ir::Instruction::Store {
                    ptr: res.clone(),
                    value: else_operand,
                },
            ));

        std::mem::replace(&mut func_ctx.curr_block, BBContext::new(exit_bid));
        Ok(self.insert_instruction(ir::Instruction::Load { ptr: res.clone() }, func_ctx)?)
    }

    /// Add Exit block to previous basic block, and connect to newly created block `then`
    /// and `else`
    fn translate_condition(
        &self,
        cond: &Expression,
        bid_then: BlockId,
        bid_else: BlockId,
        func_ctx: &mut FunctionContext,
    ) -> Result<(), IrgenError> {
        let condition = self.translate_expression_rvalue(cond, func_ctx)?;
        let condition = self.translate_typecast_to_bool(&condition, func_ctx)?;
        let exit = BlockExit::ConditionalJump {
            condition,
            arg_then: Box::new(ir::JumpArg::new(bid_then, Vec::new())),
            arg_else: Box::new(ir::JumpArg::new(bid_else, Vec::new())),
        };
        let block = ir::Block {
            phinodes: std::mem::replace(&mut func_ctx.curr_block.phinodes, Vec::new()),
            instructions: std::mem::replace(&mut func_ctx.curr_block.instructions, Vec::new()),
            exit,
        };
        func_ctx.blocks.insert(func_ctx.curr_block.bid, block);

        Ok(())
    }

    fn insert_jump_block(
        &self,
        bid_end: BlockId,
        func_ctx: &mut FunctionContext,
    ) -> Result<(), IrgenError> {
        let block = ir::Block {
            phinodes: std::mem::replace(&mut func_ctx.curr_block.phinodes, Vec::new()),
            instructions: std::mem::replace(&mut func_ctx.curr_block.instructions, Vec::new()),
            exit: BlockExit::Jump {
                arg: ir::JumpArg::new(bid_end, Vec::new()),
            },
        };
        func_ctx.blocks.insert(func_ctx.curr_block.bid, block);
        Ok(())
    }

    fn translate_for_initializer(
        &self,
        init: &ForInitializer,
        cond_bid: BlockId,
        func_ctx: &mut FunctionContext,
    ) -> Result<(), IrgenError> {
        Ok(match init {
            ForInitializer::Empty => {}
            ForInitializer::Expression(_) => {}
            ForInitializer::Declaration(decl) => {
                self.translate_declaration(&decl.node, func_ctx)?;
                self.insert_jump_block(cond_bid, func_ctx)?
            }
            ForInitializer::StaticAssert(_) => {}
        })
    }

    fn translate_stmt(
        &self,
        statement: &Statement,
        func_ctx: &mut FunctionContext,
    ) -> Result<(), IrgenError> {
        match statement {
            Statement::Labeled(_) => todo!("labeled"),
            Statement::Compound(block_items) => {
                func_ctx.enter_scope();
                for block_item in block_items {
                    self.translate_block_item(&block_item.node, func_ctx)?;
                }
                func_ctx.exit_scope();
            }
            Statement::Expression(expr) => {
                log::debug!("expr stmt {}, {:?}", expr.write_string(), expr);
                if let Some(expression) = expr {
                    self.translate_expression_rvalue(&expression.deref().node, func_ctx)?;
                }
            }
            Statement::If(if_stmt) => {
                let bid_then = func_ctx.alloc_bid();
                let bid_else = func_ctx.alloc_bid();
                let bid_end = func_ctx.alloc_bid();
                self.translate_condition(
                    &if_stmt.node.condition.node,
                    bid_then,
                    bid_else,
                    func_ctx,
                )?;

                std::mem::replace(&mut func_ctx.curr_block, BBContext::new(bid_then));
                self.translate_stmt(&if_stmt.node.then_statement.node, func_ctx)?;
                // Do I need to insert exit block in the end? because then block may contains
                // a return statement which has added a BlockExit already.
                self.insert_jump_block(bid_end, func_ctx)?;

                if let Some(ref else_stmt) = if_stmt.node.else_statement {
                    std::mem::replace(&mut func_ctx.curr_block, BBContext::new(bid_else));
                    self.translate_stmt(&else_stmt.deref().node, func_ctx)?;
                    self.insert_jump_block(bid_end, func_ctx)?;
                } else {
                    // create a block which only contains one jump instruction
                    let else_contex = BBContext::new(bid_else);
                    std::mem::replace(&mut func_ctx.curr_block, else_contex);
                    let jump_arg = ir::JumpArg::new(bid_end, Vec::new());
                    let block = ir::Block {
                        phinodes: std::mem::replace(&mut func_ctx.curr_block.phinodes, Vec::new()),
                        instructions: std::mem::replace(
                            &mut func_ctx.curr_block.instructions,
                            Vec::new(),
                        ),
                        exit: BlockExit::Jump { arg: jump_arg },
                    };
                    func_ctx.blocks.insert(func_ctx.curr_block.bid, block);
                }

                let end_contex = BBContext::new(bid_end);
                std::mem::replace(&mut func_ctx.curr_block, end_contex);
            }
            Statement::Switch(_) => todo!("switch"),
            Statement::While(while_stmt) => {
                let cond = &while_stmt.node.expression.deref().node;
                let statement = &while_stmt.node.statement.deref().node;
                let cond_bid = func_ctx.alloc_bid();
                let body_bid = func_ctx.alloc_bid();
                let exit_bid = func_ctx.alloc_bid();

                // cond block
                self.insert_jump_block(cond_bid, func_ctx)?;
                std::mem::replace(&mut func_ctx.curr_block, BBContext::new(cond_bid));
                self.translate_condition(cond, body_bid, exit_bid, func_ctx)?;

                // body statements
                std::mem::replace(&mut func_ctx.curr_block, BBContext::new(body_bid));
                self.translate_stmt(statement, func_ctx)?;
                self.insert_jump_block(cond_bid, func_ctx)?;

                // exit block
                std::mem::replace(&mut func_ctx.curr_block, BBContext::new(exit_bid));
            }
            Statement::DoWhile(_) => todo!("do-while"),
            Statement::For(for_stmt) => {
                let init_bid = func_ctx.alloc_bid();
                let cond_bid = func_ctx.alloc_bid();
                let body_bid = func_ctx.alloc_bid();
                let step_bid = func_ctx.alloc_bid();
                let exit_bid = func_ctx.alloc_bid();

                // initializer in a seperate block
                self.insert_jump_block(init_bid, func_ctx)?;
                func_ctx.enter_scope();
                std::mem::replace(&mut func_ctx.curr_block, BBContext::new(init_bid));
                self.translate_for_initializer(
                    &for_stmt.node.initializer.node,
                    cond_bid,
                    func_ctx,
                )?;
                if let Some(ref cond) = for_stmt.node.condition {
                    std::mem::replace(&mut func_ctx.curr_block, BBContext::new(cond_bid));
                    self.translate_condition(&cond.deref().node, body_bid, exit_bid, func_ctx)?;

                    std::mem::replace(&mut func_ctx.curr_block, BBContext::new(body_bid));
                    self.translate_stmt(&for_stmt.node.statement.deref().node, func_ctx)?;

                    if let Some(step) = &for_stmt.node.step {
                        self.insert_jump_block(step_bid, func_ctx)?;
                        std::mem::replace(&mut func_ctx.curr_block, BBContext::new(step_bid));
                        self.translate_expression_rvalue(&step.deref().node, func_ctx)?;
                        self.insert_jump_block(cond_bid, func_ctx)?;
                    } else {
                        self.insert_jump_block(cond_bid, func_ctx)?;
                    }
                } else {
                    self.insert_jump_block(exit_bid, func_ctx)?;
                }
                func_ctx.exit_scope();
                std::mem::replace(&mut func_ctx.curr_block, BBContext::new(exit_bid));
            }
            Statement::Goto(_) => todo!("goto"),
            Statement::Continue => todo!("continue"),
            Statement::Break => todo!("break"),
            Statement::Return(return_stmt) => {
                if let Some(expr) = return_stmt {
                    log::debug!("handing return expression: {:?}", &func_ctx);
                    let value = self.translate_expression_rvalue(&expr.deref().node, func_ctx)?;
                    let value =
                        self.translate_typecast(&value, &func_ctx.return_dtype.clone(), func_ctx)?;
                    let exit = BlockExit::Return { value };
                    let block = ir::Block {
                        phinodes: std::mem::replace(&mut func_ctx.curr_block.phinodes, Vec::new()),
                        instructions: std::mem::replace(
                            &mut func_ctx.curr_block.instructions,
                            Vec::new(),
                        ),
                        exit,
                    };
                    func_ctx.blocks.insert(func_ctx.curr_block.bid, block);
                }
            }
            Statement::Asm(_) => todo!("asm"),
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
        log::debug!("Dtype: {}, is_typedef: {}", dtype, is_typedef);
        for declarator in &decl.declarators {
            let name = declarator.node.declarator.node.kind.write_string();
            let dtype = dtype
                .clone()
                .with_ast_declarator(&declarator.node.declarator.node)
                .map_err(|e| {
                    IrgenError::new(
                        declarator.write_string(),
                        IrgenErrorMessage::InvalidDtype { dtype_error: e },
                    )
                })?;
            let dtype = dtype.deref();

            let allocation = Named::new(Some(name.clone()), dtype.clone());
            func_ctx.allocations.push(allocation);
            let aid = func_ctx.allocations.len() - 1;
            let rid = RegisterId::Local { aid };
            let ptr = Operand::register(rid, Dtype::pointer(dtype.clone()));
            func_ctx.insert_symbol_table_entry(name.clone(), ptr.clone())?;

            // translate initializer
            if let Some(ref initializer) = declarator.node.initializer {
                match initializer.node {
                    Initializer::Expression(ref expr) => {
                        let operand =
                            self.translate_expression_rvalue(&expr.deref().node, func_ctx)?;
                        let operand = self.translate_typecast(&operand, &dtype, func_ctx)?;
                        let instr = Instruction::Store {
                            ptr,
                            value: operand,
                        };
                        func_ctx
                            .curr_block
                            .instructions
                            .push(Named::new(None, instr));
                    }
                    Initializer::List(_) => unimplemented!("list initializer"),
                }
            }
        }
        Ok(())
    }

    // take a look at the following links for better understanding of typecast:
    //   1. [[Homework 2] Type and typecasting #88](https://github.com/kaist-cp/cs420/issues/88)
    //   2. [[Homework 2] Tips on arithmetic operators](https://github.com/kaist-cp/cs420/issues/106)
    fn translate_typecast(
        &self,
        operand: &Operand,
        target_dtype: &Dtype,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        log::debug!(
            "trying to translateing operand from `{:?}` to dtype: `{:?}`",
            operand,
            target_dtype
        );
        // convert to target format, like `u64 0` to `u8 0`
        if operand.dtype() == target_dtype.clone() {
            log::debug!("typecast skip");
            return Ok(operand.clone());
        }

        match operand {
            Operand::Constant(const_val) => {
                // the original HasDtype Trait for global vairable return a pointer type,
                // work around by return it's origianl dtype.
                let dtype = match const_val {
                    ir::Constant::GlobalVariable { dtype, .. } => dtype.clone(),
                    _ => const_val.dtype().clone(),
                };
                if dtype == target_dtype.clone() {
                    log::debug!("typecast skip");
                    return Ok(operand.clone());
                }
            }
            _ => {}
        };
        // create a new instruction to do typecast
        let instr = Instruction::TypeCast {
            // value: Operand::constant(target_operand),
            value: operand.clone(),
            target_dtype: target_dtype.clone(),
        };
        func_ctx
            .curr_block
            .instructions
            .push(Named::new(None, instr));

        // return the new register allocated by TypeCast instruction
        let iid = func_ctx.curr_block.instructions.len() - 1;
        let bid = func_ctx.curr_block.bid;
        let rid = RegisterId::Temp { bid, iid };
        let operand = Operand::Register {
            rid,
            dtype: target_dtype.clone(),
        };
        log::debug!("typecast ok");
        Ok(operand)
    }

    fn translate_expression_lvalue(
        &self,
        expression: &Expression,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        match expression {
            Expression::Identifier(id) => {
                func_ctx.lookup_symbol_table(id.deref().node.name.to_string())
            }
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
            Expression::Call(_) => todo!("call expr as lvalue"),
            Expression::CompoundLiteral(_) => todo!("compound literal"),
            Expression::SizeOf(_) => todo!("sizeof"),
            Expression::AlignOf(_) => todo!("alignof"),
            Expression::UnaryOperator(unary) => {
                match &unary.node.operator.node {
                    UnaryOperator::Indirection => {
                        self.translate_expression_rvalue(&unary.node.operand.node, func_ctx)
                    }
                    _ => Err(IrgenError::new(
                        expression.write_string(),
                        IrgenErrorMessage::Misc {
                            message: "only *ptr could be served as lvalue".to_owned(),
                        },
                    )),
                }
                // self.translate_unary_operator(&unary.deref().node, func_ctx)
            }
            Expression::Cast(_) => todo!("cast"),
            Expression::BinaryOperator(_) => todo!("bin expr"),
            Expression::Conditional(_) => todo!("conditional"),
            Expression::Comma(_) => todo!("comma"),
            Expression::OffsetOf(_) => todo!("offsetof"),
            Expression::VaArg(_) => todo!("vaarg"),
            Expression::Statement(_) => todo!("stmt"),
        }
    }

    fn translate_expression_rvalue(
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
            Expression::Call(call) => self.translate_call_expression(&call.deref().node, func_ctx),
            Expression::CompoundLiteral(_) => todo!("compound literal"),
            Expression::SizeOf(expr) | Expression::AlignOf(expr) => {
                let dtype = Dtype::try_from(&expr.node).map_err(|e| {
                    IrgenError::new(
                        expr.write_string(),
                        IrgenErrorMessage::InvalidDtype { dtype_error: e },
                    )
                })?;
                let (size_of, align_of) = dtype.size_align_of(&HashMap::new()).map_err(|e| {
                    IrgenError::new(
                        expr.write_string(),
                        IrgenErrorMessage::InvalidDtype { dtype_error: e },
                    )
                })?;
                let value = match expression {
                    Expression::SizeOf(_) => size_of,
                    Expression::AlignOf(_) => align_of,
                    _ => panic!("something strange happend"),
                };
                Ok(ir::Operand::constant(ir::Constant::int(
                    value as u128,
                    ir::Dtype::LONG,
                )))
            }
            Expression::UnaryOperator(unary) => {
                self.translate_unary_operator(&unary.deref().node, func_ctx)
            }
            Expression::Cast(_) => todo!("cast"),
            Expression::BinaryOperator(bop) => {
                self.translate_binary_operator(&bop.deref().node, func_ctx)
            }
            Expression::Conditional(expr) => {
                self.translate_conditional(&expr.deref().node, func_ctx)
            }
            Expression::Comma(_) => todo!("comma"),
            Expression::OffsetOf(_) => todo!("offsetof"),
            Expression::VaArg(_) => todo!("vaarg"),
            Expression::Statement(_) => todo!("stmt"),
        }
    }

    fn translate_call_expression(
        &self,
        call: &CallExpression,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        let callee = self.translate_expression_rvalue(&call.callee.deref().node, func_ctx)?;
        let mut args = Vec::new();
        for argument in &call.arguments {
            let argument = &argument.node;
            let arg = self.translate_expression_rvalue(&argument, func_ctx)?;
            args.push(arg.clone());
        }
        let return_type = func_ctx.lookup_symbol_table(call.callee.deref().write_string())?;
        // log::debug!("call expression function type: {:?}", function_type);
        // let return_type = function_type
        //     .dtype()
        //     .get_pointer_inner()
        //     .ok_or(IrgenError::new(
        //         call.write_string(),
        //         IrgenErrorMessage::Misc {
        //             message: "invlaid function type".to_owned(),
        //         },
        //     ))?
        //     .get_function_inner()
        //     .ok_or(IrgenError::new(
        //         call.write_string(),
        //         IrgenErrorMessage::Misc {
        //             message: "invlaid function type".to_owned(),
        //         },
        //     ))?
        //     .0
        //     .clone();

        log::debug!("call expression return type: {:?}", return_type);
        let return_type = return_type
            .get_constant()
            .map_or(ir::Constant::unit(), |c| c.clone())
            .get_function_ret()
            .unwrap();
        let instr = Instruction::Call {
            callee,
            args,
            return_type: return_type.clone(),
        };
        func_ctx
            .curr_block
            .instructions
            .push(Named::new(None, instr));
        let iid = func_ctx.curr_block.instructions.len() - 1;
        let bid = func_ctx.curr_block.bid;
        let rid = RegisterId::Temp { bid, iid };
        let operand = Operand::Register {
            rid,
            dtype: return_type.clone(),
        };
        Ok(operand)
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

        match op {
            UnaryOperator::PostIncrement => {
                let operand = self.translate_expression_rvalue(&unary.operand.node, func_ctx)?;
                log::debug!("operand: {:?}", operand);
                let one = Operand::constant(ir::Constant::Int {
                    value: 1,
                    width: operand.dtype().get_int_width().unwrap(),
                    is_signed: true,
                });
                let sub_instr = Instruction::BinOp {
                    op: BinaryOperator::Plus,
                    lhs: operand.clone(),
                    rhs: one,
                    dtype: operand.dtype().clone(),
                };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, sub_instr));

                let iid = func_ctx.curr_block.instructions.len() - 1;
                let rid = RegisterId::temp(func_ctx.curr_block.bid, iid);
                let value = Operand::register(rid, operand.dtype().clone());
                let store_instr = Instruction::Store {
                    ptr: self.translate_expression_lvalue(&unary.operand.node, func_ctx)?,
                    value: value.clone(),
                };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, store_instr));
                return Ok(value.clone());
            }
            // UnaryOperator::PostDecrement => {}
            UnaryOperator::PreIncrement | UnaryOperator::PreDecrement => {
                let operand = self.translate_expression_rvalue(&unary.operand.node, func_ctx)?;
                log::debug!("operand: {:?}", operand);
                let one = Operand::constant(ir::Constant::int(1, operand.dtype()));
                let ast_op = if op == UnaryOperator::PreDecrement {
                    BinaryOperator::Minus
                } else {
                    BinaryOperator::Plus
                };
                let sub_instr = Instruction::BinOp {
                    op: ast_op,
                    lhs: operand.clone(),
                    rhs: one,
                    dtype: operand.dtype().clone(),
                };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, sub_instr));

                let iid = func_ctx.curr_block.instructions.len() - 1;
                let rid = RegisterId::temp(func_ctx.curr_block.bid, iid);
                let value = Operand::register(rid, operand.dtype().clone());
                let store_instr = Instruction::Store {
                    ptr: self.translate_expression_lvalue(&unary.operand.node, func_ctx)?,
                    value: value.clone(),
                };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, store_instr));
                return Ok(value.clone());
            }
            UnaryOperator::Address => {
                // short circut `&*ptr` form to `ptr` according to C standard.
                match &unary.operand.node {
                    Expression::UnaryOperator(sub_unary) => {
                        let sub_expr = &sub_unary.node;
                        match &sub_expr.operator.node {
                            UnaryOperator::Indirection => {
                                return self
                                    .translate_expression_rvalue(&sub_expr.operand.node, func_ctx);
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                };
                log::debug!("unary expr: {}", &unary.write_string());
                // Should not take address from a rvalue, but the standard solution looks take
                // the expression as a rvalue-expression and an extra `load` instruction generated.
                let operand = self.translate_expression_lvalue(&unary.operand.node, func_ctx)?;
                log::debug!("address operand: {:?}", operand);
                return Ok(match &operand {
                    Operand::Constant(cst) => match cst {
                        ir::Constant::Undef { .. }
                        | ir::Constant::Unit { .. }
                        | ir::Constant::Int { .. }
                        | ir::Constant::Float { .. } => panic!("invalid lvalue: {}", cst),
                        ir::Constant::GlobalVariable { .. } => operand.clone(),
                    },
                    Operand::Register { rid, dtype } => match rid {
                        RegisterId::Local { .. } => operand.clone(),
                        RegisterId::Arg { .. } => operand.clone(),
                        RegisterId::Temp { .. } => {
                            // return a tempory register which contains a pointer dtype
                            if dtype.get_pointer_inner().is_some() {
                                operand.clone()
                            } else {
                                panic!("cannot take address from a local register")
                            }
                        }
                    },
                });
            }
            UnaryOperator::Indirection => {
                // short circut `*&ptr` form to `ptr` according to C standard.
                match &unary.operand.node {
                    Expression::UnaryOperator(sub_unary) => {
                        let sub_expr = &sub_unary.node;
                        match &sub_expr.operator.node {
                            UnaryOperator::Address => {
                                return self
                                    .translate_expression_rvalue(&sub_expr.operand.node, func_ctx);
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                };

                log::debug!(
                    "indirection processing {}",
                    &unary.operand.node.write_string()
                );
                let operand = self.translate_expression_rvalue(&unary.operand.node, func_ctx)?;
                log::debug!("indirection operand: {:?}", operand);

                let instr = Instruction::Load {
                    ptr: operand.clone(),
                };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, instr));

                let iid = func_ctx.curr_block.instructions.len() - 1;
                let bid = func_ctx.curr_block.bid;
                let rid = RegisterId::Temp { bid, iid };
                // the operand type is not pointer type, need to fetch the inner type
                // unwrap is safe here for pre-confirmation.
                let operand = Operand::Register {
                    rid,
                    dtype: operand.dtype().get_pointer_inner().unwrap().clone(),
                };
                return Ok(operand.clone());
            }
            // UnaryOperator::Plus => {}
            UnaryOperator::Minus => {
                let operand = self.translate_expression_rvalue(&unary.operand.node, func_ctx)?;
                return Ok(self.insert_instruction(
                    ir::Instruction::UnaryOp {
                        op,
                        operand: operand.clone(),
                        dtype: operand.dtype().clone(),
                    },
                    func_ctx,
                )?);
            }
            // UnaryOperator::Complement => {}
            // UnaryOperator::Negate => {}
            // UnaryOperator::SizeOf => {}
            _ => todo!("more unary operators"),
        }
        panic!("something wrong...")
    }

    fn create_bool_block(
        &self,
        block_bid: BlockId,
        exit_bid: BlockId,
        result_ptr: &Operand,
        value: bool,
        func_ctx: &mut FunctionContext,
    ) {
        let value = if value { 1 as u128 } else { 0 as u128 };
        let value = Operand::constant(ir::Constant::int(
            value,
            result_ptr
                .dtype()
                .get_pointer_inner()
                .unwrap()
                .deref()
                .clone(),
        ));
        let mut false_context = BBContext::new(block_bid);
        false_context.instructions.push(Named::new(
            None,
            Instruction::Store {
                ptr: result_ptr.clone(),
                value: value.clone(),
            },
        ));
        let false_block = ir::Block {
            phinodes: Vec::new(),
            instructions: false_context.instructions,
            exit: BlockExit::Jump {
                arg: ir::JumpArg::new(exit_bid, Vec::new()),
            },
        };
        func_ctx.blocks.insert(block_bid, false_block);
    }

    fn translate_binary_operator(
        &self,
        binary: &BinaryOperatorExpression,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        let op = binary.operator.node.clone();

        // For assign type, only right side need to be typecasted, while for others the
        // Dtype need to be deduced.
        match op {
            BinaryOperator::Assign
            | BinaryOperator::AssignDivide
            | BinaryOperator::AssignModulo
            | BinaryOperator::AssignShiftLeft
            | BinaryOperator::AssignShiftRight
            | BinaryOperator::AssignBitwiseAnd
            | BinaryOperator::AssignBitwiseXor
            | BinaryOperator::AssignBitwiseOr => {
                let lhs = self.translate_expression_lvalue(&binary.lhs.node, func_ctx)?;
                let rhs = self.translate_expression_rvalue(&binary.rhs.node, func_ctx)?;
                log::debug!("op: {:?}\n lhs: {:?}\n rhs: {:?}\n", op, lhs, rhs);
                log::debug!("assignment rhs 1: {:?}, lhs: {:?}", rhs, lhs);
                // when the lhs is a global variable, kecc represent it as a Constant::GlobalVariable;
                // if call dtype() method directly from lhs, the HasDtype transform the Dtype::Int
                // to Dtype::Pointer which is not what's needed here, so manually fetch Dtype if
                // lhs is a global variable
                let target_type = lhs.dtype();
                let target_type = if let Dtype::Pointer { inner, .. } = target_type {
                    inner.deref().clone()
                } else {
                    target_type
                };
                let rhs = self.translate_typecast(&rhs, &target_type, func_ctx)?;
                let instr = Instruction::Store {
                    ptr: lhs,
                    value: rhs.clone(),
                };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, instr));
                log::debug!("assignment rhs: {:?}", &rhs);
                // for assignment expression, rhs operand is returned
                return Ok(rhs);
            }
            BinaryOperator::AssignPlus | BinaryOperator::AssignMinus => {
                let lhs = self.translate_expression_lvalue(&binary.lhs.node, func_ctx)?;
                let rhs = self.translate_expression_rvalue(&binary.rhs.node, func_ctx)?;
                log::debug!("op: {:?}\n lhs: {:?}\n rhs: {:?}\n", op, lhs, rhs);
                log::debug!("assignment rhs 1: {:?}, lhs: {:?}", rhs, lhs);
                if lhs.dtype().get_pointer_inner().is_none() {
                    panic!(
                        "lhs value of assignment expression is not a pointer: {}",
                        lhs
                    );
                }
                let load_instr = Instruction::Load { ptr: lhs.clone() };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, load_instr));
                let iid = func_ctx.curr_block.instructions.len() - 1;
                let bid = func_ctx.curr_block.bid;
                let rid = RegisterId::Temp { bid, iid };
                let lhs_value = Operand::Register {
                    rid,
                    dtype: lhs.clone().dtype().get_pointer_inner().unwrap().clone(),
                };
                // when the lhs is a global variable, kecc represent it as a Constant::GlobalVariable;
                // if call dtype() method directly from lhs, the HasDtype transform the Dtype::Int
                // to Dtype::Pointer which is not what's needed here, so manually fetch Dtype if
                // lhs is a global variable
                // let target_type = lhs.dtype();
                // let target_type = if let Dtype::Pointer { inner, .. } = target_type {
                //     inner.deref().clone()
                // } else {
                //     target_type
                // };
                let target_type = lhs_value.dtype().clone();
                let rhs = self.translate_typecast(&rhs, &target_type, func_ctx)?;

                let op = if op == BinaryOperator::AssignPlus {
                    BinaryOperator::Plus
                } else {
                    BinaryOperator::Minus
                };
                let add_instr = Instruction::BinOp {
                    op,
                    lhs: lhs_value,
                    rhs: rhs.clone(),
                    dtype: target_type.clone(),
                };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, add_instr));
                let iid = func_ctx.curr_block.instructions.len() - 1;
                let bid = func_ctx.curr_block.bid;
                let rid = RegisterId::Temp { bid, iid };
                let value = Operand::Register {
                    rid,
                    dtype: target_type.clone(),
                };

                let instr = Instruction::Store { ptr: lhs, value };
                func_ctx
                    .curr_block
                    .instructions
                    .push(Named::new(None, instr));
                // log::debug!("assignment rhs: {:?}", &rhs);
                // for assignment expression, rhs operand is returned
                return Ok(rhs);
            }
            BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                let dtype = Dtype::BOOL;
                let lhs = &binary.lhs.node;
                let rhs = &binary.rhs.node;
                let res = func_ctx.alloc_tmp("t".to_owned(), dtype)?;

                // lhs condition block, false block
                let lhs_true_bid = func_ctx.alloc_bid();
                let lhs_false_bid = func_ctx.alloc_bid();
                let then_context = if op == BinaryOperator::LogicalAnd {
                    BBContext::new(lhs_true_bid)
                } else {
                    BBContext::new(lhs_false_bid)
                };
                let exit_bid = func_ctx.alloc_bid();
                let exit_context = BBContext::new(exit_bid);

                self.translate_condition(lhs, lhs_true_bid, lhs_false_bid, func_ctx)?;
                if op == BinaryOperator::LogicalAnd {
                    self.create_bool_block(lhs_false_bid, exit_bid, &res, false, func_ctx);
                } else {
                    self.create_bool_block(lhs_true_bid, exit_bid, &res, true, func_ctx);
                }
                std::mem::replace(&mut func_ctx.curr_block, then_context);
                let condition = self.translate_expression_rvalue(rhs, func_ctx)?;
                let condition = self.translate_typecast_to_bool(&condition, func_ctx)?;
                self.insert_instruction(
                    ir::Instruction::Store {
                        ptr: res.clone(),
                        value: condition,
                    },
                    func_ctx,
                )?;
                let block = ir::Block {
                    phinodes: Vec::new(),
                    instructions: std::mem::replace(
                        &mut func_ctx.curr_block.instructions,
                        Vec::new(),
                    ),
                    exit: ir::BlockExit::Jump {
                        arg: JumpArg::new(exit_bid, Vec::new()),
                    },
                };
                func_ctx.blocks.insert(func_ctx.curr_block.bid, block);
                std::mem::replace(&mut func_ctx.curr_block, exit_context);
                return Ok(
                    self.insert_instruction(ir::Instruction::Load { ptr: res.clone() }, func_ctx)?
                );
            }
            _ => {}
        }
        let lhs = self.translate_expression_rvalue(&binary.lhs.node, func_ctx)?;
        let rhs = self.translate_expression_rvalue(&binary.rhs.node, func_ctx)?;
        log::debug!("op: {:?}\n lhs: {:?}\n rhs: {:?}\n", op, lhs, rhs);

        let target_dtype = self.translate_merge_type(&lhs.dtype(), &rhs.dtype())?;
        log::debug!("merged dtype: {:?}", target_dtype);

        let lhs = self.translate_typecast(&lhs, &target_dtype, func_ctx)?;
        let rhs = self.translate_typecast(&rhs, &target_dtype, func_ctx)?;
        let instr_dtype = self.data_width_postprocess(&target_dtype, &op);
        let instr = Instruction::BinOp {
            op,
            lhs,
            rhs,
            dtype: instr_dtype.clone(),
        };
        func_ctx
            .curr_block
            .instructions
            .push(Named::new(None, instr));

        let iid = func_ctx.curr_block.instructions.len() - 1;
        let bid = func_ctx.curr_block.bid;
        let rid = RegisterId::Temp { bid, iid };
        let operand = Operand::Register {
            rid,
            dtype: instr_dtype.clone(),
        };
        Ok(operand)
    }

    fn data_width_postprocess(&self, dtype: &Dtype, op: &BinaryOperator) -> Dtype {
        if let Dtype::Int { is_const, .. } = dtype {
            match op {
                BinaryOperator::Less
                | BinaryOperator::Greater
                | BinaryOperator::LessOrEqual
                | BinaryOperator::GreaterOrEqual
                | BinaryOperator::Equals
                | BinaryOperator::NotEquals => {
                    return Dtype::Int {
                        width: 1,
                        is_const: *is_const,
                        is_signed: false,
                    }
                }
                _ => return dtype.clone(),
            }
        }
        dtype.clone()
    }

    fn translate_merge_type(&self, lhs: &Dtype, rhs: &Dtype) -> Result<Dtype, IrgenError> {
        match (lhs, rhs) {
            // (Dtype::Int { width, is_signed, is_const }, Dtype::Unit { is_const }) => {}
            (
                Dtype::Int {
                    width: l_width,
                    is_signed: l_is_signed,
                    is_const: l_is_const,
                },
                Dtype::Int {
                    width: r_width,
                    is_signed: r_is_signed,
                    is_const: r_is_const,
                },
            ) => Ok(Dtype::Int {
                width: cmp::max(*l_width, *r_width),
                is_signed: *l_is_signed || *r_is_signed,
                is_const: *l_is_const && *r_is_const,
            }),
            // (Dtype::Int { width, is_signed, is_const }, Dtype::Float { width, is_const }) => {}
            // (Dtype::Int { width, is_signed, is_const }, Dtype::Pointer { inner, is_const }) => {}
            // (Dtype::Int { width, is_signed, is_const }, Dtype::Array { inner, size }) => {}
            // (Dtype::Int { width, is_signed, is_const }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
            // (Dtype::Int { width, is_signed, is_const }, Dtype::Function { ret, params }) => {}
            // (Dtype::Int { width, is_signed, is_const }, Dtype::Typedef { name, is_const }) => {}
            // (Dtype::Float { width, is_const }, Dtype::Unit { is_const }) => {}
            // (Dtype::Float { width, is_const }, Dtype::Int { width, is_signed, is_const }) => {}
            // (Dtype::Float { width, is_const }, Dtype::Float { width, is_const }) => {}
            // (Dtype::Float { width, is_const }, Dtype::Pointer { inner, is_const }) => {}
            // (Dtype::Float { width, is_const }, Dtype::Array { inner, size }) => {}
            // (Dtype::Float { width, is_const }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
            // (Dtype::Float { width, is_const }, Dtype::Function { ret, params }) => {}
            // (Dtype::Float { width, is_const }, Dtype::Typedef { name, is_const }) => {}
            // (Dtype::Pointer { inner, is_const }, Dtype::Unit { is_const }) => {}
            (Dtype::Pointer { inner, .. }, Dtype::Int { .. }) => {
                self.translate_merge_type(inner.deref(), rhs)
            }
            // (Dtype::Pointer { inner, is_const }, Dtype::Float { width, is_const }) => {}
            (Dtype::Pointer { inner: l_inner, .. }, Dtype::Pointer { inner: r_inner, .. }) => {
                self.translate_merge_type(l_inner.deref(), r_inner.deref())
            }
            // (Dtype::Pointer { inner, is_const }, Dtype::Array { inner, size }) => {}
            // (Dtype::Pointer { inner, is_const }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
            // (Dtype::Pointer { inner, is_const }, Dtype::Function { ret, params }) => {}
            // (Dtype::Pointer { inner, is_const }, Dtype::Typedef { name, is_const }) => {}
            // (Dtype::Array { inner, size }, Dtype::Unit { is_const }) => {}
            // (Dtype::Array { inner, size }, Dtype::Int { width, is_signed, is_const }) => {}
            // (Dtype::Array { inner, size }, Dtype::Float { width, is_const }) => {}
            // (Dtype::Array { inner, size }, Dtype::Pointer { inner, is_const }) => {}
            // (Dtype::Array { inner, size }, Dtype::Array { inner, size }) => {}
            // (Dtype::Array { inner, size }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
            // (Dtype::Array { inner, size }, Dtype::Function { ret, params }) => {}
            // (Dtype::Array { inner, size }, Dtype::Typedef { name, is_const }) => {}
            // (Dtype::Struct { name, fields, is_const, size_align_offsets }, Dtype::Unit { is_const }) => {}
            // (Dtype::Struct { name, fields, is_const, size_align_offsets }, Dtype::Int { width, is_signed, is_const }) => {}
            // (Dtype::Struct { name, fields, is_const, size_align_offsets }, Dtype::Float { width, is_const }) => {}
            // (Dtype::Struct { name, fields, is_const, size_align_offsets }, Dtype::Pointer { inner, is_const }) => {}
            // (Dtype::Struct { name, fields, is_const, size_align_offsets }, Dtype::Array { inner, size }) => {}
            // (Dtype::Struct { name, fields, is_const, size_align_offsets }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
            // (Dtype::Struct { name, fields, is_const, size_align_offsets }, Dtype::Function { ret, params }) => {}
            // (Dtype::Struct { name, fields, is_const, size_align_offsets }, Dtype::Typedef { name, is_const }) => {}
            // (Dtype::Function { ret, params }, Dtype::Unit { is_const }) => {}
            // (Dtype::Function { ret, params }, Dtype::Int { width, is_signed, is_const }) => {}
            // (Dtype::Function { ret, params }, Dtype::Float { width, is_const }) => {}
            // (Dtype::Function { ret, params }, Dtype::Pointer { inner, is_const }) => {}
            // (Dtype::Function { ret, params }, Dtype::Array { inner, size }) => {}
            // (Dtype::Function { ret, params }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
            // (Dtype::Function { ret, params }, Dtype::Function { ret, params }) => {}
            // (Dtype::Function { ret, params }, Dtype::Typedef { name, is_const }) => {}
            // (Dtype::Typedef { name, is_const }, Dtype::Unit { is_const }) => {}
            // (Dtype::Typedef { name, is_const }, Dtype::Int { width, is_signed, is_const }) => {}
            // (Dtype::Typedef { name, is_const }, Dtype::Float { width, is_const }) => {}
            // (Dtype::Typedef { name, is_const }, Dtype::Pointer { inner, is_const }) => {}
            // (Dtype::Typedef { name, is_const }, Dtype::Array { inner, size }) => {}
            // (Dtype::Typedef { name, is_const }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
            // (Dtype::Typedef { name, is_const }, Dtype::Function { ret, params }) => {}
            // (Dtype::Typedef { name, is_const }, Dtype::Typedef { name, is_const }) => {}
            _ => todo!("merge type lhs: {:?}, rhs: {:?}", &lhs, &rhs),
        }
        // (Operand::Constant(_lval), Operand::Constant(_rval)) => todo!("const op const"),
        // (Operand::Constant(constant_val), Operand::Register { dtype, .. })
        // | (Operand::Register { dtype, .. }, Operand::Constant(constant_val)) => {
        //     match (constant_val, dtype) {
        // (ir::Constant::Undef { dtype }, Dtype::Unit { is_const }) => {}
        // (ir::Constant::Undef { dtype }, Dtype::Int { width, is_signed, is_const }) => {}
        // (ir::Constant::Undef { dtype }, Dtype::Float { width, is_const }) => {}
        // (ir::Constant::Undef { dtype }, Dtype::Pointer { inner, is_const }) => {}
        // (ir::Constant::Undef { dtype }, Dtype::Array { inner, size }) => {}
        // (ir::Constant::Undef { dtype }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
        // (ir::Constant::Undef { dtype }, Dtype::Function { ret, params }) => {}
        // (ir::Constant::Undef { dtype }, Dtype::Typedef { name, is_const }) => {}
        // (ir::Constant::Unit, Dtype::Unit { is_const }) => {}
        // (ir::Constant::Unit, Dtype::Int { width, is_signed, is_const }) => {}
        // (ir::Constant::Unit, Dtype::Float { width, is_const }) => {}
        // (ir::Constant::Unit, Dtype::Pointer { inner, is_const }) => {}
        // (ir::Constant::Unit, Dtype::Array { inner, size }) => {}
        // (ir::Constant::Unit, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
        // (ir::Constant::Unit, Dtype::Function { ret, params }) => {}
        // (ir::Constant::Unit, Dtype::Typedef { name, is_const }) => {}
        // (ir::Constant::Int { value, width, is_signed }, Dtype::Unit { is_const }) => {}
        // (
        //     ir::Constant::Int {
        //         width: cwidth,
        //         is_signed: cis_signed,
        //         ..
        //     },
        //     Dtype::Int {
        //         width,
        //         is_signed,
        //         is_const,
        //     },
        // ) => {
        //     let target_dtype = Dtype::Int {
        //         width: cmp::max(*width, *cwidth),
        //         is_signed: *is_signed || *cis_signed,
        //         is_const: *is_const,
        //     };
        //     return Ok(target_dtype);
        // } // (ir::Constant::Int { value, width, is_signed }, Dtype::Float { width, is_const }) => {}
        // (ir::Constant::Int { value, width, is_signed }, Dtype::Pointer { inner, is_const }) => {}
        // (ir::Constant::Int { value, width, is_signed }, Dtype::Array { inner, size }) => {}
        // (ir::Constant::Int { value, width, is_signed }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
        // (ir::Constant::Int { value, width, is_signed }, Dtype::Function { ret, params }) => {}
        // (ir::Constant::Int { value, width, is_signed }, Dtype::Typedef { name, is_const }) => {}
        // (ir::Constant::Float { value, width }, Dtype::Unit { is_const }) => {}
        // (ir::Constant::Float { value, width }, Dtype::Int { width, is_signed, is_const }) => {}
        // (ir::Constant::Float { value, width }, Dtype::Float { width, is_const }) => {}
        // (ir::Constant::Float { value, width }, Dtype::Pointer { inner, is_const }) => {}
        // (ir::Constant::Float { value, width }, Dtype::Array { inner, size }) => {}
        // (ir::Constant::Float { value, width }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
        // (ir::Constant::Float { value, width }, Dtype::Function { ret, params }) => {}
        // (ir::Constant::Float { value, width }, Dtype::Typedef { name, is_const }) => {}
        // (ir::Constant::GlobalVariable { name, dtype }, Dtype::Unit { is_const }) => {}
        // (ir::Constant::GlobalVariable { name, dtype }, Dtype::Int { width, is_signed, is_const }) => {}
        // (ir::Constant::GlobalVariable { name, dtype }, Dtype::Float { width, is_const }) => {}
        // (ir::Constant::GlobalVariable { name, dtype }, Dtype::Pointer { inner, is_const }) => {}
        // (ir::Constant::GlobalVariable { name, dtype }, Dtype::Array { inner, size }) => {}
        // (ir::Constant::GlobalVariable { name, dtype }, Dtype::Struct { name, fields, is_const, size_align_offsets }) => {}
        // (ir::Constant::GlobalVariable { name, dtype }, Dtype::Function { ret, params }) => {}
        // (ir::Constant::GlobalVariable { name, dtype }, Dtype::Typedef { name, is_const }) => {}
        // _ => todo!("constant | register"),
        // }
        // }
        // (Operand::Constant(constant_val), Operand::Register { dtype, .. }) => {
        //     let target_constant = constant_val.clone().typecast(dtype.clone());
        //     let mut target_dtype = dtype.clone();
        //     if let Dtype::Int {
        //         ref mut width,
        //         ref mut is_signed,
        //         ..
        //     } = target_dtype
        //     {
        //         if let ir::Constant::Int {
        //             width: cwidth,
        //             is_signed: cis_signed,
        //             ..
        //         } = target_constant
        //         {
        //             *width = cwidth;
        //             *is_signed = cis_signed;
        //         }
        //     }
        //     return Ok(target_dtype);
        // }
        // (Operand::Register { .. }, Operand::Register { .. }) => todo!("register | register"),
        // }
    }

    fn translate_identifier(
        &self,
        identifier: &Identifier,
        func_ctx: &mut FunctionContext,
    ) -> Result<Operand, IrgenError> {
        let operand = func_ctx.lookup_symbol_table(identifier.name.clone())?;
        log::debug!("indentifier operand: {:?}", operand);

        // kecc represent global function as Constant::GlobalVariable, and when access by
        // dtype() method, it'll be converted to a pointer.
        if operand
            .get_constant()
            .map_or(ir::Constant::unit(), |c| c.clone())
            .is_function()
        {
            return Ok(operand);
        }

        // need to load the identifier if the inner data type is a pointer
        // if operand.get_register().is_some() && operand.dtype().get_pointer_inner().is_some() {
        if operand.dtype().get_pointer_inner().is_some() {
            let instr = Instruction::Load {
                ptr: operand.clone(),
            };
            func_ctx
                .curr_block
                .instructions
                .push(Named::new(None, instr));

            let iid = func_ctx.curr_block.instructions.len() - 1;
            let bid = func_ctx.curr_block.bid;
            let rid = RegisterId::Temp { bid, iid };
            // the operand type is not pointer type, need to fetch the inner type
            // unwrap is safe here for pre-confirmation.
            let operand = Operand::Register {
                rid,
                dtype: operand.dtype().get_pointer_inner().unwrap().clone(),
            };
            return Ok(operand);
        }
        Ok(operand)
    }
}

impl ir::Constant {
    fn is_function(&self) -> bool {
        match self {
            ir::Constant::GlobalVariable { dtype, .. } => dtype.get_function_inner().is_some(),
            _ => false,
        }
    }

    fn get_function_ret(&self) -> Option<Dtype> {
        match self {
            ir::Constant::GlobalVariable { dtype, .. } => match dtype {
                Dtype::Function { ret, .. } => Some(ret.deref().clone()),
                _ => None,
            },
            _ => None,
        }
    }
}
