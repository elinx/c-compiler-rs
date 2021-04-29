use lang_c::ast::*;
use lang_c::span::Node;

use core::ops::Deref;
use std::io::{Result, Write};

use crate::write_base::*;

impl<T: WriteLine> WriteLine for Node<T> {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.node.write_line(indent, write)
    }
}

fn write_indent(indent: usize, write: &mut dyn Write) -> Result<()> {
    write!(write, "{0: <1$}", "", indent * 4)
}

fn write_space(write: &mut dyn Write) -> Result<()> {
    write!(write, " ")
}

fn write_eq(write: &mut dyn Write) -> Result<()> {
    write!(write, "=")
}

fn write_semicolon(write: &mut dyn Write) -> Result<()> {
    write!(write, ";")
}

fn write_newline(write: &mut dyn Write) -> Result<()> {
    write!(write, "\n")
}

impl WriteLine for ExternalDeclaration {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            ExternalDeclaration::Declaration(decl) => {
                decl.node.write_line(indent, write)?;
            }
            ExternalDeclaration::StaticAssert(sa) => {
                sa.node.write_line(indent, write)?;
            }
            ExternalDeclaration::FunctionDefinition(func) => {
                func.node.write_line(indent, write)?;
            }
        }
        // write!(write, "external declaration: {}\n", indent)
        Ok(())
    }
}

impl WriteLine for DeclarationSpecifier {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            DeclarationSpecifier::Alignment(alignment_specifier) => {
                alignment_specifier.node.write_line(indent, write)?;
            }
            DeclarationSpecifier::Extension(extension) => {
                extension
                    .iter()
                    .for_each(|ext| ext.node.write_line(indent, write).unwrap());
            }
            DeclarationSpecifier::Function(function) => {
                function.node.write_line(indent, write)?;
            }
            DeclarationSpecifier::StorageClass(storage) => {
                storage.node.write_line(indent, write)?;
            }
            DeclarationSpecifier::TypeQualifier(tq) => {
                tq.node.write_line(indent, write)?;
            }
            DeclarationSpecifier::TypeSpecifier(ts) => {
                ts.node.write_line(indent, write)?;
            }
        }
        Ok(())
    }
}

impl WriteLine for Declaration {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.specifiers
            .iter()
            .for_each(|specifier| specifier.node.write_line(indent, write).unwrap());
        write_space(write)?;
        self.declarators
            .iter()
            .for_each(|decl| decl.node.write_line(indent, write).unwrap());
        // for (specifier, declarator) in self.specifiers.iter().zip(self.declarators.iter()) {
        //     specifier.node.write_line(indent, write)?;
        //     write_space(write)?;
        //     declarator.node.declarator.node.write_line(indent, write)?;
        //     write_space(write)?;
        //     write_eq(write)?;
        //     write_space(write)?;
        //     if let Some(initializer) = &declarator.node.initializer {
        //         initializer.node.write_line(indent, write)?;
        //     }
        //     write_semicolon(write)?;
        //     write_newline(write)?;
        // }
        write_semicolon(write)?;
        write_newline(write)?;
        Ok(())
    }
}

impl WriteLine for InitDeclarator {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.declarator.node.write_line(indent, write)?;
        write_space(write)?;
        write_eq(write)?;
        write_space(write)?;
        if let Some(initializer) = &self.initializer {
            initializer.write_line(indent, write)?;
        };
        Ok(())
    }
}
impl WriteLine for Declarator {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.kind.node.write_line(indent, write)?;
        self.derived
            .iter()
            .for_each(|node| node.write_line(indent, write).unwrap());
        self.extensions
            .iter()
            .for_each(|node| node.write_line(indent, write).unwrap());
        Ok(())
    }
}

impl WriteLine for DerivedDeclarator {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            DerivedDeclarator::Pointer(_) => todo!("pointer ddtor"),
            DerivedDeclarator::Array(_) => todo!("array ddtor"),
            DerivedDeclarator::Function(func_decl) => func_decl.node.write_line(indent, write),
            DerivedDeclarator::KRFunction(func) => {
                write!(write, "(")?;
                func.iter()
                    .for_each(|id| id.write_line(indent, write).unwrap());
                write!(write, ")")
            }
        }
    }
}

impl WriteLine for FunctionDeclarator {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write!(write, "(")?;
        self.parameters
            .iter()
            .for_each(|param| param.node.write_line(indent, write).unwrap());
        self.ellipsis.write_line(indent, write)?;
        write!(write, ")")
    }
}

impl WriteLine for ParameterDeclaration {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.specifiers
            .iter()
            .for_each(|specifier| specifier.node.write_line(indent, write).unwrap());
        if let Some(declarator) = &self.declarator {
            declarator.node.write_line(indent, write)?;
        }
        self.extensions
            .iter()
            .for_each(|ext| ext.node.write_line(indent, write).unwrap());
        Ok(())
    }
}

impl WriteLine for Ellipsis {
    fn write_line(&self, _indent: usize, _write: &mut dyn Write) -> Result<()> {
        match self {
            Ellipsis::Some => todo!("ellipsis some"),
            Ellipsis::None => Ok(()),
        }
    }
}

impl WriteLine for DeclaratorKind {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            DeclaratorKind::Abstract => todo!("abstract declarator kind"),
            DeclaratorKind::Identifier(id) => {
                id.node.write_line(indent, write)?;
            }
            DeclaratorKind::Declarator(decl) => {
                decl.as_ref().node.write_line(indent, write)?;
            }
        }
        Ok(())
    }
}

impl WriteLine for Identifier {
    fn write_line(&self, _indent: usize, write: &mut dyn Write) -> Result<()> {
        write!(write, "{}", self.name)
    }
}

impl WriteLine for Constant {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            Constant::Integer(integer) => integer.write_line(indent, write),
            Constant::Float(_) => todo!("const float"),
            Constant::Character(_) => todo!("const character"),
        }
    }
}

impl WriteLine for Integer {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self.base {
            IntegerBase::Decimal => write!(write, "")?,
            IntegerBase::Octal => write!(write, "0o")?,
            IntegerBase::Hexadecimal => write!(write, "0x")?,
        }
        write!(write, "{}", self.number.as_ref())?;
        self.suffix.write_line(indent, write)
    }
}

impl WriteLine for IntegerSuffix {
    fn write_line(&self, _indent: usize, write: &mut dyn Write) -> Result<()> {
        if self.unsigned {
            write!(write, "u")?;
        }
        let s = match self.size {
            IntegerSize::Int => "",
            IntegerSize::Long => "l",
            IntegerSize::LongLong => "ll",
        };
        write!(write, "{}", s)?;
        Ok(())
    }
}

impl WriteLine for Initializer {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            Initializer::Expression(expr) => expr.as_ref().node.write_line(indent, write)?,
            Initializer::List(_list) => todo!("initializer list"),
        }
        Ok(())
    }
}

impl WriteLine for Expression {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            Expression::Identifier(id) => id.as_ref().node.write_line(indent, write)?,
            Expression::Constant(cst) => cst.as_ref().node.write_line(indent, write)?,
            Expression::StringLiteral(_) => {}
            Expression::GenericSelection(_) => {}
            Expression::Member(_) => {}
            Expression::Call(call) => call.as_ref().node.write_line(indent, write)?,
            Expression::CompoundLiteral(_) => {}
            Expression::SizeOf(_) => {}
            Expression::AlignOf(_) => {}
            Expression::UnaryOperator(unary) => unary.deref().node.write_line(indent, write)?,
            Expression::Cast(_) => {}
            Expression::BinaryOperator(binop) => binop.as_ref().node.write_line(indent, write)?,
            Expression::Conditional(_) => {}
            Expression::Comma(_) => {}
            Expression::OffsetOf(_) => {}
            Expression::VaArg(_) => {}
            Expression::Statement(_) => {}
        }
        Ok(())
    }
}

impl WriteLine for UnaryOperatorExpression {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write!(write, "(")?;
        self.operator.write_line(indent, write)?;
        self.operand.deref().node.write_line(indent, write)?;
        write!(write, ")")
    }
}

impl WriteLine for UnaryOperator {
    fn write_line(&self, _indent: usize, write: &mut dyn Write) -> Result<()> {
        let ops = match self {
            UnaryOperator::PostIncrement => "++",
            UnaryOperator::PostDecrement => "--",
            UnaryOperator::PreIncrement => "++",
            UnaryOperator::PreDecrement => "--",
            UnaryOperator::Address => "&",
            UnaryOperator::Indirection => "*",
            UnaryOperator::Plus => "+",
            UnaryOperator::Minus => "-",
            UnaryOperator::Complement => "~",
            UnaryOperator::Negate => "-",
            UnaryOperator::SizeOf => "sizeof",
        };
        write!(write, "{}", ops)
    }
}

impl WriteLine for CallExpression {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.callee.as_ref().node.write_line(indent, write)?;
        write!(write, "(")?;
        self.arguments
            .iter()
            .for_each(|arg| arg.node.write_line(indent, write).unwrap());
        write!(write, ")")
    }
}

impl WriteLine for BinaryOperatorExpression {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.lhs.as_ref().node.write_line(indent, write)?;
        write_space(write)?;
        self.operator.node.write_line(indent, write)?;
        write_space(write)?;
        self.rhs.as_ref().node.write_line(indent, write)
    }
}

impl WriteLine for BinaryOperator {
    fn write_line(&self, _indent: usize, write: &mut dyn Write) -> Result<()> {
        let opstr = match self {
            BinaryOperator::Index => "[]",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Modulo => "%",
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::ShiftLeft => "<<",
            BinaryOperator::ShiftRight => ">>",
            BinaryOperator::Less => "<",
            BinaryOperator::Greater => ">",
            BinaryOperator::LessOrEqual => "<=",
            BinaryOperator::GreaterOrEqual => ">=",
            BinaryOperator::Equals => "==",
            BinaryOperator::NotEquals => "!=",
            BinaryOperator::BitwiseAnd => "&",
            BinaryOperator::BitwiseXor => "^",
            BinaryOperator::BitwiseOr => "|",
            BinaryOperator::LogicalAnd => "&&",
            BinaryOperator::LogicalOr => "||",
            BinaryOperator::Assign => "=",
            BinaryOperator::AssignMultiply => "*=",
            BinaryOperator::AssignDivide => "/=",
            BinaryOperator::AssignModulo => "%=",
            BinaryOperator::AssignPlus => "+=",
            BinaryOperator::AssignMinus => "-=",
            BinaryOperator::AssignShiftLeft => "<<=",
            BinaryOperator::AssignShiftRight => ">>=",
            BinaryOperator::AssignBitwiseAnd => "&=",
            BinaryOperator::AssignBitwiseXor => "^=",
            BinaryOperator::AssignBitwiseOr => "|=",
        };
        write!(write, "{}", opstr)
    }
}

impl WriteLine for AlignmentSpecifier {
    fn write_line(&self, _indent: usize, _write: &mut dyn Write) -> Result<()> {
        todo!("alignment specifier");
    }
}

impl WriteLine for Extension {
    fn write_line(&self, _indent: usize, _write: &mut dyn Write) -> Result<()> {
        todo!("extension");
    }
}

impl WriteLine for FunctionSpecifier {
    fn write_line(&self, _indent: usize, _write: &mut dyn Write) -> Result<()> {
        todo!("function specifier");
    }
}

impl WriteLine for StorageClassSpecifier {
    fn write_line(&self, _indent: usize, _write: &mut dyn Write) -> Result<()> {
        todo!("storage class specifier");
    }
}

impl WriteLine for TypeQualifier {
    fn write_line(&self, _indent: usize, _write: &mut dyn Write) -> Result<()> {
        todo!("type qualifier");
    }
}

impl WriteLine for TypeSpecifier {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write_indent(indent, write)?;
        let type_str = match self {
            TypeSpecifier::Void => "void",
            TypeSpecifier::Char => "char",
            TypeSpecifier::Short => "short",
            TypeSpecifier::Int => "int",
            TypeSpecifier::Long => "long",
            TypeSpecifier::Float => "float",
            TypeSpecifier::Double => "double",
            TypeSpecifier::Signed => "signed",
            TypeSpecifier::Unsigned => "unsigned",
            TypeSpecifier::Bool => "bool",
            TypeSpecifier::Complex => unimplemented!("complex not implement"),
            TypeSpecifier::Atomic(_) => unimplemented!("atomic not implement"),
            TypeSpecifier::Struct(_) => "struct",
            TypeSpecifier::Enum(_) => unimplemented!("atomic not implement"),
            TypeSpecifier::TypedefName(_) => "typedef",
            TypeSpecifier::TypeOf(_) => unimplemented!("atomic not implement"),
            TypeSpecifier::TS18661Float(_) => unimplemented!("atomic not implement"),
        };
        write!(write, "{}", type_str)
    }
}

impl WriteLine for StaticAssert {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write!(write, "static assert {}\n", indent)
    }
}

impl WriteLine for FunctionDefinition {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        // writeln!(write, "000")?;
        self.specifiers
            .iter()
            .for_each(|node| node.node.write_line(indent, write).unwrap());
        write_space(write)?;
        self.declarator.write_line(indent, write)?;
        // writeln!(write, "111")?;
        self.statement.node.write_line(indent, write)
        // writeln!(write, "222")
    }
}

impl WriteLine for Statement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            Statement::Labeled(_) => todo!("label sattement"),
            Statement::Compound(block_items) => {
                write_newline(write)?;
                write_indent(indent, write)?;
                write!(write, "{{\n")?;
                block_items
                    .iter()
                    .for_each(|item| item.node.write_line(indent, write).unwrap());
                write_indent(indent, write)?;
                write!(write, "}}\n")?;
                Ok(())
            }
            Statement::Expression(_) => todo!("expression statement"),
            Statement::If(if_stmt) => if_stmt.node.write_line(indent, write),
            Statement::Switch(_) => todo!("switch statement"),
            Statement::While(_) => todo!("while statement"),
            Statement::DoWhile(_) => todo!("do-while statement"),
            Statement::For(_) => todo!("for statement"),
            Statement::Goto(_) => todo!("goto statement"),
            Statement::Continue => todo!("continue statement"),
            Statement::Break => todo!("break statement"),
            Statement::Return(ret_stmt) => {
                if let Some(expr) = &ret_stmt {
                    write_indent(indent, write)?;
                    write!(write, "return ")?;
                    expr.as_ref().node.write_line(indent, write)?;
                    write_semicolon(write)?;
                    write_newline(write)?;
                }
                Ok(())
            }
            Statement::Asm(_) => todo!("asm statement"),
        }
    }
}

impl WriteLine for BlockItem {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            BlockItem::Declaration(decl) => decl.node.write_line(indent + 1, write),
            BlockItem::StaticAssert(static_assert) => {
                static_assert.node.write_line(indent + 1, write)
            }
            BlockItem::Statement(stmt) => stmt.node.write_line(indent + 1, write),
        }
    }
}

impl WriteLine for IfStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write_indent(indent, write)?;
        write!(write, "if (")?;
        self.condition.as_ref().node.write_line(indent, write)?;
        write!(write, ") ")?;
        self.then_statement
            .as_ref()
            .node
            .write_line(indent, write)?;
        if let Some(else_stmt) = &self.else_statement {
            else_stmt.as_ref().node.write_line(indent, write)?;
        };
        Ok(())
    }
}

impl<T: WriteString> WriteString for Node<T> {
    fn write_string(&self) -> String {
        self.node.write_string()
    }
}

impl<T: WriteString> WriteString for Box<T> {
    fn write_string(&self) -> String {
        self.deref().write_string()
    }
}

impl<T: WriteString> WriteString for &T {
    fn write_string(&self) -> String {
        (*self).write_string()
    }
}

impl<T: WriteString> WriteString for Option<T> {
    fn write_string(&self) -> String {
        if let Some(this) = self {
            this.write_string()
        } else {
            "".to_string()
        }
    }
}

impl WriteLine for TranslationUnit {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        let Self(tu) = self;
        tu.iter()
            .for_each(|node| node.write_line(indent, write).unwrap());
        Ok(())
        // todo!("homework 1")
    }
}

impl WriteString for Initializer {
    fn write_string(&self) -> String {
        todo!()
    }
}
