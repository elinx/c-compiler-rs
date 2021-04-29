use itertools::Itertools;
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

impl<T: WriteString> WriteString for Vec<T> {
    fn write_string(&self) -> String {
        self.iter().map(|param| param.write_string()).join(" ")
    }
}

impl<T: WriteLine> WriteLine for Box<T> {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.deref().write_line(indent, write)
    }
}

impl<T: WriteLine> WriteLine for &T {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        (*self).write_line(indent, write)
    }
}

impl<T: WriteLine> WriteLine for Option<T> {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        if let Some(this) = self {
            this.write_line(indent, write)
        } else {
            Ok(())
        }
    }
}

impl<T: WriteLine> WriteLine for Vec<T> {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        Ok(for item in self {
            item.write_line(indent, write)?;
        })
    }
}

impl WriteLine for TranslationUnit {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.0.write_line(indent, write)
    }
}

impl WriteLine for ExternalDeclaration {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            ExternalDeclaration::Declaration(decl) => {
                write_indent(indent, write)?;
                write!(write, "{};\n", decl.write_string())
            }
            ExternalDeclaration::StaticAssert(sa) => sa.node.write_line(indent, write),
            ExternalDeclaration::FunctionDefinition(func) => func.node.write_line(indent, write),
        }
    }
}

impl WriteString for Declaration {
    fn write_string(&self) -> String {
        let specifiers = self
            .specifiers
            .iter()
            .map(|specifier| specifier.write_string())
            .join(" ");
        let declarators = self
            .declarators
            .iter()
            .map(|decl| decl.write_string())
            .join(", ");
        specifiers + &" " + &declarators
    }
}

impl WriteString for DeclarationSpecifier {
    fn write_string(&self) -> String {
        match self {
            DeclarationSpecifier::StorageClass(storage_class) => storage_class.write_string(),
            DeclarationSpecifier::TypeSpecifier(type_specifier) => type_specifier.write_string(),
            DeclarationSpecifier::TypeQualifier(type_qualifier) => type_qualifier.write_string(),
            DeclarationSpecifier::Function(_) => unimplemented!("function specifier"),
            DeclarationSpecifier::Alignment(_) => unimplemented!("alignment"),
            DeclarationSpecifier::Extension(_) => unimplemented!("extension"),
        }
    }
}

impl WriteString for StorageClassSpecifier {
    fn write_string(&self) -> String {
        match self {
            StorageClassSpecifier::Typedef => "typedef".to_owned(),
            StorageClassSpecifier::Extern => "extern".to_owned(),
            StorageClassSpecifier::Static => "static".to_owned(),
            StorageClassSpecifier::ThreadLocal => "_Thread_local".to_owned(),
            StorageClassSpecifier::Auto => "auto".to_owned(),
            StorageClassSpecifier::Register => "register".to_owned(),
        }
    }
}

impl WriteString for TypeSpecifier {
    fn write_string(&self) -> String {
        match self {
            TypeSpecifier::Void => "void".to_owned(),
            TypeSpecifier::Char => "char".to_owned(),
            TypeSpecifier::Short => "short".to_owned(),
            TypeSpecifier::Int => "int".to_owned(),
            TypeSpecifier::Long => "long".to_owned(),
            TypeSpecifier::Float => "float".to_owned(),
            TypeSpecifier::Double => "double".to_owned(),
            TypeSpecifier::Signed => "signed".to_owned(),
            TypeSpecifier::Unsigned => "unsigned".to_owned(),
            TypeSpecifier::Bool => "bool".to_owned(),
            TypeSpecifier::Complex => unimplemented!("complex not implement"),
            TypeSpecifier::Atomic(_) => unimplemented!("atomic not implement"),
            TypeSpecifier::Struct(_) => "struct".to_owned(),
            TypeSpecifier::Enum(_) => unimplemented!("atomic not implement"),
            TypeSpecifier::TypedefName(_) => "typedef".to_owned(),
            TypeSpecifier::TypeOf(_) => unimplemented!("atomic not implement"),
            TypeSpecifier::TS18661Float(_) => unimplemented!("atomic not implement"),
        }
    }
}

impl WriteString for TypeQualifier {
    fn write_string(&self) -> String {
        match self {
            TypeQualifier::Const => "const",
            TypeQualifier::Restrict => "restrict",
            TypeQualifier::Volatile => "volatile",
            TypeQualifier::Nonnull => "_Nonull",
            TypeQualifier::NullUnspecified => "_Null_unspecified",
            TypeQualifier::Nullable => "_Nullable",
            TypeQualifier::Atomic => "_Atomic",
        }
        .to_owned()
    }
}

impl WriteString for InitDeclarator {
    fn write_string(&self) -> String {
        let declarator = self.declarator.write_string();
        if let Some(initializer) = &self.initializer {
            return std::format!("{} = {}", declarator, initializer.write_string());
        }
        declarator
    }
}

impl WriteString for Declarator {
    fn write_string(&self) -> String {
        let kind = self.kind.write_string();
        let krfunction = self
            .derived
            .clone()
            .iter()
            .filter(|declarator| {
                matches!(declarator.node, DerivedDeclarator::KRFunction(..))
                    || matches!(declarator.node, DerivedDeclarator::Function(..))
            })
            .map(|declarator| declarator.write_string())
            .join(",");
        let pointers = self
            .derived
            .clone()
            .iter()
            .filter(|declarator| matches!(declarator.node, DerivedDeclarator::Pointer(..)))
            .map(|declarator| declarator.write_string())
            .join("");
        let array = self
            .derived
            .clone()
            .iter()
            .filter(|declarator| matches!(declarator.node, DerivedDeclarator::Array(..)))
            .map(|derved| derved.write_string())
            .join("");
        pointers + &kind + &krfunction + &array
    }
}

impl WriteString for Expression {
    fn write_string(&self) -> String {
        match self {
            Expression::Identifier(id) => id.write_string(),
            Expression::Constant(cst) => cst.write_string(),
            // Expression::StringLiteral(_) => {}
            // Expression::GenericSelection(_) => {}
            // Expression::Member(_) => {}
            Expression::Call(call) => call.write_string(),
            // Expression::CompoundLiteral(_) => {}
            Expression::SizeOf(sizeof) => std::format!("sizeof({})", sizeof.write_string()),
            Expression::AlignOf(a) => std::format!("_Alignof({})", a.write_string()),
            Expression::UnaryOperator(uop) => uop.write_string(),
            // Expression::Cast(_) => {}
            Expression::BinaryOperator(binop) => binop.write_string(),
            // Expression::Conditional(_) => {}
            // Expression::Comma(_) => {}
            // Expression::OffsetOf(_) => {}
            // Expression::VaArg(_) => {}
            // Expression::Statement(_) => {}
            _ => "".to_owned(),
        }
    }
}

impl WriteString for TypeName {
    fn write_string(&self) -> String {
        let specifier = self.specifiers.write_string();
        if let Some(decl) = &self.declarator {
            return specifier + &decl.write_string();
        }
        specifier
    }
}

impl WriteString for SpecifierQualifier {
    fn write_string(&self) -> String {
        match self {
            SpecifierQualifier::TypeSpecifier(spec) => spec.write_string(),
            SpecifierQualifier::TypeQualifier(q) => q.write_string(),
        }
    }
}

impl WriteString for Identifier {
    fn write_string(&self) -> String {
        self.name.clone()
    }
}

impl WriteString for Constant {
    fn write_string(&self) -> String {
        match self {
            Constant::Integer(integer) => integer.write_string(),
            // Constant::Float(_) => {}
            // Constant::Character(_) => {}
            _ => unimplemented!("float, char NYI"),
        }
    }
}

impl WriteString for CallExpression {
    fn write_string(&self) -> String {
        let callee = self.callee.write_string();
        let arguments = self
            .arguments
            .iter()
            .map(|expr| expr.write_string())
            .join(", ");
        std::format!("{}({})", callee, arguments)
    }
}

impl WriteString for BinaryOperatorExpression {
    fn write_string(&self) -> String {
        let lhs = self.lhs.write_string();
        let rhs = self.rhs.write_string();
        let op = self.operator.write_string();
        if op == "[]" {
            return std::format!("{}[{}]", lhs, rhs);
        }
        std::format!("({} {} {})", lhs, op, rhs)
    }
}

impl WriteString for BinaryOperator {
    fn write_string(&self) -> String {
        match self {
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
        }
        .to_owned()
    }
}

impl WriteString for UnaryOperatorExpression {
    fn write_string(&self) -> String {
        if matches!(self.operator.node, UnaryOperator::PostDecrement)
            || matches!(self.operator.node, UnaryOperator::PostIncrement)
        {
            return std::format!(
                "({}{})",
                self.operand.write_string(),
                self.operator.write_string()
            );
        }
        std::format!(
            "({}{})",
            self.operator.write_string(),
            self.operand.write_string()
        )
    }
}

impl WriteString for UnaryOperator {
    fn write_string(&self) -> String {
        match self {
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
        }
        .to_owned()
    }
}
impl WriteString for Integer {
    fn write_string(&self) -> String {
        let base = match self.base {
            IntegerBase::Decimal => "",
            IntegerBase::Octal => "0o",
            IntegerBase::Hexadecimal => "0x",
        }
        .to_owned();
        let number = self.number.deref().to_owned();
        let suffix = self.suffix.write_string();
        std::format!("{}{}{}", base, number, suffix)
    }
}

impl WriteString for IntegerSuffix {
    fn write_string(&self) -> String {
        let mut suffix = String::new();
        if self.unsigned {
            suffix += "u";
        }
        suffix += match self.size {
            IntegerSize::Int => "",
            IntegerSize::Long => "l",
            IntegerSize::LongLong => "ll",
        }
        .to_owned()
        .deref();
        suffix
    }
}

impl WriteString for DeclaratorKind {
    fn write_string(&self) -> String {
        match self {
            DeclaratorKind::Abstract => "".to_owned(),
            DeclaratorKind::Identifier(id) => id.write_string(),
            DeclaratorKind::Declarator(decl) => decl.write_string(),
        }
    }
}

impl WriteString for DerivedDeclarator {
    fn write_string(&self) -> String {
        match self {
            DerivedDeclarator::Pointer(ptr) => std::format!("*{}", ptr.write_string()),
            DerivedDeclarator::Array(array) => array.write_string(),
            DerivedDeclarator::Function(func_decl) => func_decl.write_string(),
            DerivedDeclarator::KRFunction(func) => std::format!("({})", func.write_string()),
        }
    }
}

impl WriteString for PointerQualifier {
    fn write_string(&self) -> String {
        println!("pppppp");
        "*".to_owned()
    }
}

impl WriteString for ArrayDeclarator {
    fn write_string(&self) -> String {
        self.qualifiers.write_string() + &self.size.write_string()
    }
}

impl WriteString for ArraySize {
    fn write_string(&self) -> String {
        match self {
            ArraySize::Unknown => "[]".to_owned(),
            ArraySize::VariableUnknown => "[*]".to_owned(),
            ArraySize::VariableExpression(expr) => std::format!("[{}]", expr.write_string()),
            ArraySize::StaticExpression(expr) => std::format!("[{}]", expr.write_string()),
        }
    }
}

impl WriteString for FunctionDeclarator {
    fn write_string(&self) -> String {
        let parameters = self
            .parameters
            .iter()
            .map(|param| param.write_string())
            .join(",");
        std::format!("({})", parameters)
    }
}

impl WriteString for ParameterDeclaration {
    fn write_string(&self) -> String {
        let specifiers = self
            .specifiers
            .iter()
            .map(|specifier| specifier.write_string())
            .join(" ");
        let mut declarator = String::new();
        if let Some(decl) = &self.declarator {
            declarator = decl.write_string();
        }
        std::format!("{} {}", specifiers, declarator)
    }
}

impl WriteString for Ellipsis {
    fn write_string(&self) -> String {
        match self {
            Ellipsis::Some => unimplemented!("ellipse..."),
            Ellipsis::None => "".to_owned(),
        }
    }
}

impl WriteString for Extension {
    fn write_string(&self) -> String {
        unimplemented!("extension")
    }
}

impl WriteLine for StaticAssert {
    fn write_line(&self, _indent: usize, _write: &mut dyn Write) -> Result<()> {
        unimplemented!("static assert")
    }
}

impl WriteLine for FunctionDefinition {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        let specifiers = self
            .specifiers
            .iter()
            .map(|specifier| specifier.write_string())
            .join(" ");
        let declarator = self.declarator.write_string();
        write_indent(indent, write)?;
        write!(write, "{} {}\n", specifiers, declarator)?;
        write!(write, "{}", self.declarations.write_string())?;
        self.statement.write_line(indent, write)
    }
}

impl WriteLine for Statement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            Statement::Labeled(_) => todo!("label sattement"),
            Statement::Compound(block_items) => {
                write_indent(indent, write)?;
                write!(write, "{{\n")?;
                block_items.write_line(indent, write)?;
                write_indent(indent, write)?;
                write!(write, "}}\n")
            }
            Statement::Expression(expr) => {
                write_indent(indent, write)?;
                write!(write, "{};\n", expr.write_string())
            }
            Statement::If(if_stmt) => if_stmt.node.write_line(indent, write),
            Statement::Switch(_) => todo!("switch statement"),
            Statement::While(_) => todo!("while statement"),
            Statement::DoWhile(_) => todo!("do-while statement"),
            Statement::For(for_stmt) => for_stmt.write_line(indent, write),
            Statement::Goto(_) => todo!("goto statement"),
            Statement::Continue => todo!("continue statement"),
            Statement::Break => todo!("break statement"),
            Statement::Return(ret_stmt) => {
                if let Some(expr) = &ret_stmt {
                    write_indent(indent, write)?;
                    let expr = expr.write_string();
                    write!(write, "return {};\n", expr)?;
                }
                Ok(())
            }
            Statement::Asm(_) => todo!("asm statement"),
        }
    }
}

impl WriteLine for ForStatement {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        write_indent(indent, write)?;
        let initializer = self.initializer.write_string();
        let condition = self.condition.write_string();
        let step = self.step.write_string();
        write!(write, "for ({}; {}; {})\n", initializer, condition, step)?;
        self.statement.write_line(indent, write)
    }
}

impl WriteString for ForInitializer {
    fn write_string(&self) -> String {
        match self {
            ForInitializer::Empty => "".to_owned(),
            ForInitializer::Expression(expr) => expr.write_string(),
            ForInitializer::Declaration(decl) => decl.write_string(),
            ForInitializer::StaticAssert(_) => unimplemented!("_StaticAssert for"),
        }
    }
}

impl WriteLine for BlockItem {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        match self {
            BlockItem::Declaration(decl) => {
                let decl = decl.write_string();
                write_indent(indent + 1, write)?;
                write!(write, "{};\n", decl)
            }
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
        let condition = self.condition.write_string();
        write!(write, "if ({})\n", condition)?;
        self.then_statement.write_line(indent, write)?;
        if let Some(else_stmt) = &self.else_statement {
            else_stmt.write_line(indent, write)?;
        };
        Ok(())
    }
}

impl WriteString for Initializer {
    fn write_string(&self) -> String {
        match self {
            Initializer::Expression(expr) => expr.write_string(),
            Initializer::List(_) => unimplemented!("list initializer"),
        }
    }
}
