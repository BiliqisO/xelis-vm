mod scope;
mod context;
mod error;

pub use self::error::ParserError;
use self::context::Context;

use crate::{
    expressions::{Operator, Statement, Expression, DeclarationStatement, Parameter},
    functions::{CustomFunction, FunctionType},
    types::{Value, Type, Struct, RefMap},
    Environment,
    Token
};
use std::{
    borrow::Cow,
    collections::{HashMap, VecDeque},
    convert::TryInto
};

macro_rules! convert {
    ($a: expr) => {{
        match $a.try_into() {
            Ok(v) => v,
            Err(_) => return Err(ParserError::InvalidNumberValueForType)
        }
    }};
}

#[derive(Debug)]
pub struct Program {
    // All constants declared
    pub constants: Vec<DeclarationStatement>,
    // All structures declared
    pub structures: HashMap<String, Struct>,
    // All functions declared
    // TODO HashMap with signature as key
    pub functions: Vec<FunctionType>
}

// TODO: use a u64
type VariableId = String;

pub struct Parser<'a> {
    constants: Vec<DeclarationStatement>,
    tokens: VecDeque<Token>,
    functions: Vec<FunctionType>,
    structures: HashMap<String, Struct>,
    // Environment contains all the library linked to the program
    env: &'a Environment
}

impl<'a> Parser<'a> {
    pub fn new(tokens: VecDeque<Token>, env: &'a Environment) -> Self {
        Parser {
            constants: Vec::new(),
            tokens,
            functions: Vec::new(),
            structures: HashMap::new(),
            env,
        }
    }

    // Consume the next token
    fn advance(&mut self) -> Result<Token, ParserError> {
        self.tokens.pop_front().ok_or(ParserError::ExpectedToken)
    }

    // Consume the next token without error
    fn next(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    // Peek the next token without consuming it
    fn peek(&self) -> Result<&Token, ParserError> {
        self.tokens.front().ok_or(ParserError::ExpectedToken)
    }

    // Limited to 32 characters
    fn next_identifier(&mut self) -> Result<String, ParserError> {
        match self.advance()? {
            Token::Identifier(id) => if id.len() <= 32 {
                Ok(id)
            } else {
                Err(ParserError::VariableTooLong(id))
            },
            token => Err(ParserError::ExpectedIdentifierToken(token))
        }
    }

    // Check if the next token is an identifier
    fn peek_is_identifier(&self) -> bool {
        self.peek().ok().filter(|t| match t {
            Token::Identifier(_) => true,
            _ => false
        }).is_some()
    }

    // Require a specific token
    fn expect_token(&mut self, expected: Token) -> Result<(), ParserError> {
        let token = self.advance()?;
        if token != expected {
            return Err(ParserError::InvalidToken(token, expected)) 
        }
        Ok(())
    }

    /**
     * Example: let message: string[] = ["hello", "world", "!"];
     * Types: (unsigned)
     * - byte
     * - short
     * - int
     * - long
     * - string
     * - bool
     * - Struct (Structure with name that starts with a uppercase letter)
     * - T[] (where T is any above Type)
     */
    fn read_type(&mut self) -> Result<Type, ParserError> {
        let token = self.advance()?;
        let mut _type = match Type::from_token(token, &self.structures) {
            Some(v) => v,
            None => return Err(ParserError::TypeNotFound)
        };

        // support multi dimensional arrays
        loop {
            let token = self.advance()?;
            if token != Token::BracketOpen {
                // Push back
                // This allow us to economize one read per iteration on array type
                // by simply pushing back the token that we don't need
                self.tokens.push_front(token);
                break;
            }

            self.expect_token(Token::BracketClose)?;
            _type = Type::Array(Box::new(_type));
        }

        Ok(_type)
    }

    // this function don't verify, but only returns the type of an expression
    // all tests should be done when constructing an expression, not here
    fn get_type_from_expression(&self, on_type: Option<&Type>, expression: &Expression, context: &Context) -> Result<Type, ParserError> {
        let _type: Type = match expression {
            Expression::ArrayConstructor(ref values) => match values.first() {
                Some(v) => Type::Array(Box::new(self.get_type_from_expression(on_type, v, context)?)),
                None => return Err(ParserError::EmptyArrayConstructor) // cannot determine type from empty array
            },
            Expression::Variable(ref var_name) => match on_type {
                Some(t) => {
                    if let Type::Struct(struct_name) = t {
                        let structure = self.get_structure(struct_name)?;
                        if structure.fields.contains_key(var_name) {
                            structure.fields.get(var_name).unwrap().clone()
                        } else {
                            return Err(ParserError::UnexpectedVariable(var_name.clone()))
                        }
                    } else {
                        return Err(ParserError::UnexpectedVariable(var_name.clone()))
                    }
                },
                None => {
                    if context.has_variable(var_name) {
                        context.get_type_of_variable(var_name)?.clone()
                    } else {
                        return Err(ParserError::UnexpectedVariable(var_name.clone()))
                    }
                }
            
            },
            Expression::FunctionCall(name, parameters) => {
                let types: Vec<Cow<'_, Type>> = parameters.into_iter()
                    .map(|param| self.get_type_from_expression(on_type, param, context).map(Cow::Owned))
                    .collect::<Result<_, ParserError>>()?;

                let func = self.get_function(on_type, name, types.as_slice())?;
                match &func.return_type() {
                    Some(ref v) => v.clone(),
                    None => return Err(ParserError::FunctionNoReturnType(name.clone()))
                }
            },
            Expression::Value(ref val) => match Type::from_value(val, &RefMap::from_vec(vec![&self.structures, self.env.get_structures()])) { // we have to clone everything due to this
                Some(v) => v,
                None => return Err(ParserError::EmptyValue)
            },
            Expression::ArrayCall(path, _) => {
                match self.get_type_from_expression(on_type, path, context)? {
                    Type::Array(_type) => *_type,
                    _ => return Err(ParserError::InvalidArrayCall)
                }
            },
            Expression::SubExpression(expr) => self.get_type_from_expression(on_type, expr, context)?,
            Expression::StructConstructor(name, _) => {
                if !self.get_structure(name).is_ok() {
                    return Err(ParserError::StructureNotFound(name.clone()))
                }

                Type::Struct(name.clone())
            }
            Expression::Path(left, right) => {
                let var_type = self.get_type_from_expression(on_type, left, context)?;
                self.get_type_from_expression(Some(&var_type), right, context)?
            },
            // Compatibility checks are done when constructing the expression
            Expression::Operator(op, left, right) => match op {
                Operator::Or
                | Operator::Equals
                | Operator::NotEquals
                | Operator::GreaterOrEqual
                | Operator::GreaterThan
                | Operator::LessOrEqual
                | Operator::LessThan
                | Operator::And => Type::Boolean,
                Operator::Assign
                | Operator::AssignPlus
                | Operator::AssignMinus
                | Operator::AssignDivide
                | Operator::AssignMultiply => return Err(ParserError::AssignReturnNothing),
                Operator::Plus | Operator::Minus => {
                    let left_type = self.get_type_from_expression(on_type, left, context)?;
                    let right_type = self.get_type_from_expression(on_type, right, context)?;

                    if left_type == Type::String || right_type == Type::String {
                        Type::String
                    } else {
                        left_type
                    }
                },
                Operator::Multiply
                | Operator::Divide
                | Operator::BitwiseLeft
                | Operator::BitwiseRight
                | Operator::Modulo => {
                    let left_type = self.get_type_from_expression(on_type, left, context)?;
                    let right_type = self.get_type_from_expression(on_type, right, context)?;

                    if !left_type.is_number() || !right_type.is_number() || left_type != right_type {
                        return Err(ParserError::InvalidOperationNotSameType(left_type, right_type))
                    }
                    left_type
                },
            },
            Expression::IsNot(_) => Type::Boolean,
            Expression::Ternary(_, expr, _) => self.get_type_from_expression(on_type, expr, context)?,
            Expression::Cast(_, _type) => _type.clone()
        };

        Ok(_type)
    }

    // Read a function call with the following syntax:
    // function_name(param1, param2, ...)
    fn read_function_call(&mut self, on_type: Option<&Type>, id: String, context: &mut Context) -> Result<Expression, ParserError> {
        // we remove the token from the list
        self.expect_token(Token::ParenthesisOpen)?;
        let mut parameters: Vec<Expression> = Vec::new();
        let mut types: Vec<Cow<'_, Type>> = Vec::new();

        // read parameters for function call
        while *self.peek()? != Token::ParenthesisClose {
            let expr = self.read_expression(context)?;
            types.push(Cow::Owned(self.get_type_from_expression(on_type, &expr, context)?));
            parameters.push(expr);

            if *self.peek()? == Token::Comma {
                self.expect_token(Token::Comma)?;
            }
        }

        let func = self.get_function(on_type, &id, types.as_slice())?;
        // Function call cannot call entry function
        if func.is_entry() {
            return Err(ParserError::FunctionNotFound(id, parameters.len()))
        }

        self.expect_token(Token::ParenthesisClose)?;
        Ok(Expression::FunctionCall(id, parameters))
    }

    // Read a struct constructor with the following syntax:
    // struct_name { field_name: value1, field2: value2 }
    // If we have a field that has the same name as a variable we can pass it as following:
    // Example: struct_name { field_name, field2: value2 }
    fn read_struct_constructor(&mut self, on_type: Option<&Type>, id: String, context: &mut Context) -> Result<Expression, ParserError> {
        self.expect_token(Token::BraceOpen)?;
        // TODO no clone here
        // This is done due to borrowing struct
        let (name, types) = {
            let structure = self.get_structure(&id)?;
            let name = structure.name.clone();
            (name, structure.fields.iter().map(|(_, v)| v.clone()).collect::<Vec<_>>())
        };

        let mut fields = HashMap::new();
        for t in types {
            let field_name = self.next_identifier()?;
            let field_value = match self.advance()? {
                Token::Comma => {
                    if context.has_variable(&field_name) {
                        Expression::Variable(field_name.clone())
                    } else {
                        return Err(ParserError::UnexpectedVariable(field_name)) 
                    }
                }
                Token::Colon => {
                    let value = self.read_expr(on_type, true, Some(&t), context)?;
                    if *self.peek()? == Token::Comma {
                        self.advance()?;
                    }
                    value
                }
                token => {
                    return Err(ParserError::UnexpectedToken(token))
                }
            };

            let field_type = self.get_type_from_expression(on_type, &field_value, context)?;
            if t != field_type {
                return Err(ParserError::InvalidValueType(field_type, t))
            }

            fields.insert(field_name, field_value);
        }
        self.expect_token(Token::BraceClose)?;
        Ok(Expression::StructConstructor(name, fields))
    }

    // Read an expression with default parameters
    fn read_expression(&mut self, context: &mut Context) -> Result<Expression, ParserError> {
        self.read_expr(None, true, None, context)
    }

    // Read an expression with the possibility to accept operators
    // number_type is used to force the type of a number
    fn read_expr(&mut self, on_type: Option<&Type>, accept_operator: bool, number_type: Option<&Type>, context: &mut Context) -> Result<Expression, ParserError> {
        let mut required_operator = false;
        let mut last_expression: Option<Expression> = None;
        while {
            let peek = self.peek()?;
            !peek.should_stop() && ((required_operator == peek.is_operator()) || (*peek == Token::BracketOpen && last_expression.is_none()))
        } {
            if !accept_operator && required_operator {
                break;
            }

            let expr: Expression = match self.advance()? {
                Token::BracketOpen => {
                    match last_expression {
                        Some(v) => {
                            if !self.get_type_from_expression(on_type, &v, context)?.is_array() {
                                return Err(ParserError::InvalidArrayCall)
                            }

                            // Index must be of type Int
                            let index = self.read_expr(on_type, true, Some(&Type::Int), context)?;
                            let index_type = self.get_type_from_expression(on_type, &index, context)?;
                            if index_type != Type::Int {
                                return Err(ParserError::InvalidArrayCallIndexType(index_type))
                            }

                            self.expect_token(Token::BracketClose)?;
                            required_operator = !required_operator;
                            Expression::ArrayCall(Box::new(v), Box::new(index))
                        },
                        None => { // require at least one value in a array constructor
                            let mut expressions: Vec<Expression> = Vec::new();
                            let mut array_type: Option<Type> = None;
                            while *self.peek()? != Token::BracketClose {
                                let expr = self.read_expr(on_type, true, number_type.map(|t| t.get_array_type()), context)?;
                                match &array_type { // array values must have the same type
                                    Some(t) => {
                                        let _type = self.get_type_from_expression(on_type, &expr, context)?;
                                        if _type != *t {
                                            return Err(ParserError::InvalidTypeInArray(_type, t.clone()))
                                        }
                                    },
                                    None => { // first value rules the type of array
                                        array_type = Some(self.get_type_from_expression(on_type, &expr, context)?);
                                    }
                                };
                                expressions.push(expr);

                                if *self.peek()? == Token::Comma {
                                    self.expect_token(Token::Comma)?;
                                }
                            }

                            self.expect_token(Token::BracketClose)?;
                            Expression::ArrayConstructor(expressions)
                        }
                    }
                },
                Token::ParenthesisOpen => {
                    let expr = self.read_expression(context)?;
                    self.expect_token(Token::ParenthesisClose)?;
                    Expression::SubExpression(Box::new(expr))
                },
                Token::Identifier(id) => {
                    match self.peek()? {
                        // function call
                        Token::ParenthesisOpen => self.read_function_call(on_type, id, context)?,
                        _ => {
                            match on_type {
                                // mostly an access to a struct field
                                Some(t) => {
                                    if let Type::Struct(struct_name) = t {
                                        let structure = self.get_structure(struct_name)?;
                                        if structure.fields.contains_key(&id) {
                                            Expression::Variable(id)
                                        } else {
                                            return Err(ParserError::UnexpectedVariable(id))
                                        }
                                    } else {
                                        return Err(ParserError::UnexpectedType(t.clone()))
                                    }
                                },
                                None => {
                                    if context.has_variable(&id) {
                                        Expression::Variable(id)
                                    } else if self.structures.contains_key(&id) {
                                        self.read_struct_constructor(on_type, id, context)?
                                    } else {
                                        return Err(ParserError::UnexpectedVariable(id))
                                    }
                                }
                            }
                        }
                    }
                },
                Token::IntValue(value) => Expression::Value(match number_type {
                    Some(t) => match t {
                        Type::Byte => Value::Byte(convert!(value)),
                        Type::Short => Value::Short(convert!(value)),
                        Type::Int => Value::Int(value),
                        Type::Long => Value::Long(convert!(value)),
                        _ => return Err(ParserError::ExpectedNumberType)
                    },
                    None => Value::Int(value)
                }),
                Token::LongValue(value) => Expression::Value(Value::Long(value)),
                Token::StringValue(value) => Expression::Value(Value::String(value)),
                Token::True => Expression::Value(Value::Boolean(true)),
                Token::False => Expression::Value(Value::Boolean(false)),
                Token::Null => Expression::Value(Value::Null),
                Token::Dot => {
                    match last_expression {
                        Some(value) => {
                            let _type = self.get_type_from_expression(on_type, &value, context)?;
                            let right_expr = self.read_expr(Some(&_type), false, None, context)?;
                            // because we read operator DOT + right expression
                            required_operator = !required_operator;
                            Expression::Path(Box::new(value), Box::new(right_expr))
                        },
                        None => return Err(ParserError::UnexpectedToken(Token::Dot))
                    }
                },
                Token::IsNot => { // it's an operator, but not declared as
                    let expr = self.read_expression(context)?;
                    let expr_type = self.get_type_from_expression(on_type, &expr, context)?;
                    if expr_type != Type::Boolean {
                        return Err(ParserError::InvalidValueType(expr_type, Type::Boolean))
                    }

                    Expression::IsNot(Box::new(expr))
                },
                Token::OperatorTernary => match last_expression { // condition ? expr : expr
                    Some(expr) => {
                        if self.get_type_from_expression(on_type, &expr, context)? != Type::Boolean {
                            return Err(ParserError::InvalidCondition(expr, Type::Boolean))
                        }

                        let valid_expr = self.read_expr(on_type, true, number_type, context)?;
                        let first_type = self.get_type_from_expression(on_type, &valid_expr, context)?;
                        self.expect_token(Token::Colon)?;
                        let else_expr = self.read_expr(on_type, true, number_type, context)?;
                        let else_type = self.get_type_from_expression(on_type, &else_expr, context)?;
                        
                        if first_type != else_type { // both expr should have the SAME type.
                            return Err(ParserError::InvalidValueType(else_type, first_type))
                        }
                        required_operator = !required_operator;
                        Expression::Ternary(Box::new(expr), Box::new(valid_expr), Box::new(else_expr))
                    },
                    None => return Err(ParserError::InvalidTernaryNoPreviousExpression)
                },
                token => {
                    if !token.is_operator() {
                        return Err(ParserError::UnexpectedToken(token))
                    }

                    match last_expression {
                        Some(previous_expr) => {
                            required_operator = !required_operator;

                            let left_type = self.get_type_from_expression(on_type, &previous_expr, context)?;
                            if token == Token::As {
                                let right_type = self.read_type()?;
                                if !left_type.is_castable_to(&right_type) {
                                    return Err(ParserError::CastError(left_type, right_type))
                                }

                                Expression::Cast(Box::new(previous_expr), right_type)
                            } else {
                                // Parse the operator for this token
                                let op = match Operator::value_of(&token) {
                                    Some(op) => op,
                                    None => return Err(ParserError::OperatorNotFound(token))
                                };

                                let expr = self.read_expr(on_type, true, Some(&left_type), context)?;
                                let right_type = self.get_type_from_expression(on_type, &expr, context)?;
    
                                match &op {
                                    Operator::Minus | Operator::Modulo | Operator::Divide | Operator::Multiply
                                    | Operator::AssignMinus | Operator::AssignDivide | Operator::AssignMultiply
                                    | Operator::BitwiseLeft | Operator::BitwiseRight
                                    | Operator::GreaterThan | Operator::LessThan | Operator::LessOrEqual
                                    | Operator::GreaterOrEqual => {
                                        if left_type != right_type || !left_type.is_number() || !right_type.is_number() {
                                            return Err(ParserError::InvalidOperationNotSameType(left_type, right_type))
                                        }
                                    },
                                    Operator::Plus => {
                                        if left_type != Type::String && right_type != Type::String {
                                            if left_type != right_type {
                                                return Err(ParserError::InvalidOperationNotSameType(left_type, right_type))
                                            }
                                        }
                                    },
                                    Operator::And | Operator::Or => {
                                        if left_type != Type::Boolean {
                                            return Err(ParserError::InvalidOperationNotSameType(left_type, Type::Boolean))
                                        }
    
                                        if right_type != Type::Boolean {
                                            return Err(ParserError::InvalidOperationNotSameType(right_type, Type::Boolean))
                                        }
                                    },
                                    _ => if left_type != right_type {
                                        return Err(ParserError::InvalidOperationNotSameType(left_type, right_type))
                                    }
                                };
    
                                Expression::Operator(op, Box::new(previous_expr), Box::new(expr))
                            }
                        }
                        None => return Err(ParserError::InvalidOperation)
                    }
                }
            };

            last_expression = Some(expr);
            required_operator = !required_operator;
        }

        match last_expression {
            Some(v) => Ok(v),
            None => Err(ParserError::NotImplemented)
        }
    }

    /**
     * {
     *     ...
     * }
     */
    fn read_body(&mut self, context: &mut Context, return_type: &Option<Type>, consume_brace: bool) -> Result<Vec<Statement>, ParserError> {
        context.begin_scope();
        let statements = self.read_statements(context, return_type, consume_brace)?;
        context.end_scope();
        Ok(statements)
    }

    /**
     * Example: let hello: string = "hello";
     * Rules:
     * - Every variable must be declared with 'let' keyword
     * - Variable name must be alphanumeric characters
     * - Must provide a value type
     * - If no value is set, Null is set by default
     */
    fn read_variable(&mut self, context: &mut Context) -> Result<DeclarationStatement, ParserError> {
        let name = self.next_identifier()?;
        if context.has_variable(&name) {
            return Err(ParserError::VariableNameAlreadyUsed(name))
        }

        self.expect_token(Token::Colon)?;
        let value_type = self.read_type()?;
        let value: Expression = if *self.peek()? == Token::OperatorAssign {
            self.expect_token(Token::OperatorAssign)?;
            let expr = self.read_expr(None, true, Some(&value_type), context)?;

            let expr_type = match self.get_type_from_expression(None, &expr, context) {
                Ok(_type) => _type,
                Err(e) => match e { // support empty array declaration
                    ParserError::EmptyArrayConstructor if value_type.is_array() => value_type.clone(),
                    _ => return Err(e)
                }
            };
            if !expr_type.is_compatible_with(&value_type) {
                return Err(ParserError::InvalidValueType(expr_type, value_type))
            }

            expr
        } else {
            Expression::Value(Value::Null)
        };

        context.register_variable(name.clone(), value_type.clone())?;

        Ok(DeclarationStatement {
            name,
            value_type,
            value
        })
    }

    fn read_loop_body(&mut self, context: &mut Context, return_type: &Option<Type>) -> Result<Vec<Statement>, ParserError> {
        let old_value = context.is_in_a_loop(); // support loop in loop
        context.set_in_a_loop(true);
        let statements = self.read_body(context, return_type, true)?;
        context.set_in_a_loop(old_value);

        Ok(statements)
    }

    // Read all statements in a block
    // return type is used to verify that the last statement is a return with a valid value type
    // consume_brace is used to know if we should consume the open brace
    fn read_statements(&mut self, context: &mut Context, return_type: &Option<Type>, consume_brace: bool) -> Result<Vec<Statement>, ParserError> {
        if consume_brace {
            self.expect_token(Token::BraceOpen)?;
        }

        let mut statements: Vec<Statement> = Vec::new();
        let mut has_if = false;
        loop {
            let token = self.advance()?;
            let statement: Statement = match token {
                Token::BraceClose => break,
                Token::For => { // Example: for i: int = 0; i < 10; i += 1 {}
                    context.begin_scope();
                    let var = self.read_variable(context)?;
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  condition_type != Type::Boolean {
                        return Err(ParserError::InvalidCondition(condition, condition_type))
                    }

                    let increment = self.read_expression(context)?;
                    match &increment { // allow only assignations on this expr
                        Expression::Operator(op, _, _) if op.is_assignation() => {},
                        _ => return Err(ParserError::InvalidForExpression(increment))
                    };

                    let statements = self.read_loop_body(context, return_type)?;
                    context.end_scope();

                    Statement::For(var, condition, increment, statements)
                }
                Token::ForEach => { // Example: foreach a in array {}
                    context.begin_scope();
                    let variable: String = match self.advance()? {
                        Token::Identifier(v) => v,
                        token => return Err(ParserError::UnexpectedToken(token))
                    };
                    self.expect_token(Token::In)?;
                    let expr = self.read_expression(context)?;
                    let expr_type = self.get_type_from_expression(None, &expr, context)?;
                    if !expr_type.is_array() { // verify that we can iter on it
                        return Err(ParserError::InvalidValueType(expr_type, Type::Array(Box::new(Type::Any))))
                    }
                    context.register_variable(variable.clone(), expr_type.get_array_type().clone())?;
                    let statements = self.read_loop_body(context, return_type)?;
                    context.end_scope();

                    Statement::ForEach(variable, expr, statements)
                },
                Token::While => { // Example: while i < 10 {}
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  condition_type != Type::Boolean {
                        return Err(ParserError::InvalidCondition(condition, condition_type))
                    }

                    let statements = self.read_loop_body(context, return_type)?;

                    Statement::While(condition, statements)
                },
                Token::If => {
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  condition_type != Type::Boolean {
                        return Err(ParserError::InvalidCondition(condition, condition_type))
                    }

                    Statement::If(condition, self.read_body(context, return_type, true)?)
                },
                Token::Else => {
                    if !has_if {
                        return Err(ParserError::NoIfBeforeElse)
                    }

                    if *self.peek()? == Token::If {
                        self.expect_token(Token::If)?;
                        let condition = self.read_expression(context)?;
                        let condition_type = self.get_type_from_expression(None, &condition, context)?;
                        if  condition_type != Type::Boolean {
                            return Err(ParserError::InvalidCondition(condition, condition_type))
                        }
                        Statement::ElseIf(condition, self.read_body(context, return_type, true)?)
                    } else {
                        Statement::Else(self.read_body(context, return_type, true)?)
                    }
                },
                Token::BraceOpen => Statement::Scope(self.read_body(context, return_type, false)?),
                Token::Let => Statement::Variable(self.read_variable(context)?),
                Token::Return => {
                    let opt: Option<Expression> = if let Some(return_type) = return_type {
                        let expr = self.read_expr(None, true, Some(return_type), context)?;
                        let expr_type = self.get_type_from_expression(None, &expr, context)?;
                        if expr_type != *return_type {
                            return Err(ParserError::InvalidValueType(expr_type, return_type.clone()))
                        }
                        Some(expr)
                    } else {
                        None
                    };

                    // we can't have anything after a return
                    if *self.peek()? != Token::BraceClose {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Return(opt)
                }
                Token::Continue => {
                    if !context.is_in_a_loop() {
                        return Err(ParserError::UnexpectedToken(Token::Continue));
                    }

                    // we can't have anything after a continue
                    if *self.peek()? != Token::BraceClose {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Continue
                },
                Token::Break => {
                    if !context.is_in_a_loop() {
                        return Err(ParserError::UnexpectedToken(Token::Break));
                    }

                    // we can't have anything after a break
                    if *self.peek()? != Token::BraceClose {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Break
                },
                token => {
                    self.tokens.push_front(token);
                    Statement::Expression(self.read_expression(context)?)
                }
            };

            match &statement {
                Statement::If(_, _) | Statement::ElseIf(_, _) => {
                    has_if = true;
                },
                _ => {
                    has_if = false;
                }
            };

            statements.push(statement);
        }

        Ok(statements)
    }

    // Read the parameters for a function
    fn read_parameters(&mut self) -> Result<Vec<Parameter>, ParserError> {
        let mut parameters: Vec<Parameter> = Vec::new();
        while self.peek_is_identifier() {
            let name = self.next_identifier()?;
            self.expect_token(Token::Colon)?;
            let value_type = self.read_type()?;

            // verify that we have unique names here
            if parameters.iter().any(|p| *p.get_name() == name) {
                return Err(ParserError::VariableNameAlreadyUsed(name))
            }

            parameters.push(Parameter::new(name, value_type));

            if *self.peek()? != Token::Comma {
                break;
            }

            self.expect_token(Token::Comma)?;
        }

        Ok(parameters)
    }

    // Verify that the last statement is a return
    // We don't check the last statement directly has it would allow
    // to have dead code after a return
    fn ends_with_return(&self, statements: &Vec<Statement>) -> Result<bool, ParserError> {
        let mut ok = false;
        let mut last_is_else = false;
        let size = statements.len();
        for (i, statement) in statements.into_iter().enumerate() {
            match statement {
                Statement::If(_, statements) => {
                    if i + 1 < size { // verify that there is not a if alone
                        ok = self.ends_with_return(statements)?;
                    }
                    last_is_else = false;
                }
                Statement::ElseIf(_, statements) => {
                    if ok {
                        ok = self.ends_with_return(statements)?;
                    }
                    last_is_else = false;
                }
                Statement::Else(statements) => {
                    if ok {
                        ok = self.ends_with_return(statements)?;
                        if ok {
                            last_is_else = true;
                            
                            if i + 1 < size { // if we have all case that returns something, then we don't allow dead code.
                                return Err(ParserError::DeadCodeNotAllowed) 
                            }
                        }
                    }
                }
                Statement::Return(_) => {
                    if last_is_else { // this return would be useless because if & else have a return, so we don't accept this one.
                        return Err(ParserError::ReturnAlreadyInElse)
                    }

                    ok = true;
                },
                _ => {}
            }
        }

        Ok(ok)
    }

    /**
     * Examples:
     * - entry foo() { ... }
     * - func foo() { ... }
     * - func foo(): int { ... }
     * - func foo(a: int, b: int) { ... }
     * - func (f Foo) bar() { ... }
     * Rules:
     * - Signature is based on function name, and parameters
     * - Entry function is a "public callable" function and must return a int value
     */
    fn read_function(&mut self, entry: bool, context: &mut Context) -> Result<(), ParserError> {
        context.begin_scope();

        let token = self.advance()?;
        let (instance_name, for_type, name) = if !entry && token == Token::ParenthesisOpen {
            let instance_name = self.next_identifier()?;
            let for_type = self.read_type()?;

            // verify that the type is a struct
            if let Type::Struct(struct_name) = &for_type {
                // only types that are declared by the same program
                if !self.structures.contains_key(struct_name) {
                    return Err(ParserError::StructureNotFound(struct_name.clone()))
                }
            } else {
                return Err(ParserError::InvalidFunctionType(for_type))
            }

            context.register_variable(instance_name.clone(), for_type.clone())?;
            self.expect_token(Token::ParenthesisClose)?;

            (Some(instance_name), Some(for_type), self.next_identifier()?)
        } else {
            let Token::Identifier(name) = token else {
                return Err(ParserError::ExpectedIdentifierToken(token))
            };
            (None, None, name)
        };

        self.expect_token(Token::ParenthesisOpen)?;
        let parameters = self.read_parameters()?;
        self.expect_token(Token::ParenthesisClose)?;

        // all entries must return a int value without being specified
        let return_type: Option<Type> = if entry {
            // an entrypoint cannot be a method
            if for_type.is_some() {
                return Err(ParserError::EntryFunctionCannotHaveForType)
            }

            Some(Type::Int)
        } else if *self.peek()? == Token::Colon { // read returned type
            self.advance()?;
            Some(self.read_type()?)
        } else {
            None
        };

        let types: Vec<Cow<'_, Type>> = parameters.iter().map(|p| Cow::Borrowed(p.get_type())).collect();
        if self.has_function(for_type.as_ref(), &name, &types) {
            return Err(ParserError::FunctionSignatureAlreadyExist(name)) 
        }

        let has_return_type = return_type.is_some();

        for param in parameters.iter() {
            context.register_variable(param.get_name().clone(), param.get_type().clone())?;
        }

        let statements = Vec::new();
        let function = FunctionType::Custom(CustomFunction::new(
            name,
            for_type,
            instance_name,
            parameters,
            statements,
            entry,
            return_type.clone()
        ));

        // push function before reading statements to allow recursive calls
        self.functions.push(function);

        let statements = self.read_body(context, &return_type, true)?;

        // verify that the function ends with a return
        if has_return_type && !self.ends_with_return(&statements)? {
            return Err(ParserError::NoReturnFound)
        }

        context.end_scope();

        match self.functions.last_mut() {
            Some(function) => {
                if let FunctionType::Custom(f) = function {
                    f.set_statements(statements);
                }
            },
            None => return Err(ParserError::FunctionNotFound(String::from("last one"), 0)) // shouldn't happen
        };

        Ok(())
    }

    // check if a function with the same signature exists
    fn has_function(&self, for_type: Option<&Type>, name: &String, params: &[Cow<'_, Type>]) -> bool {
        self.get_function(for_type, name, params).is_ok()
    }

    // get a function exist based on signature (name + params)
    fn get_function(&self, for_type: Option<&Type>, name: &String, params: &[Cow<'_, Type>]) -> Result<&FunctionType, ParserError> {
        // merge both iterators in one
        let functions = self.functions.iter().map(|v| v).chain(self.env.get_functions());

        'funcs: for f in functions {
            if *f.get_name() == *name && f.get_parameters_count() == params.len() {
                let same_type: bool = if let Some(type_a) = for_type {
                    if let Some(type_b) = f.for_type() {
                        type_a.is_compatible_with(type_b)
                    } else {
                        false
                    }
                } else {
                    let f_type = f.for_type();
                    if let (Some(left), Some(right)) = (for_type, f_type) {
                        left == right
                    } else {
                        for_type.is_none() && f_type.is_none()
                    }
                };

                if same_type {
                    let types = f.get_parameters_types();
                    for (param, param_type) in params.into_iter().zip(types.into_iter()) {
                        if *param.as_ref() != *param_type && *param_type != Type::Any {
                            continue 'funcs;
                        }
                    }
    
                    return Ok(&f);
                }
            }
        }

        Err(ParserError::FunctionNotFound(name.clone(), params.len()))
    }

    // get a structure by name
    fn get_structure(&self, name: &String) -> Result<&Struct, ParserError> {
        match self.structures.get(name) {
            Some(v) => Ok(v),
            None => match self.env.get_structure(name) {
                Some(v) => Ok(v),
                None => return Err(ParserError::StructureNotFound(name.clone()))
            }
        }
    }

    /**
     * Example: Message { message_id: int, message: string }
     * Rules:
     * - Structure name should start with a uppercase character
     * - only alphanumeric chars in name
     */
    fn read_struct(&mut self) -> Result<Struct, ParserError> {
        let name = self.next_identifier()?;
        let mut chars = name.chars();
        if !chars.all(|c| c.is_ascii_alphanumeric()) {
            return Err(ParserError::InvalidStructureName(name))
        }

        // check if the first letter is in uppercase
        match name.chars().nth(0) {
            Some(v) => {
                if !v.is_ascii_alphabetic() || !v.is_uppercase() {
                    return Err(ParserError::InvalidStructureName(name))
                }
            },
            None => return Err(ParserError::EmptyValue)
        };

        self.expect_token(Token::BraceOpen)?;
        let mut fields: HashMap<String, Type> = HashMap::new();
        for param in self.read_parameters()? {
            let (name, _type) = param.consume();
            fields.insert(name, _type);
        }
        self.expect_token(Token::BraceClose)?;

        Ok(Struct {
            name,
            fields
        })
    }

    // Parse the tokens and return a Program
    pub fn parse(mut self) -> Result<Program, ParserError> {
        let mut context: Context = Context::new();
        while let Some(token) = self.next() {
            match token {
                Token::Import => return Err(ParserError::NotImplemented), // TODO
                Token::Const => {
                    let var = self.read_variable(&mut context)?;
                    self.constants.push(var);
                },
                Token::Function => self.read_function(false, &mut context)?,
                Token::Entry => self.read_function(true, &mut context)?,
                Token::Struct => {
                    let new_struct = self.read_struct()?;
                    if self.structures.contains_key(&new_struct.name) {
                        return Err(ParserError::StructNameAlreadyUsed(new_struct.name))
                    }

                    self.structures.insert(new_struct.name.clone(), new_struct);
                },
                token => return Err(ParserError::UnexpectedToken(token))
            };
        }

        Ok(Program {
            constants: self.constants,
            structures: self.structures,
            functions: self.functions,
        })
    }
}