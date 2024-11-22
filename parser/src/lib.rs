mod context;
mod error;

use std::{
    borrow::Cow,
    collections::{HashSet, VecDeque}
};
use xelis_builder::{
    EnvironmentBuilder,
    FunctionMapper,
    StructManager
};
use xelis_ast::*;
use xelis_environment::NativeFunction;
use xelis_types::*;
use context::Context;

pub use error::ParserError;

enum Function<'a> {
    Native(&'a NativeFunction),
    Program(&'a FunctionType)
}

impl<'a> Function<'a> {
    fn return_type(&self) -> &Option<Type> {
        match self {
            Function::Native(f) => f.return_type(),
            Function::Program(f) => f.return_type()
        }
    }

    fn is_entry(&self) -> bool {
        match self {
            Function::Program(f) => f.is_entry(),
            _ => false
        }
    }
}

pub struct Parser<'a> {
    // Tokens to process
    tokens: VecDeque<Token<'a>>,
    // All constants declared
    constants: HashSet<DeclarationStatement>,
    // All functions registered by the program
    functions: Vec<FunctionType>,
    // Functions mapper
    // It will contains all the functions declared in the program
    // with the matching id <-> function signature
    functions_mapper: FunctionMapper<'a>,
    // Struct manager
    struct_manager: StructManager<'a>,
    // Environment contains all the library linked to the program
    environment: &'a EnvironmentBuilder<'a>,
    // TODO: Path to use to import files
    // _path: Option<&'a str>
}

impl<'a> Parser<'a> {
    pub fn new(tokens: VecDeque<Token<'a>>, environment: &'a EnvironmentBuilder) -> Self {
        let functions_mapper = FunctionMapper::with_parent(environment.get_functions_mapper());

        Parser {
            tokens,
            constants: HashSet::new(),
            functions: Vec::new(),
            functions_mapper,
            struct_manager: StructManager::with_parent(environment.get_struct_manager()),
            environment
        }
    }

    // Consume the next token
    #[inline(always)]
    fn advance(&mut self) -> Result<Token<'a>, ParserError<'a>> {
        self.tokens.pop_front().ok_or(ParserError::ExpectedToken)
    }

    // Consume the next token without error
    #[inline(always)]
    fn next(&mut self) -> Option<Token<'a>> {
        self.tokens.pop_front()
    }

    // Peek the next token without consuming it
    #[inline(always)]
    fn peek(&self) -> Result<&Token<'a>, ParserError<'a>> {
        self.tokens.front().ok_or(ParserError::ExpectedToken)
    }

    // Limited to 32 characters
    #[inline(always)]
    fn next_identifier(&mut self) -> Result<&'a str, ParserError<'a>> {
        match self.advance()? {
            Token::Identifier(id) => Ok(id),
            token => Err(ParserError::ExpectedIdentifierToken(token))
        }
    }

    // Check if the next token is a specific token
    #[inline(always)]
    fn peek_is(&self, token: Token<'a>) -> bool {
        self.tokens.front().filter(|t| **t == token).is_some()
    }

    // Check if the next token is not a specific token
    #[inline(always)]
    fn peek_is_not(&self, token: Token<'a>) -> bool {
        self.tokens.front().filter(|t| **t != token).is_some()
    }

    // Check if the next token is an identifier
    #[inline(always)]
    fn peek_is_identifier(&self) -> bool {
        self.peek().ok().filter(|t| match t {
            Token::Identifier(_) => true,
            _ => false
        }).is_some()
    }

    // Require a specific token
    fn expect_token(&mut self, expected: Token<'a>) -> Result<(), ParserError<'a>> {
        let token = self.advance()?;
        if token != expected {
            return Err(ParserError::InvalidToken(token, expected)) 
        }
        Ok(())
    }

    fn get_type_from_token(&self, token: Token<'a>) -> Result<Type, ParserError<'a>> {
    let mut current_token = token;
    let mut optional_stack = Vec::new();
    let mut range_stack = Vec::new();

    loop {
        match current_token {
            Token::Number(inner) => {
                return Ok(match inner {
                    NumberType::U8 => Type::U8,
                    NumberType::U16 => Type::U16,
                    NumberType::U32 => Type::U32,
                    NumberType::U64 => Type::U64,
                    NumberType::U128 => Type::U128,
                    NumberType::U256 => Type::U256,
                });
            }
            Token::String => return Ok(Type::String),
            Token::Bool => return Ok(Type::Bool),
            Token::Optional(ref inner) => {
                optional_stack.push(current_token.clone());
                current_token = *inner.clone();
            }
            Token::Range(ref inner) => {
                range_stack.push(current_token.clone());
                current_token = *inner.clone();
            }
            Token::Identifier(id) => {
                if let Ok(v) = self.struct_manager.get_by_name(id) {
                    let mut type_result = Type::Struct(v.inner().clone());
                    
                    while let Some(_) = optional_stack.pop() {
                        type_result = Type::Optional(Box::new(type_result));
                    }

                    while let Some(_) = range_stack.pop() {
                        type_result = Type::Range(Box::new(type_result));
                    }

                    return Ok(type_result);
                } else {
                    return Err(ParserError::StructNotFound(id));
                }
            }
            token => return Err(ParserError::UnexpectedToken(token)),
        }
    }
}

    /**
     * Example: let message: string[] = ["hello", "world", "!"];
     * Types: (unsigned)
     * - u8
     * - u16
     * - u64
     * - u128
     * - u256
     * - string
     * - bool
     * - Struct (Structure with name that starts with a uppercase letter)
     * - T[] (where T is any above Type)
     */
    fn read_type(&mut self) -> Result<Type, ParserError<'a>> {
        let token = self.advance()?;
        let mut _type = self.get_type_from_token(token)?;

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

    // get the type of an expression
    fn get_type_from_expression<'b>(&'b self, on_type: Option<&'b Type>, expression: &'b Expression, context: &'b Context<'a>) -> Result<Cow<'b, Type>, ParserError<'a>> {
        match self.get_type_from_expression_internal(on_type, expression, context)? {
            Some(v) => Ok(v),
            None => Err(ParserError::EmptyValue)
        }
    }

    // this function don't verify, but only returns the type of an expression
    // all tests should be done when constructing an expression, not here
fn get_type_from_expression_internal<'b>(
    &'b self,
    on_type: Option<&'b Type>,
    expression: &'b Expression,
    context: &'b Context<'a>,
) -> Result<Option<Cow<'b, Type>>, ParserError<'a>> {
    let mut stack: Vec<(&'b Expression, Option<&Type>)> = vec![(expression, on_type)];
    let mut current_type: Option<Cow<'b, Type>> = None;

    while let Some((expr, current_on_type)) = stack.pop() {
        match expr {
            Expression::ArrayConstructor(ref values) => {
                println!("Resolved type: 11"); // Logging for debugging
            if values.is_empty() {
                // If the array is empty, check if the type is explicitly defined
                if let Some(Type::Array(inner)) = current_on_type {
                    current_type = Some(Cow::Owned(Type::Array(inner.clone())));
                } else {
                    return Err(ParserError::EmptyArrayConstructor);
                }
            } else {
                // Use a stack to iteratively check all elements
                let mut stack: Vec<&Expression> = values.iter().collect();
                let mut array_type: Option<Type> = None;

                while let Some(current_expr) = stack.pop() {
                    // Process the current expression to determine its type
                    let element_type = match *current_expr {
                        Expression::Value(ref val) => {
                            Type::from_value(val).ok_or(ParserError::EmptyArrayConstructor)?
                        }
                        Expression::Variable(ref var_name) => {
                            context.get_type_of_variable(var_name)?.clone()
                        }
                        _ => return Err(ParserError::EmptyArrayConstructor), // Handle other cases if needed
            };

                    // Check type consistency
                    match array_type {
                        Some(ref ty) if *ty != element_type => {
                            return Err(ParserError::EmptyArrayConstructor);
                        }
                        _ => array_type = Some(element_type),
                    }
                }

                // If all elements have consistent types, set the array type
                if let Some(final_type) = array_type {
                    current_type = Some(Cow::Owned(Type::Array(Box::new(final_type))));
                } else {
                    return Err(ParserError::EmptyArrayConstructor);
                }
            }
        },
            Expression::Variable(ref var_name) => match on_type {
                                Some(t) => {
                                    
                                    if let Type::Struct(_type) = t {
                                        let index = *var_name as usize;
                                if let Some(field_type) = _type.fields().get(index) {
                                current_type = Some(Cow::Owned(field_type.clone()));
                                } else {
                                    return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()))
                                }
                            } else {
                                return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()))
                            }
                        },
                        None => current_type = Some(Cow::Borrowed(context.get_type_of_variable(var_name)?)),
        },
            Expression::FunctionCall(path, name, _) => {
                    println!("Resolving FunctionCall");
                    let f = self.get_function(*name)?;
                    let return_type = f.return_type();

                    match return_type {
                        Some(ref v) => match v {
                    Type::T => {
                        if let Some(context_type) = current_on_type {
                            current_type = Some(Cow::Owned(context_type.get_inner_type().clone()));
                        } else if let Some(p) = path {
                            let mut path_stack: Vec<&Expression> = vec![p];
                            let mut resolved_type: Option<Cow<'b, Type>> = None;

                            while let Some(path_expr) = path_stack.pop() {
                                match path_expr {
                                    Expression::Variable(var_name) => {
                                        resolved_type = Some(Cow::Borrowed(context.get_type_of_variable(var_name)?));
                                        
                                    }
                                    Expression::Path(left, right) => {
                                        path_stack.push(right);
                                        path_stack.push(left);
                                    }
                                    _ => { 
                                resolved_type = Some(Cow::Owned(v.clone()))
                                },
                                }
                            }

                            if let Some(resolved) = resolved_type {
                                current_type = Some(Cow::Owned(resolved.into_owned()));
                            } else {
                            
                                return Err(ParserError::InvalidTypeT);
                            }
                        } else {
                            
                            return Err(ParserError::InvalidTypeT);
                        }
                    }
                    Type::Optional(inner) => match inner.as_ref() {
                        Type::T => {
                            if let Some(context_type) = current_on_type {
                                current_type = Some(Cow::Owned(Type::Optional(Box::new(context_type.get_inner_type().clone()))));
                            } else if let Some(p) = path {
                                let mut path_stack: Vec<&Expression> = vec![p];
                                let mut resolved_type: Option<Cow<'b, Type>> = None;

                                while let Some(path_expr) = path_stack.pop() {
                                    match path_expr {
                                        Expression::Variable(var_name) => { 
                                            resolved_type = Some(Cow::Borrowed(context.get_type_of_variable(var_name)?));

                                            println!("current_terpe {:?}", resolved_type);
                                        }
                                        Expression::Path(left, right) => {
                                            path_stack.push(right);
                                            path_stack.push(left);
                                        }
                                        _ =>{ 
                                current_type = Some(Cow::Owned(*inner.clone()));
                                return Ok(current_type);
                            
                                        },
                                    }
                                }
                                if let Some(resolved) = resolved_type {
                                    match resolved.into_owned() {
                                        Type::Array(inner_array_type) => {
                                            current_type = Some(Cow::Owned(Type::Optional(inner_array_type)));
                                            println!("current_tpe {:?}", current_type);
                                        }
                                        other_type => {
                                            current_type = Some(Cow::Owned(Type::Optional(Box::new(other_type))));
                                        }
                                    }
                                } else {
                                current_type = Some(Cow::Owned(*inner.clone()));
                                println!("current_tpe {:?}", current_type);
                                return  Ok(current_type);
                                }
                            } else {
                                current_type = Some(Cow::Owned(v.clone()));
                                return  Ok(current_type);
                            }
                        }
                        _ => {
                            current_type = Some(Cow::Owned(*inner.clone()));
                        }
                    },
                    _ => {
                        current_type = Some(Cow::Owned(v.clone()));
                    }
                },
                None => return Err(ParserError::FunctionNoReturnType),
            }
        },


            
            Expression::ArrayCall(path, _) => {
                println!("Resolved type:8 "); // Logging for debugging
                let mut stack: Vec<&Expression> = vec![path];
                let mut current_type: Option<Cow<'b, Type>> = None;

                while let Some(current_expr) = stack.pop() {
                    match current_expr {
                        Expression::Variable(var_name) => {
                            if let Some(_type) = current_type.as_ref() {
                                match _type.as_ref() {
                                    Type::Array(inner_type) => {
                                        current_type = Some(Cow::Owned(*inner_type.clone()));
                                    }
                                    _ => {
                                        return Err(ParserError::InvalidArrayCall);
                                    }
                                }
                            } else {
                                current_type = Some(Cow::Borrowed(context.get_type_of_variable(var_name)?));
                            }
                        }
                        Expression::Path(left_expr, right_expr) => {
                            stack.push(right_expr);
                            stack.push(left_expr);
                        }
                        Expression::FunctionCall(ref func_path, func_name, func_args) => {
                println!("Resolving FunctionCall in ArrayCall");

                            // Store the return type in a longer-lived variable
                            let function = self.get_function(*func_name)?;
                            let function_type = function.return_type();

                            match function_type {
                                Some(ref v) => match v {
                                    Type::T => {
                                        // Handle Type::T logic here
                                    if let Some(t) = current_type.as_ref() {
                        current_type = Some(t.clone()); // Directly use t.clone() without wrapping in Cow::Owned
                    } else {
                        return Err(ParserError::InvalidTypeT);
                    }

                        }
                        Type::Optional(inner) => {
                            // Handle Optional logic
                            current_type = Some(Cow::Owned(Type::Optional(inner.clone())));
                        }
                        _ => {
                            current_type = Some(Cow::Owned(v.clone()));
                        }
                    },
                    None => return Err(ParserError::FunctionNoReturnType),
                }
            }

            Expression::ArrayConstructor(_) => {
                return Err(ParserError::InvalidArrayCall);
            }
            _ => {
                println!("Unsupported expression encountered: {:?}", current_expr);
                return Err(ParserError::InvalidArrayCall);
            }
        }
    }

    if let Some(resolved) = current_type {
        match resolved.into_owned() {
            Type::Array(inner_type) => {
                current_type = Some(Cow::Owned(*inner_type));
            }
            _ => return Err(ParserError::InvalidArrayCall),
        }
    } else {
        return Err(ParserError::InvalidArrayCall);
    }

    return Ok(current_type)
}

            Expression::SubExpression(expr) => {
                println!("Resolved type:7 "); // Logging for debugging
             
                stack.push((expr, current_on_type));
                   println!("currennttyypeee {:?}", stack); 
            }
            Expression::StructConstructor(_, struct_type) => {
                println!("Resolved type: 1"); // Logging for debugging
                println!("heloo {:?}", struct_type); 
                current_type = Some(Cow::Owned(Type::Struct(struct_type.clone())));
               
               
            }
            Expression::Path(left, right) => {
            let mut stack: Vec<&Expression> = Vec::new();
            let mut current_type: Option<Cow<'b, Type>> = on_type.map(Cow::Borrowed);

            // Push the initial expressions onto the stack
            stack.push(right);
            stack.push(left);

            // Process the stack iteratively
            while let Some(current_expr) = stack.pop() {
                match current_expr {
                    Expression::Variable(var_name) => {
                        if let Some(_type) = current_type.as_ref() {
                            match _type.as_ref() {
                                Type::Struct(struct_type) => {
                                    let field_index = *var_name as usize; // Assuming `var_name` is indexable
                                    if let Some(field_type) = struct_type.fields().get(field_index) {
                                        current_type = Some(Cow::Owned(field_type.clone()));
                                    } else {
                                        return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()));
                                    }
                                },
                                _ => {
                                    todo!("Handle unexpected variable type");
                                }
                            }
                        } else {
                            current_type = Some(Cow::Borrowed(context.get_type_of_variable(var_name)?));
                        }
                    }
                    Expression::Path(left_expr, right_expr) => {
                        // Push right first, then left for correct order processing
                        stack.push(right_expr);
                        stack.push(left_expr);
                    }
                    _ => {
                return Err(ParserError::NotImplemented);
            }
        }
    }

    // Ensure we return a resolved type at the end
    return Ok(current_type)
        },
            Expression::Operator(op, left, right) =>{
      
            
             match op {
                Operator::Or
                | Operator::Equals
                | Operator::NotEquals
                | Operator::GreaterOrEqual
                | Operator::GreaterThan
                | Operator::LessOrEqual
                | Operator::LessThan
                | Operator::And => {
            
                    
                    current_type = Some(Cow::Owned(Type::Bool));
                }
                
                Operator::Plus | Operator::Minus => {
                    stack.push((left, current_on_type));
                    stack.push((right, current_on_type));
                     println!("currennttyypeee stackk {:?}", stack); 
                    if let Some(left_type) = current_type.take() {
                        if left_type == Cow::Owned(Type::String) {
                            current_type = Some(Cow::Owned(Type::String));
                            println!("current_type {:?}", current_type);
                         
                        }
                    }
                }
                Operator::Multiply
                | Operator::Divide
                | Operator::BitwiseXor
                | Operator::BitwiseAnd
                | Operator::BitwiseOr
                | Operator::BitwiseLeft
                | Operator::BitwiseRight
                | Operator::Rem => {
                    stack.push((left, current_on_type));
                    stack.push((right, current_on_type));
                }
                _ => {
                    return Err(ParserError::AssignReturnNothing);
                }}
        },
            Expression::Value(val) => match Type::from_value(val) {
                Some(v) => {      
                println!("Resolved type: 3 "); // Logging for debugging
                current_type = Some(Cow::Owned(v))},
                None => return Ok(None),
        },
            Expression::Ternary(_, expr, _) => {
                println!("Resolved type: 4"); // Logging for debugging
                stack.push((expr, current_on_type));
        },
            Expression::Cast(_, target_type) => {
                println!("Resolved type: 5"); // Logging for debugging
                current_type = Some(Cow::Borrowed(target_type));
        },
            Expression::Range(start, _) => {
                println!("Resolved type: 6"); // Logging for debugging
                stack.push((start, current_on_type));
                if let Some(Type::Range(inner)) = current_type.take().map(|cow| cow.into_owned()) {
                    current_type = Some(Cow::Owned(*inner));
                }
            }
            Expression::IsNot(_) => {
                current_type = Some(Cow::Owned(Type::Bool));
        }
        }
    }
      println!("heloower {:?}", current_type.clone().unwrap());
    
    // Ok(current_type);
   
    Ok(Some(current_type.clone().unwrap()))
}
 
    // Read a function call with the following syntax:
    // function_name(param1, param2, ...)
    fn read_function_call(&mut self, path: Option<Expression>, on_type: Option<&Type>, name: &str, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        // we remove the token from the list
        self.expect_token(Token::ParenthesisOpen)?;
        let mut parameters: Vec<Expression> = Vec::new();
        let mut types: Vec<Type> = Vec::new();

        // read parameters for function call
        while self.peek_is_not(Token::ParenthesisClose) {
            let expr = self.read_expression(context)?;
            // We are forced to clone the type because we can't borrow it from the expression
            // I prefer to do this than doing an iteration below
            let t = self.get_type_from_expression(None, &expr, context)?.into_owned();
            types.push(t);
            parameters.push(expr);

            if self.peek_is(Token::Comma) {
                self.expect_token(Token::Comma)?;
            }
        }

        let id = self.functions_mapper.get_compatible(Signature::new(name.to_owned(), on_type.cloned(), types), &mut parameters)?;

        // Entry are only callable by external
        let f = self.get_function(id)?;
        if f.is_entry() {
            return Err(ParserError::FunctionNotFound)
        }

        self.expect_token(Token::ParenthesisClose)?;
        Ok(Expression::FunctionCall(path.map(Box::new), id, parameters))
    }

    // Read a struct constructor with the following syntax:
    // struct_name { field_name: value1, field2: value2 }
    // If we have a field that has the same name as a variable we can pass it as following:
    // Example: struct_name { field_name, field2: value2 }
    fn read_struct_constructor(&mut self, on_type: Option<&Type>, struct_type: StructType, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        self.expect_token(Token::BraceOpen)?;
        
        let mut fields = Vec::with_capacity(struct_type.fields().len());
        for t in struct_type.fields() {
            let field_name = self.next_identifier()?;
            let field_value = match self.advance()? {
                Token::Comma => {
                    let id = context.get_variable_id(field_name)
                        .ok_or_else(|| ParserError::UnexpectedVariable(field_name.to_owned()))?;
                    Expression::Variable(id)
                }
                Token::Colon => {
                    let value = self.read_expr(on_type, true, true, Some(t), context)?;
                    if self.peek_is(Token::Comma) {
                        self.advance()?;
                    }
                    value
                }
                token => return Err(ParserError::UnexpectedToken(token))
            };

            let field_type = self.get_type_from_expression(on_type, &field_value, context)?;
            if !t.is_compatible_with(&field_type) {

                return Err(ParserError::InvalidValueType(field_type.into_owned(), t.clone()))
            }

            fields.push(field_value);
        }

        self.expect_token(Token::BraceClose)?;
        Ok(Expression::StructConstructor(fields, struct_type))
    }

    // Read a constant from the environment
    fn read_type_constant(&mut self, token: Token<'a>) -> Result<Expression, ParserError<'a>> {
        let _type = self.get_type_from_token(token)?;
        self.expect_token(Token::Colon)?;
        self.expect_token(Token::Colon)?;

        let constant_name = self.next_identifier()?;

        self.environment.get_constant_by_name(&_type, &constant_name)
            .map(|v| Expression::Value(v.clone()))
            .ok_or_else(|| ParserError::ConstantNotFound(_type, constant_name))
    }

    // Read an expression with default parameters
    fn read_expression(&mut self, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        self.read_expr(None, true, true, None, context)
    }

    // Read an expression with the possibility to accept operators
    // number_type is used to force the type of a number
    fn read_expr(&mut self, on_type: Option<&Type>, allow_ternary: bool, accept_operator: bool, expected_type: Option<&Type>, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        let mut required_operator = false;
        let mut last_expression: Option<Expression> = None;
        while self.peek()
            .ok()
            .filter(|peek| {
                if !allow_ternary && **peek == Token::OperatorTernary {
                    return false
                }

                if !accept_operator && required_operator {
                    return false
                }

                !peek.should_stop()
                && (
                    required_operator == peek.is_operator()
                    || (**peek == Token::BracketOpen && last_expression.is_none())
                )
            }).is_some()
        {

            let token = self.advance()?;
            let expr: Expression = match token {
                Token::BracketOpen => {
                    match last_expression {
                        Some(v) => {
                            if !self.get_type_from_expression(on_type, &v, context)?.is_array() {
                                return Err(ParserError::InvalidArrayCall)
                            }

                            // Index must be of type u64
                            let index = self.read_expr(on_type, true, true, Some(&Type::U32), context)?;
                            let index_type = self.get_type_from_expression(on_type, &index, context)?;
                            if *index_type != Type::U32 {
                                return Err(ParserError::InvalidArrayCallIndexType(index_type.into_owned()))
                            }

                            self.expect_token(Token::BracketClose)?;
                            required_operator = !required_operator;
                            Expression::ArrayCall(Box::new(v), Box::new(index))
                        },
                        None => { // require at least one value in a array constructor
                            let mut expressions: Vec<Expression> = Vec::new();
                            let mut array_type: Option<Type> = None;
                            while self.peek_is_not(Token::BracketClose) {
                                let expr = self.read_expr(on_type, true, true, expected_type.map(|t| t.get_inner_type()), context)?;
                                match &array_type { // array values must have the same type
                                    Some(t) => {
                                        let _type = self.get_type_from_expression(on_type, &expr, context)?;
                                        if *_type != *t {
                                            return Err(ParserError::InvalidTypeInArray(_type.into_owned(), t.clone()))
                                        }
                                    },
                                    None => { // first value rules the type of array
                                        array_type = Some(self.get_type_from_expression(on_type, &expr, context)?.into_owned());
                                    }
                                };
                                expressions.push(expr);

                                if self.peek_is(Token::Comma) {
                                    self.expect_token(Token::Comma)?;
                                }
                            }

                            self.expect_token(Token::BracketClose)?;
                            Expression::ArrayConstructor(expressions)
                        }
                    }
                },
                Token::ParenthesisOpen => {
                    let expr = self.read_expr(None, true, true, expected_type, context)?;
                    self.expect_token(Token::ParenthesisClose)?;
                    Expression::SubExpression(Box::new(expr))
                },
                Token::Identifier(id) => {
                    match self.peek()? {
                        // function call
                        Token::ParenthesisOpen => self.read_function_call(last_expression.take(), on_type, id, context)?,
                        Token::Colon => self.read_type_constant(Token::Identifier(id))?,
                        _ => {
                            match on_type {
                                // mostly an access to a struct field
                                Some(t) => {
                                    if let Type::Struct(_type) = t {
                                        let builder = self.struct_manager.get_by_ref(_type)?;
                                        match builder.get_id_for_field(id) {
                                            Some(v) => Expression::Variable(v),
                                            None => return Err(ParserError::UnexpectedVariable(id.to_owned()))
                                        }
                                    } else {
                                        return Err(ParserError::UnexpectedType(t.clone()))
                                    }
                                },
                                None => {
                                    if let Some(id) = context.get_variable_id(id) {
                                        Expression::Variable(id)
                                    } else if let Ok(id) = self.struct_manager.get_by_name(&id) {
                                        self.read_struct_constructor(on_type, id.inner().clone(), context)?
                                    } else {
                                        return Err(ParserError::UnexpectedVariable(id.to_owned()))
                                    }
                                }
                            }
                        }
                    }
                },
                Token::Value(value) => {
                    Expression::Value(match value {
                        Literal::U8(n) => Value::U8(n),
                        Literal::U16(n) => Value::U16(n),
                        Literal::U32(n) => Value::U32(n),
                        Literal::U64(n) => Value::U64(n),
                        Literal::U128(n) => Value::U128(n),
                        Literal::U256(n) => Value::U256(n),
                        Literal::Number(n) => match expected_type {
                            Some(Type::U8) => Value::U8(n as u8),
                            Some(Type::U16) => Value::U16(n as u16),
                            Some(Type::U32) => Value::U32(n as u32),
                            Some(Type::U64) => Value::U64(n as u64),
                            Some(Type::U128) => Value::U128(n as u128),
                            Some(Type::U256) => Value::U256(U256::from(n)),
                            _ => Value::U64(n)
                        },
                        Literal::String(s) => Value::String(s.into_owned()),
                        Literal::Bool(b) => Value::Boolean(b),
                        Literal::Null => Value::Null
                    })
                },
                Token::Dot => {
                    match last_expression {
                        Some(value) => {
                            let _type = self.get_type_from_expression(on_type, &value, context)?.into_owned();
                            // If we have .. that is mostly a range

                            // because we read operator DOT + right expression
                            required_operator = !required_operator;

                            if self.peek_is(Token::Dot) {
                                self.expect_token(Token::Dot)?;
                                let end_expr = self.read_expr(Some(&_type), false, false, expected_type, context)?;
                                let end_type = self.get_type_from_expression(on_type, &end_expr, context)?;
                                if _type != *end_type {
                                    return Err(ParserError::InvalidRangeType(_type, end_type.into_owned()))
                                }

                                if !_type.is_primitive() {
                                    return Err(ParserError::InvalidRangeTypePrimitive(_type))
                                }

                                Expression::Range(Box::new(value), Box::new(end_expr))
                            } else {
                                let right_expr = self.read_expr(Some(&_type), false, false, expected_type, context)?;

                                if let Expression::FunctionCall(path, name, params) = right_expr {
                                    if path.is_some() {
                                        return Err(ParserError::UnexpectedPathInFunctionCall)
                                    }
                                    Expression::FunctionCall(Some(Box::new(value)), name, params)
                                } else {
                                    Expression::Path(Box::new(value), Box::new(right_expr))
                                }
                            }
                        },
                        None => return Err(ParserError::UnexpectedToken(Token::Dot))
                    }
                },
                Token::IsNot => { // it's an operator, but not declared as
                    let expr = self.read_expression(context)?;
                    let expr_type = self.get_type_from_expression(on_type, &expr, context)?;
                    if *expr_type != Type::Bool {
                        
                        return Err(ParserError::InvalidValueType(expr_type.into_owned(), Type::Bool))
                    }

                    Expression::IsNot(Box::new(expr))
                },
                Token::OperatorTernary => match last_expression { // condition ? expr : expr
                    Some(expr) => {
                        if *self.get_type_from_expression(on_type, &expr, context)? != Type::Bool {
                            return Err(ParserError::InvalidCondition(Type::Bool, expr))
                        }

                        let valid_expr = self.read_expr(on_type, true, true, expected_type, context)?;
                        let first_type = self.get_type_from_expression(on_type, &valid_expr, context)?.into_owned();
                        self.expect_token(Token::Colon)?;
                        let else_expr = self.read_expr(on_type, true, true, expected_type, context)?;
                        let else_type = self.get_type_from_expression(on_type, &else_expr, context)?;
                        
                        if first_type != *else_type { // both expr should have the SAME type.
                            return Err(ParserError::InvalidValueType(else_type.into_owned(), first_type))
                        }
                        required_operator = !required_operator;
                        Expression::Ternary(Box::new(expr), Box::new(valid_expr), Box::new(else_expr))
                    },
                    None => return Err(ParserError::InvalidTernaryNoPreviousExpression)
                },
                Token::As => {
                    let previous_expr = last_expression.ok_or_else(|| ParserError::InvalidOperation)?;
                    let left_type = self.get_type_from_expression(on_type, &previous_expr, context)?.into_owned();
                    let right_type = self.read_type()?;

                    if !left_type.is_castable_to(&right_type) {
                        return Err(ParserError::CastError(left_type, right_type))
                    }

                    if !right_type.is_primitive() {
                        return Err(ParserError::CastPrimitiveError(left_type, right_type))
                    }

                    required_operator = !required_operator;
                    Expression::Cast(Box::new(previous_expr), right_type)
                },
                token => {
                    match last_expression {
                        Some(mut previous_expr) => {
                            required_operator = !required_operator;

                            let left_type = self.get_type_from_expression(on_type, &previous_expr, context)?.into_owned();
                            // Parse the operator for this token
                            let op = match Operator::value_of(&token) {
                                Some(op) => op,
                                None => return Err(ParserError::OperatorNotFound(token))
                            };

                            let mut expr = self.read_expr(on_type, false, true, Some(&left_type), context)?;
                            if let Some(right_type) = self.get_type_from_expression_internal(on_type, &expr, context)? {
                                match &op {
                                    Operator::Minus | Operator::Rem | Operator::Divide | Operator::Multiply
                                    | Operator::Assign(_) | Operator::BitwiseLeft | Operator::BitwiseRight
                                    | Operator::GreaterThan | Operator::LessThan | Operator::LessOrEqual
                                    | Operator::GreaterOrEqual => {
                                        if left_type != *right_type {
                                            // It is an hardcoded value, lets map it to the correct type
                                            if let Expression::Value(value) = previous_expr {
                                                previous_expr = Expression::Value(value.checked_cast_to_primitive_type(&right_type)?);
                                            } else if let Expression::Value(value) = expr {
                                                expr = Expression::Value(value.checked_cast_to_primitive_type(&left_type)?);
                                            } else {
                                                return Err(ParserError::InvalidOperationNotSameType(left_type, right_type.into_owned()))
                                            }
                                        }
                                    },
                                    Operator::Plus => {
                                        if left_type != *right_type && !(left_type == Type::String || *right_type == Type::String) {
                                            return Err(ParserError::InvalidOperationNotSameType(left_type, right_type.into_owned()))
                                        }
                                    },
                                    Operator::And | Operator::Or => {
                                        if left_type != Type::Bool {
                                            return Err(ParserError::InvalidOperationNotSameType(left_type, Type::Bool))
                                        }

                                        if *right_type != Type::Bool {
                                            return Err(ParserError::InvalidOperationNotSameType(right_type.into_owned(), Type::Bool))
                                        }
                                    },
                                    _ => if left_type != *right_type {
                                        return Err(ParserError::InvalidOperationNotSameType(left_type, right_type.into_owned()))
                                    }
                                };

                                Expression::Operator(op, Box::new(previous_expr), Box::new(expr))
                            } else {
                                match op {
                                    Operator::Equals | Operator::NotEquals |
                                    Operator::Assign(None) if left_type.allow_null() => Expression::Operator(op, Box::new(previous_expr), Box::new(expr)),
                                    _ => return Err(ParserError::IncompatibleNullWith(left_type))
                                }
                            }
                        }
                        None => {
                            if token.is_type() {
                                self.read_type_constant(token)?
                            } else {
                                return Err(ParserError::InvalidOperation)
                            }
                        }
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
    fn read_body(&mut self, context: &mut Context<'a>, return_type: &Option<Type>) -> Result<Vec<Statement>, ParserError<'a>> {
        context.begin_scope();
        let statements = self.read_statements(context, return_type)?;
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
    fn read_variable(&mut self, context: &mut Context<'a>, is_const: bool) -> Result<DeclarationStatement, ParserError<'a>> {
        let name: &'a str = self.next_identifier()?;

        // Constants must be uppercase
        if is_const && name.to_uppercase() != name {
            return Err(ParserError::ConstantNameNotUppercase(name.to_owned()))
        }

        let ignored = name == "_";

        // Variable name must start with a alphabetic character
        if !ignored && !name.starts_with(char::is_alphabetic) {
            return Err(ParserError::VariableMustStartWithAlphabetic(name.to_owned()))
        }

        self.expect_token(Token::Colon)?;
        let value_type = self.read_type()?;
        let value: Expression = if self.peek_is(Token::OperatorAssign) {
            self.expect_token(Token::OperatorAssign)?;
            let expr = self.read_expr(None, true, true, Some(value_type.get_inner_type()), context)?;

            let expr_type = match self.get_type_from_expression_internal(None, &expr, context) {
                Ok(opt_type) => match opt_type {
                    Some(v) => v,
                    None => if value_type.is_optional() {
                        Cow::Owned(value_type.clone())
                    } else {
                        return Err(ParserError::NoValueType)
                    }
                },
                Err(e) => match e { // support empty array declaration
                    ParserError::EmptyArrayConstructor if value_type.is_array() => Cow::Owned(value_type.clone()),
                    _ => return Err(e)
                }
            };

            if !expr_type.is_compatible_with(&value_type) {
              
                return Err(ParserError::InvalidValueType(expr_type.into_owned(), value_type))
            }

            expr
        } else {
            Expression::Value(Value::Null)
        };

        let id = if ignored {
            context.register_variable_unchecked(name, value_type.clone())
        } else {
            context.register_variable(name, value_type.clone())?
        };

        Ok(DeclarationStatement {
            id,
            value_type,
            value
        })
    }

    fn read_loop_body(&mut self, context: &mut Context<'a>, return_type: &Option<Type>) -> Result<Vec<Statement>, ParserError<'a>> {
        // support nested loop
        let old_value = context.is_in_a_loop();
        context.set_in_a_loop(true);
        self.expect_token(Token::BraceOpen)?;
        let statements = self.read_body(context, return_type)?;
        context.set_in_a_loop(old_value);

        Ok(statements)
    }

    // Read a single statement
    fn read_statement(&mut self, context: &mut Context<'a>, return_type: &Option<Type>) -> Result<Option<Statement>, ParserError<'a>> {
        if let Some(token) = self.next() {
            let statement: Statement = match token {
                Token::BraceClose => return Ok(None),
                Token::For => { // Example: for i: u64 = 0; i < 10; i += 1 {}
                    context.begin_scope();
                    let var = self.read_variable(context, false)?;
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(condition_type.into_owned(), condition))
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
                    let variable = self.next_identifier()?;
                    self.expect_token(Token::In)?;
                    let expr = self.read_expression(context)?;
                    let expr_type = self.get_type_from_expression(None, &expr, context)?;

                    // verify that we can iter on it
                    if !expr_type.is_iterable() {
                        return Err(ParserError::NotIterable(expr_type.into_owned()))
                    }

                    let id = context.register_variable(variable, expr_type.get_inner_type().clone())?;
                    let statements = self.read_loop_body(context, return_type)?;
                    context.end_scope();

                    Statement::ForEach(id, expr, statements)
                },
                Token::While => { // Example: while i < 10 {}
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(condition_type.into_owned(), condition))
                    }

                    let statements = self.read_loop_body(context, return_type)?;

                    Statement::While(condition, statements)
                },
                Token::If => {
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(condition_type.into_owned(), condition))
                    }

                    self.expect_token(Token::BraceOpen)?;
                    let body = self.read_body(context, return_type)?;
                    let else_statement = if self.peek_is(Token::Else) {
                        self.advance()?;
                        Some(if self.peek_is(Token::If) {
                            let statement = self.read_statement(context, return_type)?;
                            vec![statement.ok_or(ParserError::UnexpectedToken(Token::If))?]
                        } else {
                            self.expect_token(Token::BraceOpen)?;
                            self.read_body(context, return_type)?
                        })
                    } else {
                        None
                    };

                    Statement::If(condition, body, else_statement)
                },
                Token::BraceOpen => Statement::Scope(self.read_body(context, return_type)?),
                Token::Let => Statement::Variable(self.read_variable(context, false)?),
                Token::Return => {
                    // Optionally resolve the return expression if a return type is provided
                    let opt: Option<Expression> = if let Some(return_type) = return_type {
                        // Read the return expression
                        let expr = self.read_expr(None, true, true, Some(return_type), context)?;

                        // Determine the type of the expression
                        let expr_type = self.get_type_from_expression(None, &expr, context)?;

                        // Check if the expression's type matches the expected return type
                        if !expr_type.is_compatible_with(return_type) {
                            println!("Resolved Expression: {:?}", expr);
                            println!("Resolved Expression Type: {:?}", expr_type);
                            println!("Expected Return Type: {:?}", return_type);

                            // Return a descriptive error when types are incompatible
                            return Err(ParserError::InvalidValueType(
                                expr_type.into_owned(),
                                return_type.clone(),
                            ));
                        }

                        // If the types are compatible, include the expression in the return statement
                        Some(expr)
                    } else {
                        // If no return type is defined, allow an empty return
                        None
                    };

                    // Ensure no dead code follows the return statement
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    // Return a properly formed return statement
                    Statement::Return(opt)
                }
                Token::Continue => {
                    if !context.is_in_a_loop() {
                        return Err(ParserError::UnexpectedToken(Token::Continue));
                    }

                    // we can't have anything after a continue
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Continue
                },
                Token::Break => {
                    if !context.is_in_a_loop() {
                        return Err(ParserError::UnexpectedToken(Token::Break));
                    }

                    // we can't have anything after a break
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Break
                },
                token => {
                    self.tokens.push_front(token);
                    Statement::Expression(self.read_expression(context)?)
                }
            };
            Ok(Some(statement))
        } else {
            Ok(None)
        }

    }
    // Read all statements in a block
    // return type is used to verify that the last statement is a return with a valid value type
    // consume_brace is used to know if we should consume the open brace
    fn read_statements(&mut self, context: &mut Context<'a>, return_type: &Option<Type>) -> Result<Vec<Statement>, ParserError<'a>> {
        let mut statements: Vec<Statement> = Vec::new();
        while let Some(statement) = self.read_statement(context, return_type)? {
            statements.push(statement);
        }

        Ok(statements)
    }

    // Read the parameters for a function
    fn read_parameters(&mut self) -> Result<Vec<(&'a str, Type)>, ParserError<'a>> {
        let mut parameters = Vec::new();
        while self.peek_is_identifier() {
            let name = self.next_identifier()?;
            self.expect_token(Token::Colon)?;
            let value_type = self.read_type()?;

            parameters.push((name, value_type));

            if self.peek_is_not(Token::Comma) {
                break;
            }

            self.expect_token(Token::Comma)?;
        }

        Ok(parameters)
    }

    // Verify that the last statement is a return
    // We don't check the last statement directly has it would allow
    // to have dead code after a return
    fn ends_with_return(statements: &Vec<Statement>) -> Result<bool, ParserError<'a>> {
        let mut ok = false;
        if let Some(statement) = statements.last() {
            match statement {
                Statement::If(_, statements, else_statements) => {
                    // if its the last statement
                    ok = Self::ends_with_return(statements)?;
                    // if it ends with a return, else must also end with a return
                    if let Some(statements) = else_statements.as_ref().filter(|_| ok) {
                        ok = Self::ends_with_return(statements)?;
                    } else {
                        ok = false;
                    }
                }
                Statement::Return(Some(_)) => {
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
     * - fn foo() { ... }
     * - fn foo() -> u64 { ... }
     * - fn foo(a: u64, b: u64) { ... }
     * - fn (f Foo) bar() { ... }
     * Rules:
     * - Signature is based on function name, and parameters
     * - Entry function is a "public callable" function and must return a u64 value
     */
    fn read_function(&mut self, entry: bool, context: &mut Context<'a>) -> Result<(), ParserError<'a>> {
        context.begin_scope();

        let token = self.advance()?;
        let (instance_name, for_type, name) = if !entry && token == Token::ParenthesisOpen {
            let instance_name = self.next_identifier()?;
            let for_type = self.read_type()?;
            let id = context.register_variable(instance_name, for_type.clone())?;
            self.expect_token(Token::ParenthesisClose)?;

            (Some(id), Some(for_type), self.next_identifier()?)
        } else {
            let Token::Identifier(name) = token else {
                return Err(ParserError::ExpectedIdentifierToken(token))
            };
            (None, None, name)
        };

        self.expect_token(Token::ParenthesisOpen)?;
        let parameters = self.read_parameters()?;
        self.expect_token(Token::ParenthesisClose)?;

        // all entries must return a u64 value without being specified
        let return_type: Option<Type> = if entry {
            // an entrypoint cannot be a method
            if for_type.is_some() {
                return Err(ParserError::EntryFunctionCannotHaveForType)
            }

            Some(Type::U64)
        } else if self.peek_is(Token::ReturnType) { // read returned type
            self.advance()?;
            Some(self.read_type()?)
        } else {
            None
        };

        let types: Vec<Type> = parameters.iter().map(|p| p.1.clone()).collect();
        let id = self.functions_mapper.register(Signature::new(name.to_owned(), for_type.clone(), types))?;
        if self.has_function(id) {
            return Err(ParserError::FunctionSignatureAlreadyExist) 
        }

        let has_return_type = return_type.is_some();

        let mut new_params = Vec::with_capacity(parameters.len());
        for (name, param_type) in parameters {
            let id = context.register_variable(name, param_type.clone())?;
            new_params.push(Parameter::new(id, param_type));
        }

        self.expect_token(Token::BraceOpen)?;
        let statements = self.read_body(context, &return_type)?;

        context.end_scope();

        // verify that the function ends with a return
        if has_return_type && !Self::ends_with_return(&statements)? {
            return Err(ParserError::NoReturnFound)
        }

        let function = match entry {
            true => FunctionType::Entry(EntryFunction::new(new_params, statements, context.max_variables_count() as u16)),
            false => FunctionType::Declared(DeclaredFunction::new(
                for_type,
                instance_name,
                new_params,
                statements,
                return_type,
                context.max_variables_count() as u16
            ))
        };

        // push function before reading statements to allow recursive calls
        self.functions.push(function);

        Ok(())
    }

    // Read a type with the following syntax:
    // import "filename.xel";
    // or with an alias:
    // import "filename.xel" as alias;
    fn read_import(&mut self) -> Result<(), ParserError<'a>> {
        let path = self.advance()?;

        let Token::Value(Literal::String(path)) = path else {
            return Err(ParserError::InvalidImport)
        };

        // We don't allow absolute path or path that contains ".."
        if path.starts_with("/") || path.contains("..") {
            return Err(ParserError::InvalidImportPath(path.into_owned()))
        }

        // If its a local import, we will import its content directly
        let is_local = path.ends_with(".xel");
        if !is_local {
            return Err(ParserError::NotImplemented)
        }

        Ok(())
    }

    // check if a function with the same signature exists
    fn has_function(&self, id: u16) -> bool {
        self.get_function(id).is_ok()
    }

    // get a function using its identifier
    fn get_function<'b>(&'b self, id: u16) -> Result<Function<'b>, ParserError<'a>> {
        // the id is the index of the function in the functions array
        let index = id as usize;
        let len = self.environment.get_functions().len();
        if index < len {
            Ok(Function::Native(&self.environment.get_functions()[index]))
        } else {
            match self.functions.get(index - len) {
                Some(func) => Ok(Function::Program(func)),
                None => Err(ParserError::FunctionNotFound)
            }
        }
    }

    /**
     * Example: Message { message_id: u64, message: string }
     * Rules:
     * - Structure name should start with a uppercase character
     * - only alphanumeric chars in name
     */
    fn read_struct(&mut self) -> Result<(), ParserError<'a>> {
        let name = self.next_identifier()?;
        let mut chars = name.chars();
        if !chars.all(|c| c.is_ascii_alphanumeric()) {
            return Err(ParserError::InvalidStructureName(name.to_owned()))
        }

        // check if the first letter is in uppercase
        match name.chars().nth(0) {
            Some(v) => {
                if !v.is_ascii_alphabetic() || !v.is_uppercase() {
                    return Err(ParserError::InvalidStructureName(name.to_owned()))
                }
            },
            None => return Err(ParserError::EmptyStructName)
        };

        self.expect_token(Token::BraceOpen)?;
        let params = self.read_parameters()?;
        let mut fields = Vec::with_capacity(params.len());
        for (name, param_type) in params {
            fields.push((name, param_type));
        }

        self.expect_token(Token::BraceClose)?;

        self.struct_manager.add(Cow::Borrowed(name), fields)?;

        Ok(())
    }

    // Parse the tokens and return a Program
    // The function mapper is also returned for external calls
    pub fn parse(mut self) -> Result<(Program, FunctionMapper<'a>), ParserError<'a>> {
        let mut context: Context = Context::new();
        while let Some(token) = self.next() {
            match token {
                Token::Import => {
                    self.read_import()?;
                    continue;
                }
                Token::Const => {
                    let var = self.read_variable(&mut context, true)?;
                    let id = var.id;
                    if !self.constants.insert(var) {
                        return Err(ParserError::VariableIdAlreadyUsed(id))
                    }
                },
                Token::Function => self.read_function(false, &mut context)?,
                Token::Entry => self.read_function(true, &mut context)?,
                Token::Struct => self.read_struct()?,
                token => return Err(ParserError::UnexpectedToken(token))
            };
        }

        let program = Program::with(self.constants, self.struct_manager.finalize(), self.functions);
        Ok((program, self.functions_mapper))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn test_parser(tokens: Vec<Token>) -> Program {
        let env = EnvironmentBuilder::default();
        test_parser_with_env(tokens, &env)
    }

    #[track_caller]
    fn test_parser_with_env(tokens: Vec<Token>, env: &EnvironmentBuilder) -> Program {
        let parser = Parser::new(VecDeque::from(tokens), env);
        let (program, _) = parser.parse().unwrap();
        program
    }

    #[track_caller]
    fn test_parser_statement(tokens: Vec<Token>, variables: Vec<(&str, Type)>) -> Vec<Statement> {
        let env = EnvironmentBuilder::new();
        test_parser_statement_with(tokens, variables, &None, env)
    }

    #[track_caller]
    fn test_parser_statement_with(tokens: Vec<Token>, variables: Vec<(&str, Type)>, return_type: &Option<Type>, env: EnvironmentBuilder) -> Vec<Statement> {
        let mut parser = Parser::new(VecDeque::from(tokens), &env);
        let mut context = Context::new();
        context.begin_scope();
        for (name, t) in variables {
            context.register_variable(name, t).unwrap();
        }

        parser.read_statements(&mut context, return_type).unwrap()
    }

    fn test_parser_statement_with_return_type(tokens: Vec<Token>, variables: Vec<(&str, Type)>, return_type: Type) -> Vec<Statement> {
        let env = EnvironmentBuilder::new();
        test_parser_statement_with(tokens, variables, &Some(return_type), env)
    }

    #[test]
    fn test_range() {
        let tokens = vec![
            Token::Let,
            Token::Identifier("a"),
            Token::Colon,

            Token::Range(Box::new(Token::Number(NumberType::U64))),
            Token::OperatorAssign,

            Token::Value(Literal::U64(0)),
            Token::Dot,
            Token::Dot,
            Token::Value(Literal::U64(10)),
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(
            statements[0],
            Statement::Variable(
                DeclarationStatement {
                    id: 0,
                    value_type: Type::U64,
                    value: Expression::Range(
                        Box::new(Expression::Value(Value::U64(0))),
                        Box::new(Expression::Value(Value::U64(10))),
                    )
                }
            )
        );
    }

    #[test]
    fn test_for_each_range() {
        let tokens = vec![
            Token::ForEach,
            Token::Identifier("a"),
            Token::In,
            Token::Value(Literal::U64(0)),
            Token::Dot,
            Token::Dot,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
        assert_eq!(
            statements[0],
            Statement::ForEach(
                0,
                Expression::Range(
                    Box::new(Expression::Value(Value::U64(0))),
                    Box::new(Expression::Value(Value::U64(10))),
                ),
                Vec::new()
            )
        );
    }

    #[test]
    fn test_function() {
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_call_in_expr() {
        /*
        function foo() -> bool {
            let array: u64[] = [1, 2, 3];
            return 0 > array.len()
        }
        */
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::ReturnType,
            Token::Bool,
            Token::BraceOpen,
            Token::Let,
            Token::Identifier("array"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::BracketOpen,
            Token::BracketClose,
            Token::OperatorAssign,
            Token::BracketOpen,
            Token::Value(Literal::U64(1)),
            Token::Comma,
            Token::Value(Literal::U64(2)),
            Token::Comma,
            Token::Value(Literal::U64(3)),
            Token::BracketClose,
            Token::Return,
            Token::Value(Literal::U64(0)),
            Token::OperatorGreaterThan,
            Token::Identifier("array"),
            Token::Dot,
            Token::Identifier("len"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        let statements = vec![
            Statement::Variable(
                DeclarationStatement {
                    id: 0,
                    value_type: Type::Array(Box::new(Type::U64)),
                    value: Expression::ArrayConstructor(vec![
                        Expression::Value(Value::U64(1)),
                        Expression::Value(Value::U64(2)),
                        Expression::Value(Value::U64(3))
                    ])
                }
            ),
            Statement::Return(Some(Expression::Operator(
                Operator::GreaterThan,
                Box::new(Expression::Value(Value::U32(0))),
                Box::new(Expression::FunctionCall(
                    Some(Box::new(Expression::Variable(0))),
                    0,
                    Vec::new()
                ))
            )))
        ];

        assert_eq!(
            *program.functions().get(0).unwrap(),
            FunctionType::Declared(
                DeclaredFunction::new(None, None, Vec::new(), statements, Some(Type::Bool), 1)
            )
        );
    }

    #[test]
    fn test_entry_function() {
        let tokens = vec![
            Token::Entry,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::Value(Literal::U64(0)),
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_on_type() {
        // fn (f Foo) bar() {}
        let tokens = vec![
            Token::Function,
            Token::ParenthesisOpen,
            Token::Identifier("f"),
            Token::Identifier("Foo"),
            Token::ParenthesisClose,
            Token::Identifier("bar"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::BraceClose
        ];

        let mut env = EnvironmentBuilder::new();
        env.register_structure("Foo", Vec::new());
        let program = test_parser_with_env(tokens, &env);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_with_parameters() {
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::Identifier("a"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::Comma,
            Token::Identifier("b"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_with_return_type() {
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::ReturnType,
            Token::Number(NumberType::U64),
            Token::BraceOpen,
            Token::Return,
            Token::Value(Literal::U64(0)),
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_foreach() {
        // foreach a in array {}
        let tokens = vec![
            Token::ForEach,
            Token::Identifier("a"),
            Token::In,
            Token::Identifier("array"),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(
            tokens,
            vec![
                ("array", Type::Array(Box::new(Type::U64)))
            ]
        );

        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_for() {
        // for i: u64 = 0; i < 10; i += 1 {}
        let tokens = vec![
            Token::For,
            Token::Identifier("i"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::OperatorAssign,
            Token::Value(Literal::U64(0)),
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::Identifier("i"),
            Token::OperatorPlusAssign,
            Token::Value(Literal::U64(1)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_while() {
        // while i < 10 {}
        let tokens = vec![
            Token::While,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if() {
        // if i < 10 {}
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if_else() {
        // if i < 10 {} else {}
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if_else_if() {
        // if i < 10 {} else if i < 20 {}
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(20)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if_return() {
        // if i < 10 { nothing } return 0
        let mut tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Return
        ];

        let statements = test_parser_statement(tokens.clone(), vec![("i", Type::U64)]);
        assert_eq!(statements, vec![
            Statement::If(
                Expression::Operator(
                    Operator::LessThan,
                    Box::new(Expression::Variable(0)),
                    Box::new(Expression::Value(Value::U64(10)))
                ),
                Vec::new(),
                None
            ),
            Statement::Return(None)
        ]);

        tokens.push(Token::Value(Literal::U64(0)));


        let statements = test_parser_statement_with_return_type(tokens, vec![("i", Type::U64)], Type::U64);
        assert_eq!(statements, vec![
            Statement::If(
                Expression::Operator(
                    Operator::LessThan,
                    Box::new(Expression::Variable(0)),
                    Box::new(Expression::Value(Value::U64(10)))
                ),
                Vec::new(),
                None
            ),
            Statement::Return(Some(Expression::Value(Value::U64(0))))
        ]);
    }

    #[test]
    fn test_if_else_if_else_return() {
        // if i < 10 {} else if i < 20 {} else {} return 0
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(20)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::BraceOpen,
            Token::BraceClose,
            Token::Return,
            Token::Value(Literal::U64(0))
        ];

        let statements = test_parser_statement_with_return_type(tokens, vec![("i", Type::U64)], Type::U64);
        assert_eq!(statements, vec![
            Statement::If(
                Expression::Operator(
                    Operator::LessThan,
                    Box::new(Expression::Variable(0)),
                    Box::new(Expression::Value(Value::U64(10)))
                ),
                Vec::new(),
                Some(vec![
                    Statement::If(
                        Expression::Operator(
                            Operator::LessThan,
                            Box::new(Expression::Variable(0)),
                            Box::new(Expression::Value(Value::U64(20)))
                        ),
                        Vec::new(),
                        Some(Vec::new())
                    )
                ])
            ),
            Statement::Return(Some(Expression::Value(Value::U64(0))))
        ]);
    }

    #[test]
    fn test_ends_with_return() {
        const RETURN: Statement = Statement::Return(Some(Expression::Value(Value::U64(0))));
        let statements = vec![RETURN];
        assert!(Parser::ends_with_return(&statements).unwrap());

        let statements = vec![RETURN, Statement::Expression(Expression::Value(Value::U64(0)))];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if ... return
        let statements = vec![Statement::If(Expression::Value(Value::Boolean(true)), Vec::new(), None), RETURN];
        assert!(Parser::ends_with_return(&statements).unwrap());

        let statements = vec![Statement::If(Expression::Value(Value::Boolean(true)), Vec::new(), None)];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if else
        let statements = vec![Statement::If(Expression::Value(Value::Boolean(true)), Vec::new(), Some(Vec::new()))];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if return else return
        let statements = vec![
            Statement::If(Expression::Value(Value::Boolean(true)), vec![RETURN], Some(vec![RETURN]))
        ];
        assert!(Parser::ends_with_return(&statements).unwrap());

        // if return else if return else no return
        let statements = vec![
            Statement::If(
                Expression::Value(Value::Boolean(true)),
                vec![RETURN],
                Some(vec![
                    Statement::If(Expression::Value(Value::Boolean(true)), vec![RETURN], None)
                ])
            )
        ];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if return else if return else return
        let statements = vec![
            Statement::If(
                Expression::Value(Value::Boolean(true)),
                vec![RETURN],
                Some(vec![
                    Statement::If(Expression::Value(Value::Boolean(true)), vec![RETURN], Some(vec![RETURN]))
                ])
            )
        ];
        assert!(Parser::ends_with_return(&statements).unwrap());
    }

    #[test]
    fn test_variable() {
        // let hello: string = "hello";
        let tokens = vec![
            Token::Let,
            Token::Identifier("hello"),
            Token::Colon,
            Token::String,
            Token::OperatorAssign,
            Token::Value(Literal::String(Cow::Borrowed("world"))),
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_ternary() {
        // i < 10 ? 1 : 0
        let tokens = vec![
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::OperatorTernary,
            Token::Value(Literal::U64(1)),
            Token::Colon,
            Token::Value(Literal::U64(0)),
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_struct() {
        // Message { message_id: u64, message: string }
        let tokens = vec![
            Token::Struct,
            Token::Identifier("Message"),
            Token::BraceOpen,
            Token::Identifier("aaa_message_id"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::Comma,
            Token::Identifier("aaa_message"),
            Token::Colon,
            Token::String,
            Token::BraceClose
        ];

        let program = test_parser(tokens.clone());
        assert_eq!(program.structures().len(), 1);

        // Also test with a environment
        let mut env = EnvironmentBuilder::new();
        env.register_structure("Message", vec![
            ("message_id", Type::U64),
            ("message", Type::String)
        ]);

        // Create a struct instance
        let tokens = vec![
            Token::Let,
            Token::Identifier("msg"),
            Token::Colon,
            Token::Identifier("Message"),
            Token::OperatorAssign,
            Token::Identifier("Message"),
            Token::BraceOpen,
            Token::Identifier("message_id"),
            Token::Colon,
            Token::Value(Literal::U64(0)),
            Token::Comma,
            Token::Identifier("message"),
            Token::Colon,
            Token::Value(Literal::String(Cow::Borrowed("hello"))),
            Token::BraceClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, env);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_type_constant() {
        // let test: u64 = u64::MAX;
        let tokens = vec![
            Token::Let,
            Token::Identifier("test"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::OperatorAssign,
            Token::Number(NumberType::U64),
            Token::Colon,
            Token::Colon,
            Token::Identifier("MAX"),
        ];

        let mut env = EnvironmentBuilder::new();
        env.register_constant(Type::U64, "MAX", Value::U64(u64::MAX));
        let statements = test_parser_statement_with(tokens, Vec::new(), &None, env);
        assert_eq!(statements.len(), 1);
    }
}