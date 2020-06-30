//! # `full-moon` <-[miniserde]-> `luaparse.js`
//!
//! This is a silly library I wrote to bridge the gap between some software I had
//! written previously that worked with the JSON output from [luaparse.js] and
//! the [full_moon] lua parsing library that I chose.
//!
//! It offers a range of wrapper types around the types from `full-moon` and
//! implements the `miniserde::Serialize` trait from [miniserde] on them. Use the
//! `Chunk::wrap(ast)` method on some `Ast` from `full-moon` to get started.
//!
//! You can then use `miniserde::json::to_string(x)` on any `x` that implements
//! `miniserde::Serialize`, such as the `Chunk` mentioned above.
//!
//! Note: This library includes some code that works around issues such as missing
//! operator precendence in `full-moon`. The upstream author is currently reworking
//! the parser so there was no point in trying to fork the current API.
//!
//! [luaparse.js]: https://oxyc.github.io/luaparse/
//! [full_moon]: https://crates.io/crates/full_moon/
//! [miniserde]: https://crates.io/crates/miniserde/
use {
    full_moon::{
        ast::{
            punctuated::Punctuated, Ast, BinOp, Block, Call, ElseIf, Expression, Field,
            FunctionArgs, FunctionBody, FunctionCall, FunctionName, If, Index, LastStmt, Parameter,
            Prefix, Stmt, Suffix, TableConstructor, TableConstructorField, UnOp, Value, Var,
            VarExpression,
        },
        tokenizer::{Symbol, TokenReference, TokenType},
    },
    miniserde::{
        ser::{Fragment, Map, Seq},
        Serialize,
    },
    std::{borrow::Cow, ops::Deref, str::FromStr},
};

pub mod stream;
use stream::{
    AssignmentStmtStream, BinaryExprStream, BodyStream, BooleanLiteralStream, BreakStatementStream,
    CallExpressionStream, CallStatementStream, ChunkStream, ElseClauseStream, ElseifClauseStream,
    ExprStream, ForGenericStatementStream, ForNumericStatementStream, FunctionDeclarationStream,
    FunctionParameterStream, IdentifierStream, IfClauseStream, IfClausesStream, IfStatementStream,
    LocalStatementStream, MemberExprStream, NameStream, NilLiteralStream, NumericLiteralStream,
    ReturnStatementStream, StringCallExpressionStream, StringLiteralStream,
    TableCallExpressionStream, TableConstructorFieldsStream, TableConstructorStream,
    TableKeyStream, TableKeyStringStream, TableValueStream, UnaryExprStream, VarIndexExprStream,
    VarStream, VarargLiteralStream, WhileStatementStream,
};

pub struct Chunk<'a, 'b>(&'b Ast<'a>);
impl<'a, 'b> Chunk<'a, 'b> {
    pub fn wrap(ast: &'b Ast<'a>) -> Self {
        Self(ast)
    }
}

impl<'a, 'b> Serialize for Chunk<'a, 'b> {
    fn begin(&self) -> Fragment {
        Fragment::Map(Box::new(ChunkStream {
            state: 0,
            body: Body(self.0.nodes()),
        }))
    }
}

pub struct Body<'a, 'b>(&'b Block<'a>);

impl<'a, 'b> Serialize for Body<'a, 'b> {
    fn begin(&self) -> Fragment {
        let stmts = self.0.iter_stmts();
        let last = self.0.last_stmt().map(LastStatement);
        Fragment::Seq(Box::new(BodyStream {
            state: 0,
            stmts,
            curr: None,
            last,
        }))
    }
}

pub struct Statement<'a, 'b>(&'b Stmt<'a>);
impl<'a, 'b> Serialize for Statement<'a, 'b> {
    fn begin(&self) -> Fragment {
        match &self.0 {
            Stmt::Assignment(a) => Fragment::Map(Box::new(AssignmentStmtStream {
                state: 0,
                variables: VarList(a.var_list()),
                init: ExprList(a.expr_list()),
            })),
            Stmt::LocalAssignment(a) => Fragment::Map(Box::new(LocalStatementStream {
                state: 0,
                variables: NameList(a.name_list()),
                init: ExprList(a.expr_list()),
            })),
            Stmt::FunctionCall(f) => Fragment::Map(Box::new(CallStatementStream {
                state: 0,
                expression: CallExpression(f),
            })),
            Stmt::FunctionDeclaration(f) => {
                let body = f.body();
                let name = f.name();
                Fragment::Map(Box::new(FunctionDeclarationStream {
                    state: 0,
                    is_local: false,
                    identifier: FunctionIdentifier::Name(name),
                    parameters: FunctionParameters(body),
                    body: Body(body.block()),
                }))
            }
            Stmt::If(i) => Fragment::Map(Box::new(IfStatementStream {
                state: 0,
                clauses: IfClauses(i),
            })),
            Stmt::NumericFor(nf) => Fragment::Map(Box::new(ForNumericStatementStream {
                state: 0,
                variable: Identifier(Cow::Borrowed(nf.index_variable())),
                start: Expr(nf.start()),
                end: Expr(nf.end()),
                step: nf.step().map(Expr),
                body: Body(nf.block()),
            })),
            Stmt::GenericFor(gf) => Fragment::Map(Box::new(ForGenericStatementStream {
                state: 0,
                variables: NameList(gf.names()),
                iterators: ExprList(gf.expr_list()),
                body: Body(gf.block()),
            })),
            Stmt::While(w) => Fragment::Map(Box::new(WhileStatementStream {
                state: 0,
                condition: Expr(w.condition()),
                body: Body(w.block()),
            })),
            Stmt::LocalFunction(f) => {
                let body = f.func_body();
                let name = f.name();
                Fragment::Map(Box::new(FunctionDeclarationStream {
                    state: 0,
                    is_local: true,
                    identifier: FunctionIdentifier::Simple(name.clone()),
                    parameters: FunctionParameters(body),
                    body: Body(body.block()),
                }))
            }
            _x => todo!("{:?}", _x),
        }
    }
}

pub struct CallExpression<'a, 'b>(&'b FunctionCall<'a>);
impl<'a, 'b> Serialize for CallExpression<'a, 'b> {
    fn begin(&self) -> Fragment {
        call_expr_fragment(self.0)
    }
}

fn call_expr_fragment<'a, 'b>(f: &'b FunctionCall<'a>) -> Fragment<'b> {
    prefix_suffixes_fragment(f.prefix(), f.iter_suffixes())
}

pub struct IfClauses<'a, 'b>(&'b If<'a>);
impl<'a, 'b> Serialize for IfClauses<'a, 'b> {
    fn begin(&self) -> Fragment {
        Fragment::Seq(Box::new(IfClausesStream {
            state: 0,
            if_clause: IfClause(self.0.condition(), self.0.block()),
            elseif_iter: self.0.else_if().map(|v| v.iter()),
            elseif_clause: None,
            else_clause: self.0.else_block().map(ElseClause),
        }))
    }
}

pub struct IfClause<'a, 'b>(&'b Expression<'a>, &'b Block<'a>);
pub struct ElseifClause<'a, 'b>(&'b ElseIf<'a>);
pub struct ElseClause<'a, 'b>(&'b Block<'a>);

impl<'a, 'b> Serialize for IfClause<'a, 'b> {
    fn begin(&self) -> Fragment {
        Fragment::Map(Box::new(IfClauseStream {
            state: 0,
            condition: Expr(self.0),
            body: Body(self.1),
        }))
    }
}
impl<'a, 'b> Serialize for ElseifClause<'a, 'b> {
    fn begin(&self) -> Fragment {
        Fragment::Map(Box::new(ElseifClauseStream {
            state: 0,
            condition: Expr(self.0.condition()),
            body: Body(self.0.block()),
        }))
    }
}
impl<'a, 'b> Serialize for ElseClause<'a, 'b> {
    fn begin(&self) -> Fragment {
        Fragment::Map(Box::new(ElseClauseStream {
            state: 0,
            body: Body(self.0),
        }))
    }
}

pub enum FunctionIdentifier<'a, 'b> {
    Name(&'b FunctionName<'a>),
    Simple(TokenReference<'a>),
    None,
}
impl<'a, 'b> Serialize for FunctionIdentifier<'a, 'b> {
    fn begin(&self) -> Fragment {
        match self {
            Self::Name(func_name) => {
                let mut name_vec = func_name.names().iter().cloned().collect::<Vec<_>>();
                if let Some(method) = func_name.method_name() {
                    Fragment::Map(Box::new(MemberExprStream {
                        state: 0,
                        base: FunctionIdentifierNames(Cow::Owned(name_vec)),
                        indexer: Cow::Borrowed(":"),
                        identifier: Identifier(Cow::Borrowed(&method)),
                    }))
                } else {
                    if let Some(last) = name_vec.pop() {
                        let base = Cow::Owned(name_vec);
                        function_identifier_fragment(base, last)
                    } else {
                        todo!("missing function identifier")
                    }
                }
            }
            Self::Simple(ident) => identifier_fragment(&ident),
            Self::None => Fragment::Null,
        }
    }
}

fn function_identifier_fragment<'a, 'b>(
    base: Cow<'b, [Cow<'b, TokenReference<'a>>]>,
    last: Cow<'b, TokenReference<'a>>,
) -> Fragment<'b> {
    if base.len() == 0 {
        identifier_fragment(&last)
    } else {
        Fragment::Map(Box::new(MemberExprStream {
            state: 0,
            base: FunctionIdentifierNames(base),
            indexer: Cow::Borrowed("."),
            identifier: Identifier(last),
        }))
    }
}

pub struct FunctionIdentifierNames<'a, 'b>(Cow<'b, [Cow<'b, TokenReference<'a>>]>);
impl<'a, 'b> Serialize for FunctionIdentifierNames<'a, 'b> {
    fn begin(&self) -> Fragment {
        if let Some((last, base)) = self.0.split_last() {
            function_identifier_fragment(Cow::Borrowed(base), last.clone())
        } else {
            todo!("missing function identifier name")
        }
    }
}

pub struct FunctionParameters<'a, 'b>(&'b FunctionBody<'a>);
impl<'a, 'b> Serialize for FunctionParameters<'a, 'b> {
    fn begin(&self) -> Fragment {
        Fragment::Seq(Box::new(FunctionParameterStream {
            iter: self.0.iter_parameters(),
            place: None,
        }))
    }
}

pub struct Param<'a, 'b>(&'b Parameter<'a>);
impl<'a, 'b> Serialize for Param<'a, 'b> {
    fn begin(&self) -> Fragment {
        match self.0 {
            Parameter::Ellipse(_token) => Fragment::Map(Box::new(VarargLiteralStream { state: 0 })),
            Parameter::Name(token) => identifier_fragment(token.as_ref()),
        }
    }
}

pub struct NameList<'a, 'b>(&'b Punctuated<'a, Cow<'b, TokenReference<'a>>>);
impl<'a, 'b> Serialize for NameList<'a, 'b> {
    fn begin(&self) -> Fragment {
        Fragment::Seq(Box::new(NameStream {
            iter: self.0.iter(),
            curr: None,
        }))
    }
}

pub struct VarList<'a, 'b>(&'b Punctuated<'a, Var<'a>>);
impl<'a, 'b> Serialize for VarList<'a, 'b> {
    fn begin(&self) -> Fragment {
        Fragment::Seq(Box::new(VarStream {
            iter: self.0.iter(),
            curr: None,
        }))
    }
}

pub struct Variable<'a, 'b>(&'b Var<'a>);
impl<'a, 'b> Serialize for Variable<'a, 'b> {
    fn begin(&self) -> Fragment {
        var_fragment(self.0)
    }
}

fn var_fragment<'a, 'b>(var: &'b Var<'a>) -> Fragment<'b> {
    match var {
        Var::Name(n) => name_fragment(n.as_ref()),
        Var::Expression(e) => var_expr_fragment(e),
    }
}

fn name_fragment<'a, 'b>(name: &'b TokenReference<'a>) -> Fragment<'a> {
    Fragment::Map(Box::new(IdentifierStream {
        name: name.to_string(),
        state: 0,
    }))
}

fn var_expr_fragment<'a, 'b>(expr: &'b VarExpression<'a>) -> Fragment<'b> {
    prefix_suffixes_fragment(expr.prefix(), expr.iter_suffixes())
}

fn prefix_suffixes_fragment<'a, 'b, I>(prefix: &'b Prefix<'a>, iter_suffixes: I) -> Fragment<'b>
where
    I: Iterator<Item = &'b Suffix<'a>>,
{
    let mut suffix_vec: Vec<_> = iter_suffixes.collect();
    if let Some(last_suffix) = suffix_vec.pop() {
        let suffixes = Cow::Owned(suffix_vec);
        let base = VarExpr(prefix, suffixes);
        var_suffix_fragment(base, last_suffix)
    } else {
        var_prefix_fragment(prefix)
    }
}

fn make_call_expr_fragment<'a, 'b, Base: Serialize + 'b>(
    base: Base,
    args: &'b FunctionArgs<'a>,
) -> Fragment<'b> {
    match args {
        FunctionArgs::Parentheses { arguments, .. } => {
            Fragment::Map(Box::new(CallExpressionStream {
                state: 0,
                base,
                arguments: ExprList(arguments),
            }))
        }
        FunctionArgs::TableConstructor(t) => Fragment::Map(Box::new(TableCallExpressionStream {
            state: 0,
            base,
            arguments: TableConstructorExpr(t),
        })),
        FunctionArgs::String(token) => Fragment::Map(Box::new(StringCallExpressionStream {
            state: 0,
            base,
            argument: Token(token.as_ref()),
        })),
    }
}

fn var_suffix_fragment<'a, 'b>(base: VarExpr<'a, 'b>, last_suffix: &'b Suffix<'a>) -> Fragment<'b> {
    match last_suffix {
        Suffix::Call(Call::AnonymousCall(args)) => make_call_expr_fragment(base, args),
        Suffix::Call(Call::MethodCall(method_call)) => {
            let args = method_call.args();
            let base = MemberExpr {
                base,
                indexer: Cow::Borrowed(":"),
                identifier: method_call.name().clone(),
            };
            make_call_expr_fragment(base, args)
        }
        Suffix::Index(Index::Brackets { expression, .. }) => {
            Fragment::Map(Box::new(VarIndexExprStream {
                state: 0,
                base,
                index: Expr(expression),
            }))
        }
        Suffix::Index(Index::Dot { name, .. }) => Fragment::Map(Box::new(MemberExprStream {
            state: 0,
            base,
            indexer: Cow::Borrowed("."), //dot.to_string(),
            identifier: Identifier(name.clone()),
        })),
    }
}

fn var_prefix_fragment<'a, 'b>(prefix: &'b Prefix<'a>) -> Fragment<'b> {
    match prefix {
        Prefix::Name(n) => name_fragment(n.as_ref()),
        Prefix::Expression(expr) => expr_fragment(expr, false),
    }
}

pub struct VarExpr<'a, 'b>(&'b Prefix<'a>, Cow<'b, [&'b Suffix<'a>]>);
impl<'a, 'b> Serialize for VarExpr<'a, 'b> {
    fn begin(&self) -> Fragment {
        if let Some((last, elements)) = self.1.split_last() {
            let base = VarExpr(self.0, Cow::Borrowed(elements));
            var_suffix_fragment(base, last)
        } else {
            var_prefix_fragment(self.0)
        }
    }
}

pub struct Token<'a, 'b>(&'b TokenReference<'a>);
impl<'a, 'b> Serialize for Token<'a, 'b> {
    fn begin(&self) -> Fragment {
        token_fragment(&self.0)
    }
}

pub struct MemberExpr<'a, Base: Serialize> {
    base: Base,
    indexer: Cow<'a, str>,
    identifier: TokenReference<'a>,
}
impl<'a, Base: Serialize> Serialize for MemberExpr<'a, Base> {
    fn begin(&self) -> Fragment {
        Fragment::Map(Box::new(MemberExprStream {
            state: 0,
            base: &self.base,
            indexer: self.indexer.clone(),
            identifier: Identifier(Cow::Borrowed(&self.identifier)),
        }))
    }
}

fn identifier_fragment<'a, 'b>(token: &'b TokenReference<'a>) -> Fragment<'static> {
    Fragment::Map(Box::new(IdentifierStream {
        state: 0,
        name: token.to_string(),
    }))
}

#[derive(Clone)]
pub struct Identifier<'a, 'b>(Cow<'b, TokenReference<'a>>);
impl<'a, 'b> Serialize for Identifier<'a, 'b> {
    fn begin(&self) -> Fragment {
        identifier_fragment(&self.0)
    }
}

pub struct ExprList<'a, 'b>(&'b Punctuated<'a, Expression<'a>>);
impl<'a, 'b> Serialize for ExprList<'a, 'b> {
    fn begin(&self) -> Fragment {
        Fragment::Seq(Box::new(ExprStream {
            iter: self.0.iter(),
            curr: None,
        }))
    }
}

fn parse_or_panic<'a, R: FromStr>(text: &str, token: TokenReference<'a>) -> R {
    match text.parse() {
        Ok(value) => value,
        Err(_e) => {
            let pos = token.start_position();
            panic!(
                "Could not parse: {:?} at line {} char {}",
                text,
                pos.line(),
                pos.character()
            );
        }
    }
}

fn token_fragment<'a, 'b>(token: &'b TokenReference<'a>) -> Fragment<'a> {
    match token.token_type().deref() {
        TokenType::StringLiteral { literal, .. } => Fragment::Map(Box::new(StringLiteralStream {
            state: 0,
            value: literal.clone(),
            raw: token.to_string(),
        })),
        TokenType::Number { text } => {
            if text.starts_with("0x") {
                let value = u64::from_str_radix(&text[2..], 16).unwrap();
                Fragment::Map(Box::new(NumericLiteralStream {
                    state: 0,
                    value,
                    raw: text.clone(),
                }))
            /*} else if text.starts_with(".") {
            parse_or_panic(&format!("0{}", text), token.clone())*/
            } else {
                let value: f64 = parse_or_panic(text, token.clone());
                Fragment::Map(Box::new(NumericLiteralStream {
                    state: 0,
                    value,
                    raw: text.clone(),
                }))
            }
        }
        TokenType::Symbol { symbol } => match symbol {
            Symbol::Nil => Fragment::Map(Box::new(NilLiteralStream {
                state: 0,
                value: Null,
                raw: Cow::Borrowed("nil"),
            })),
            Symbol::True => Fragment::Map(Box::new(BooleanLiteralStream {
                state: 0,
                value: true,
                raw: Cow::Borrowed("true"),
            })),
            Symbol::False => Fragment::Map(Box::new(BooleanLiteralStream {
                state: 0,
                value: false,
                raw: Cow::Borrowed("false"),
            })),
            Symbol::Ellipse => Fragment::Map(Box::new(VarargLiteralStream { state: 0 })),
            _s => todo!("{:?}", _s),
        },
        _tt => todo!("{:?}", _tt),
    }
}

fn value_fragment<'a, 'b>(value: &'b Value<'a>) -> Fragment<'b> {
    match value {
        Value::TableConstructor(t) => table_constructor_fragment(t),
        Value::Function((_t, f)) => Fragment::Map(Box::new(FunctionDeclarationStream {
            state: 0,
            is_local: false,
            identifier: FunctionIdentifier::None,
            parameters: FunctionParameters(f),
            body: Body(f.block()),
        })),
        Value::FunctionCall(f) => call_expr_fragment(f),
        Value::String(s) => token_fragment(s.as_ref()),
        Value::Number(s) => token_fragment(s.as_ref()),
        Value::Var(v) => var_fragment(v),
        Value::Symbol(s) => token_fragment(s.as_ref()),
        Value::ParseExpression(expr) => expr_fragment(expr, true),
    }
}

pub struct Express<'a, 'b> {
    in_parens: bool,
    kind: ExpressKind<'a, 'b>,
}

pub enum ExpressKind<'a, 'b> {
    Binary {
        op: BinOp<'a>,
        left: Box<Express<'a, 'b>>,
        right: Box<Express<'a, 'b>>,
    },
    Unary {
        op: UnOp<'a>,
        arg: Box<Express<'a, 'b>>,
    },
    Value(&'b Value<'a>),
}

pub enum ExprRef<'a, 'b, 'c> {
    Borrowed(&'c Express<'a, 'b>),
    Owned(Box<Express<'a, 'b>>),
}

fn un_op_to_string(op: &UnOp<'_>) -> String {
    match op {
        UnOp::Minus(t) => t.to_string(),
        UnOp::Not(t) => t.to_string(),
        UnOp::Hash(t) => t.to_string(),
    }
}

fn bin_op_to_string(op: &BinOp<'_>) -> (String, bool) {
    match op {
        BinOp::And(o) => (o.to_string(), true),
        BinOp::Caret(o) => (o.to_string(), false),
        BinOp::GreaterThan(o) => (o.to_string(), false),
        BinOp::GreaterThanEqual(o) => (o.to_string(), false),
        BinOp::LessThan(o) => (o.to_string(), false),
        BinOp::LessThanEqual(o) => (o.to_string(), false),
        BinOp::Minus(o) => (o.to_string(), false),
        BinOp::Or(o) => (o.to_string(), true),
        BinOp::Percent(o) => (o.to_string(), false),
        BinOp::Plus(o) => (o.to_string(), false),
        BinOp::Slash(o) => (o.to_string(), false),
        BinOp::Star(o) => (o.to_string(), false),
        BinOp::TildeEqual(o) => (o.to_string(), false),
        BinOp::TwoDots(o) => (o.to_string(), false),
        BinOp::TwoEqual(o) => (o.to_string(), false),
    }
}

impl<'a, 'b, 'c> Serialize for ExprRef<'a, 'b, 'c> {
    fn begin(&self) -> Fragment {
        let express = match self {
            Self::Owned(e) => e.as_ref(),
            Self::Borrowed(r) => r,
        };
        match &express.kind {
            ExpressKind::Value(value) => value_fragment(value),
            ExpressKind::Binary { op, left, right } => {
                let (operator, is_logical) = bin_op_to_string(op);
                Fragment::Map(Box::new(BinaryExprStream {
                    state: 0,
                    operator,
                    is_logical,
                    left: ExprRef::Borrowed(left),
                    right: ExprRef::Borrowed(right),
                    in_parens: express.in_parens,
                }))
            }
            ExpressKind::Unary { op, arg } => Fragment::Map(Box::new(UnaryExprStream {
                state: 0,
                operator: un_op_to_string(op),
                argument: ExprRef::Borrowed(arg),
                in_parens: express.in_parens,
            })),
        }
    }
}

impl<'a, 'b> Express<'a, 'b> {
    fn with_parens(mut self) -> Self {
        self.in_parens = true;
        self
    }

    fn into_fragment(self) -> Fragment<'b> {
        match self.kind {
            ExpressKind::Value(value) => value_fragment(value),
            ExpressKind::Binary { op, left, right } => {
                let (operator, is_logical) = bin_op_to_string(&op);
                Fragment::Map(Box::new(BinaryExprStream {
                    state: 0,
                    operator,
                    is_logical,
                    left: ExprRef::Owned(left),
                    right: ExprRef::Owned(right),
                    in_parens: self.in_parens,
                }))
            }
            ExpressKind::Unary { op, arg } => Fragment::Map(Box::new(UnaryExprStream {
                state: 0,
                operator: un_op_to_string(&op),
                argument: ExprRef::Owned(arg),
                in_parens: self.in_parens,
            })),
        }
    }
}

fn expr_fix_precedence<'a, 'b>(expr: &'b Expression<'a>, in_parens: bool) -> Express<'a, 'b> {
    type UnOps<'a, 'b> = Vec<&'b UnOp<'a>>;
    type BinOps<'a, 'b> = Vec<(&'b BinOp<'a>, Express<'a, 'b>, u8)>;

    let mut bin_op: BinOps<'a, 'b> = Vec::new();
    let mut un_op: UnOps<'a, 'b> = Vec::new();
    let mut in_expr = expr;
    let mut precedence = 0;

    fn unroll_un_ops<'a, 'b>(
        un_op: &mut UnOps<'a, 'b>,
        mut xpr: Express<'a, 'b>,
    ) -> Express<'a, 'b> {
        let mut iter = un_op.drain(..);
        loop {
            if let Some(op) = iter.next_back() {
                xpr = Express {
                    kind: ExpressKind::Unary {
                        op: op.clone(),
                        arg: Box::new(xpr),
                    },
                    in_parens: false,
                };
            } else {
                break xpr;
            }
        }
    }

    fn unroll_bin_ops<'a, 'b>(
        bin_op: &mut BinOps<'a, 'b>,
        mut last: Express<'a, 'b>,
    ) -> Express<'a, 'b> {
        let mut iter = bin_op.drain(..);
        loop {
            if let Some((op, lxp, _)) = iter.next_back() {
                last = Express {
                    kind: ExpressKind::Binary {
                        op: op.clone(),
                        left: Box::new(lxp),
                        right: Box::new(last),
                    },
                    in_parens: false,
                };
            } else {
                break last;
            }
        }
    }

    let express: Express<'a, 'b> = loop {
        match in_expr {
            Expression::Value { value, binop } => {
                let xpr = {
                    let val = Express {
                        kind: ExpressKind::Value(value),
                        in_parens: false,
                    };
                    unroll_un_ops(&mut un_op, val)
                };

                if let Some(bin_op_rhs) = binop {
                    let binop = bin_op_rhs.bin_op();
                    in_expr = bin_op_rhs.rhs();

                    let bin_prec = binary_precedence(binop);
                    let mut lxp = xpr;
                    loop {
                        if bin_prec <= precedence {
                            // if the current op binds less than the last one
                            // -> pop the last partial bin op from the stack
                            if let Some((op, lhs, last_prec)) = bin_op.pop() {
                                precedence = last_prec;
                                lxp = Express {
                                    kind: ExpressKind::Binary {
                                        op: op.clone(),
                                        left: Box::new(lhs),
                                        right: Box::new(lxp),
                                    },
                                    in_parens: false,
                                };
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }

                    bin_op.push((binop, lxp, precedence));
                    precedence = bin_prec;
                } else {
                    break unroll_bin_ops(&mut bin_op, xpr);
                }
            }
            Expression::UnaryOperator { unop, expression } => {
                in_expr = expression;
                un_op.push(unop);
            }
            Expression::Parentheses { expression, .. } => {
                let inner = expr_fix_precedence(expression, true);
                let inner = unroll_un_ops(&mut un_op, inner);
                break unroll_bin_ops(&mut bin_op, inner);
            }
        }
    };
    if in_parens {
        express.with_parens()
    } else {
        express
    }
}

fn expr_fragment<'a, 'b>(expr: &'b Expression<'a>, in_parens: bool) -> Fragment<'b> {
    let express = expr_fix_precedence(expr, in_parens);
    express.into_fragment()
}

pub struct Expr<'a, 'b>(&'b Expression<'a>);
impl<'a, 'b> Serialize for Expr<'a, 'b> {
    fn begin(&self) -> Fragment {
        expr_fragment(self.0, false)
    }
}

pub struct Val<'a, 'b>(&'b Value<'a>);
impl<'a, 'b> Serialize for Val<'a, 'b> {
    fn begin(&self) -> Fragment {
        value_fragment(self.0)
    }
}

pub struct TableConstructorExpr<'a, 'b>(&'b TableConstructor<'a>);
impl<'a, 'b> Serialize for TableConstructorExpr<'a, 'b> {
    fn begin(&self) -> Fragment {
        table_constructor_fragment(self.0)
    }
}

fn table_constructor_fragment<'a, 'b>(t: &'b TableConstructor<'a>) -> Fragment<'b> {
    Fragment::Map(Box::new(TableConstructorStream {
        fields: TableConstructorFields(t),
        state: 0,
    }))
}

pub struct TableConstructorFields<'a, 'b>(&'b TableConstructor<'a>);
impl<'a, 'b> Serialize for TableConstructorFields<'a, 'b> {
    fn begin(&self) -> Fragment {
        Fragment::Seq(Box::new(TableConstructorFieldsStream {
            iter: self.0.iter_fields(),
            curr: None,
        }))
    }
}

pub struct TableConsField<'a, 'b>(&'b TableConstructorField<'a>);
impl<'a, 'b> Serialize for TableConsField<'a, 'b> {
    fn begin(&self) -> Fragment {
        match &(self.0).0 {
            Field::NameKey { key, value, .. } => Fragment::Map(Box::new(TableKeyStringStream {
                state: 0,
                key: Identifier(key.clone()),
                value: Expr(value),
            })),
            Field::ExpressionKey { key, value, .. } => Fragment::Map(Box::new(TableKeyStream {
                state: 0,
                key: Expr(key),
                value: Expr(value),
            })),
            Field::NoKey(expr) => Fragment::Map(Box::new(TableValueStream {
                state: 0,
                value: Expr(expr),
            })),
        }
    }
}

fn binary_precedence(op: &BinOp<'_>) -> u8 {
    match op {
        BinOp::And(_tr) => 2,
        BinOp::Caret(_tr) => 8,
        BinOp::GreaterThan(_tr) => 3,
        BinOp::GreaterThanEqual(_tr) => 3,
        BinOp::LessThan(_tr) => 3,
        BinOp::LessThanEqual(_tr) => 3,
        BinOp::Minus(_tr) => 5,
        BinOp::Or(_tr) => 1,
        BinOp::Percent(_tr) => 6,
        BinOp::Plus(_tr) => 5,
        BinOp::Slash(_tr) => 6,
        BinOp::Star(_tr) => 6,
        BinOp::TildeEqual(_tr) => 3,
        BinOp::TwoDots(_tr) => 4,
        BinOp::TwoEqual(_tr) => 3,
    }
}

fn _unary_precedence(op: &UnOp<'_>) -> u8 {
    match op {
        UnOp::Minus(_tr) => 7,
        UnOp::Not(_tr) => 7,
        UnOp::Hash(_tr) => 7,
    }
}

pub struct LastStatement<'a, 'b>(&'b LastStmt<'a>);
impl<'a, 'b> Serialize for LastStatement<'a, 'b> {
    fn begin(&self) -> Fragment {
        match self.0 {
            LastStmt::Break(_token) => Fragment::Map(Box::new(BreakStatementStream { state: 0 })),
            LastStmt::Return(stmt) => Fragment::Map(Box::new(ReturnStatementStream {
                state: 0,
                arguments: ExprList(stmt.returns()),
            })),
        }
    }
}

pub struct Null;
impl Serialize for Null {
    fn begin(&self) -> Fragment {
        Fragment::Null
    }
}
impl Seq for Null {
    fn next(&mut self) -> Option<&dyn Serialize> {
        None
    }
}
impl Map for Null {
    fn next(&mut self) -> Option<(Cow<str>, &dyn Serialize)> {
        None
    }
}
