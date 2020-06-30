use {
    super::{
        Body, CallExpression, ElseClause, ElseifClause, Expr, ExprList, ExprRef,
        FunctionIdentifier, FunctionParameters, Identifier, IfClause, IfClauses, LastStatement,
        NameList, Null, Param, Statement, TableConsField, TableConstructorExpr,
        TableConstructorFields, Token, VarExpr, VarList, Variable,
    },
    full_moon::{
        ast::{
            punctuated::Iter as PIter, ElseIf, Expression, Parameter, Stmt, TableConstructorField,
            Var,
        },
        tokenizer::TokenReference,
    },
    miniserde::{
        ser::{Map, Seq},
        Serialize,
    },
    std::borrow::Cow,
};

macro_rules! impl_Map(
    ($name:ident : $type:literal {
        $($s:literal: $key:literal = $field:ident,)*
    }) => (
        impl<'a, 'b> Map for $name<'a, 'b> {
            next_Map!($type {
                $($s: $key = $field,)*
            });
        }
    );
);

macro_rules! next_Map(
    ($type:literal {
        $($s:literal: $key:literal = $field:ident,)*
    }) => (
        fn next(&mut self) -> Option<(Cow<str>, &dyn Serialize)> {
            match self.state {
                0 => type_Map!(self, $type),
                $(
                    $s => step_Map!($s: $key = self.$field),
                )*
                _ => None,
            }
        }
    );
);

macro_rules! type_Map(
    ($self:ident, $type:literal) => ({
        $self.state = 1;
        Some((Cow::Borrowed("type"), &$type))
    })
);
macro_rules! step_Map(
    ($s:literal: $key:literal = $self:ident.$field:ident) => ({
        $self.state = $s + 1;
        Some((Cow::Borrowed($key), &$self.$field))
    });
);

pub struct ChunkStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) body: Body<'a, 'b>,
}

impl_Map!(ChunkStream: "Chunk" {
    1: "body" = body,
});

pub struct BodyStream<'a, 'b, Iter: Iterator<Item = &'b Stmt<'a>>> {
    pub(crate) state: u8,
    pub(crate) stmts: Iter,
    pub(crate) curr: Option<Statement<'a, 'b>>,
    pub(crate) last: Option<LastStatement<'a, 'b>>,
}

impl<'a, 'b, Iter> Seq for BodyStream<'a, 'b, Iter>
where
    Iter: Iterator<Item = &'b Stmt<'a>>,
{
    fn next(&mut self) -> Option<&dyn Serialize> {
        match self.state {
            // ignore curr, fetch next
            0 => {
                self.curr = self.stmts.next().map(Statement);
                if let Some(stmt) = &self.curr {
                    Some(stmt)
                } else if let Some(last) = &self.last {
                    self.state = 1;
                    Some(last)
                } else {
                    self.state = 1;
                    None
                }
            }
            _ => None,
        }
    }
}

pub struct CallStatementStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) expression: CallExpression<'a, 'b>,
}
impl_Map!(CallStatementStream: "CallStatement" {
    1: "expression" = expression,
});

pub struct ForGenericStatementStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) variables: NameList<'a, 'b>,
    pub(crate) iterators: ExprList<'a, 'b>,
    pub(crate) body: Body<'a, 'b>,
}
impl_Map!(ForGenericStatementStream: "ForGenericStatement" {
    1: "variables" = variables,
    2: "iterators" = iterators,
    3: "body" = body,
});

pub struct ForNumericStatementStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) variable: Identifier<'a, 'b>,
    pub(crate) start: Expr<'a, 'b>,
    pub(crate) end: Expr<'a, 'b>,
    pub(crate) step: Option<Expr<'a, 'b>>,
    pub(crate) body: Body<'a, 'b>,
}
impl_Map!(ForNumericStatementStream: "ForNumericStatement" {
    1: "variable" = variable,
    2: "start" = start,
    3: "end" = end,
    4: "step" = step,
    5: "body" = body,
});

pub struct WhileStatementStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) condition: Expr<'a, 'b>,
    pub(crate) body: Body<'a, 'b>,
}
impl_Map!(WhileStatementStream: "WhileStatement" {
    1: "condition" = condition,
    2: "body" = body,
});

pub struct IfStatementStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) clauses: IfClauses<'a, 'b>,
}
impl_Map!(IfStatementStream: "IfStatement" {
    1: "clauses" = clauses,
});

pub struct IfClausesStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) if_clause: IfClause<'a, 'b>,
    pub(crate) elseif_iter: Option<std::slice::Iter<'b, ElseIf<'a>>>,
    pub(crate) elseif_clause: Option<ElseifClause<'a, 'b>>,
    pub(crate) else_clause: Option<ElseClause<'a, 'b>>,
}
impl<'a, 'b> Seq for IfClausesStream<'a, 'b> {
    fn next(&mut self) -> Option<&dyn Serialize> {
        match self.state {
            0 => {
                self.state = 1;
                Some(&self.if_clause)
            }
            // ignore curr, fetch next
            1 => {
                if let Some(iter) = &mut self.elseif_iter {
                    if let Some(elseif) = iter.next() {
                        self.elseif_clause = Some(ElseifClause(elseif));
                        return Some(&self.elseif_clause);
                    }
                }
                self.state = 2;
                self.next()
            }
            2 => {
                self.state = 3;
                self.else_clause.as_ref().map(|c| c as &dyn Serialize)
            }
            _ => None,
        }
    }
}

pub struct IfClauseStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) condition: Expr<'a, 'b>,
    pub(crate) body: Body<'a, 'b>,
}
pub struct ElseifClauseStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) condition: Expr<'a, 'b>,
    pub(crate) body: Body<'a, 'b>,
}
pub struct ElseClauseStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) body: Body<'a, 'b>,
}
impl_Map!(IfClauseStream: "IfClause" {
    1: "condition" = condition,
    2: "body" = body,
});
impl_Map!(ElseifClauseStream: "ElseifClause" {
    1: "condition" = condition,
    2: "body" = body,
});
impl_Map!(ElseClauseStream: "ElseClause" {
    1: "body" = body,
});

pub struct FunctionDeclarationStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) is_local: bool,
    pub(crate) identifier: FunctionIdentifier<'a, 'b>,
    pub(crate) parameters: FunctionParameters<'a, 'b>,
    pub(crate) body: Body<'a, 'b>,
}
impl_Map!(FunctionDeclarationStream: "FunctionDeclaration" {
    1: "identifier" = identifier,
    2: "isLocal" = is_local,
    3: "parameters" = parameters,
    4: "body" = body,
});

pub struct FunctionParameterStream<'a, 'b, Iter> {
    pub(crate) iter: Iter,
    pub(crate) place: Option<Param<'a, 'b>>,
}
impl<'a, 'b, Iter> Seq for FunctionParameterStream<'a, 'b, Iter>
where
    Iter: Iterator<Item = &'b Parameter<'a>>,
{
    fn next(&mut self) -> Option<&dyn Serialize> {
        self.place = self.iter.next().map(Param);
        self.place.as_ref().map(|r| r as &dyn Serialize)
    }
}

pub struct AssignmentStmtStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) variables: VarList<'a, 'b>,
    pub(crate) init: ExprList<'a, 'b>,
}
impl_Map!(AssignmentStmtStream: "AssignmentStatement" {
    1: "variables" = variables,
    2: "init" = init,
});
pub struct LocalStatementStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) variables: NameList<'a, 'b>,
    pub(crate) init: ExprList<'a, 'b>,
}
impl_Map!(LocalStatementStream: "LocalStatement" {
    1: "variables" = variables,
    2: "init" = init,
});

pub struct NameStream<'a, 'b> {
    pub(crate) iter: PIter<'a, 'b, Cow<'b, TokenReference<'a>>>,
    pub(crate) curr: Option<Identifier<'a, 'b>>,
}
impl<'a, 'b> Seq for NameStream<'a, 'b> {
    fn next(&mut self) -> Option<&dyn Serialize> {
        self.curr = self.iter.next().map(|tr| Identifier(tr.clone()));
        self.curr.as_ref().map(|r| r as &dyn Serialize)
    }
}

pub struct VarStream<'a, 'b> {
    pub(crate) iter: PIter<'a, 'b, Var<'a>>,
    pub(crate) curr: Option<Variable<'a, 'b>>,
}
impl<'a, 'b> Seq for VarStream<'a, 'b> {
    fn next(&mut self) -> Option<&dyn Serialize> {
        self.curr = self.iter.next().map(Variable);
        self.curr.as_ref().map(|r| r as &dyn Serialize)
    }
}

pub struct CallExpressionStream<'a, 'b, Base> {
    pub(crate) state: u8,
    pub(crate) base: Base,
    pub(crate) arguments: ExprList<'a, 'b>,
}
impl<'a, 'b, Base: Serialize> Map for CallExpressionStream<'a, 'b, Base> {
    next_Map!("CallExpression" {
        1: "base" = base,
        2: "arguments" = arguments,
    });
}

pub struct StringCallExpressionStream<'a, 'b, Base> {
    pub(crate) state: u8,
    pub(crate) base: Base,
    pub(crate) argument: Token<'a, 'b>,
}
impl<'a, 'b, Base: Serialize> Map for StringCallExpressionStream<'a, 'b, Base> {
    next_Map!("StringCallExpression" {
        1: "base" = base,
        2: "argument" = argument,
    });
}

pub struct TableCallExpressionStream<'a, 'b, Base> {
    pub(crate) state: u8,
    pub(crate) base: Base,
    pub(crate) arguments: TableConstructorExpr<'a, 'b>,
}
impl<'a, 'b, Base: Serialize> Map for TableCallExpressionStream<'a, 'b, Base> {
    next_Map!("TableCallExpression" {
        1: "base" = base,
        2: "arguments" = arguments,
    });
}

pub struct VarIndexExprStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) base: VarExpr<'a, 'b>,
    pub(crate) index: Expr<'a, 'b>,
}
impl<'a, 'b> Map for VarIndexExprStream<'a, 'b> {
    next_Map!("IndexExpression" {
        1: "base" = base,
        2: "index" = index,
    });
}

pub struct MemberExprStream<'a, 'b, Base: Serialize> {
    pub(crate) state: u8,
    pub(crate) base: Base,
    pub(crate) indexer: Cow<'a, str>,
    pub(crate) identifier: Identifier<'a, 'b>,
}
impl<'a, 'b, Base: Serialize> Map for MemberExprStream<'a, 'b, Base> {
    next_Map!("MemberExpression" {
        1: "indexer" = indexer,
        2: "identifier" = identifier,
        3: "base" = base,
    });
}

pub struct IdentifierStream {
    pub(crate) name: String,
    pub(crate) state: u8,
}
impl Map for IdentifierStream {
    next_Map!("Identifier" {
        1: "name" = name,
    });
}

pub struct ExprStream<'a, 'b> {
    pub(crate) iter: PIter<'a, 'b, Expression<'a>>,
    pub(crate) curr: Option<Expr<'a, 'b>>,
}
impl<'a, 'b> Seq for ExprStream<'a, 'b> {
    fn next(&mut self) -> Option<&dyn Serialize> {
        self.curr = self.iter.next().map(Expr);
        self.curr.as_ref().map(|r| r as &dyn Serialize)
    }
}

pub struct TableConstructorStream<'a, 'b> {
    pub(crate) fields: TableConstructorFields<'a, 'b>,
    pub(crate) state: u8,
}
impl_Map!(TableConstructorStream : "TableConstructorExpression" {
    1: "fields" = fields,
});

pub struct TableConstructorFieldsStream<'a, 'b, T> {
    pub(crate) iter: T,
    pub(crate) curr: Option<TableConsField<'a, 'b>>,
}
impl<'a, 'b, T> Seq for TableConstructorFieldsStream<'a, 'b, T>
where
    T: Iterator<Item = &'b TableConstructorField<'a>>,
{
    fn next(&mut self) -> Option<&dyn Serialize> {
        self.curr = self.iter.next().map(TableConsField);
        self.curr.as_ref().map(|r| r as &dyn Serialize)
    }
}

pub struct TableKeyStringStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) key: Identifier<'a, 'b>,
    pub(crate) value: Expr<'a, 'b>,
}
impl_Map!(TableKeyStringStream : "TableKeyString" {
    1: "key" = key,
    2: "value" = value,
});

pub struct TableKeyStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) key: Expr<'a, 'b>,
    pub(crate) value: Expr<'a, 'b>,
}
impl_Map!(TableKeyStream : "TableKey" {
    1: "key" = key,
    2: "value" = value,
});

pub struct TableValueStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) value: Expr<'a, 'b>,
}
impl_Map!(TableValueStream : "TableValue" {
    1: "value" = value,
});

pub struct StringLiteralStream<'a> {
    pub(crate) state: u8,
    pub(crate) value: Cow<'a, str>,
    pub(crate) raw: String,
}
impl<'a> Map for StringLiteralStream<'a> {
    next_Map!("StringLiteral" {
        1: "value" = value,
        2: "raw" = raw,
    });
}

pub struct NumericLiteralStream<'a, Val: Serialize> {
    pub(crate) state: u8,
    pub(crate) value: Val,
    pub(crate) raw: Cow<'a, str>,
}
impl<'a, Val: Serialize> Map for NumericLiteralStream<'a, Val> {
    next_Map!("NumericLiteral" {
        1: "value" = value,
        2: "raw" = raw,
    });
}
pub struct VarargLiteralStream {
    pub(crate) state: u8,
}
impl Map for VarargLiteralStream {
    fn next(&mut self) -> Option<(Cow<str>, &dyn Serialize)> {
        match self.state {
            0 => type_Map!(self, "VarargLiteral"),
            1 => {
                self.state = 2;
                Some((Cow::Borrowed("value"), &"..."))
            }
            2 => {
                self.state = 3;
                Some((Cow::Borrowed("raw"), &"..."))
            }
            _ => None,
        }
    }
}

pub struct NilLiteralStream<'a> {
    pub(crate) state: u8,
    pub(crate) value: Null,
    pub(crate) raw: Cow<'a, str>,
}
impl<'a> Map for NilLiteralStream<'a> {
    next_Map!("NilLiteral" {
        1: "value" = value,
        2: "raw" = raw,
    });
}

pub struct BooleanLiteralStream<'a> {
    pub(crate) state: u8,
    pub(crate) value: bool,
    pub(crate) raw: Cow<'a, str>,
}
impl<'a> Map for BooleanLiteralStream<'a> {
    next_Map!("BooleanLiteral" {
        1: "value" = value,
        2: "raw" = raw,
    });
}

pub struct UnaryExprStream<'a, 'b, 'c> {
    pub(crate) state: u8,
    pub(crate) operator: String,
    pub(crate) argument: ExprRef<'a, 'b, 'c>,
    pub(crate) in_parens: bool,
}
impl<'a, 'b, 'c> Map for UnaryExprStream<'a, 'b, 'c> {
    fn next(&mut self) -> Option<(Cow<str>, &dyn Serialize)> {
        match self.state {
            0 => type_Map!(self, "UnaryExpression"),
            1 => step_Map!(1: "operator" = self.operator),
            2 => step_Map!(2: "argument" = self.argument),
            3 if self.in_parens => {
                self.state = 4;
                Some((Cow::Borrowed("inParens"), &self.in_parens))
            }
            _ => None,
        }
    }
}

pub struct BinaryExprStream<'a, 'b, 'c> {
    pub(crate) state: u8,
    pub(crate) operator: String,
    pub(crate) left: ExprRef<'a, 'b, 'c>,
    pub(crate) right: ExprRef<'a, 'b, 'c>,
    pub(crate) in_parens: bool,
    pub(crate) is_logical: bool,
}
impl<'a, 'b, 'c> Map for BinaryExprStream<'a, 'b, 'c> {
    fn next(&mut self) -> Option<(Cow<str>, &dyn Serialize)> {
        match self.state {
            0 => {
                self.state = 1;
                if self.is_logical {
                    Some((Cow::Borrowed("type"), &"LogicalExpression"))
                } else {
                    Some((Cow::Borrowed("type"), &"BinaryExpression"))
                }
            }
            1 => step_Map!(1: "operator" = self.operator),
            2 => step_Map!(2: "left" = self.left),
            3 => step_Map!(3: "right" = self.right),
            4 if self.in_parens => {
                self.state = 5;
                Some((Cow::Borrowed("inParens"), &self.in_parens))
            }
            _ => None,
        }
    }
}

pub struct ReturnStatementStream<'a, 'b> {
    pub(crate) state: u8,
    pub(crate) arguments: ExprList<'a, 'b>,
}
impl_Map!(ReturnStatementStream: "ReturnStatement" {
    1: "arguments" = arguments,
});

pub struct BreakStatementStream {
    pub(crate) state: u8,
}
impl Map for BreakStatementStream {
    next_Map!("BreakStatement" {});
}
