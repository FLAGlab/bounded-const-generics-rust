#[derive(Clone, Debug)]
pub struct BlockID(pub usize);

#[derive(Clone, Debug)]
pub struct StatementID(pub usize);

#[derive(Clone, Debug)]
pub struct Place(pub usize);

#[derive(Clone, Debug)]
pub struct Constant(pub i32);

#[derive(Clone, Debug)]
pub enum Operand {
    Use(Place),
    Const(Constant),
}

#[derive(Clone, Debug)]
pub enum Rvalue {
    Ref(Place),
    Add(Operand, Operand),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Assign(Place, Rvalue),
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Return,
    Goto(BlockID),
}

#[derive(Clone, Debug)]
pub struct Block {
    statements: Vec<Statement>,
    terminator: Terminator,
}

impl Block {
    pub fn new(statements: Vec<Statement>, terminator: Terminator) -> Self {
        Block {
            statements,
            terminator,
        }
    }

    pub fn is_statement(&self, id: &StatementID) -> bool {
        id.0 < self.statements.len()
    }

    pub fn get_statement(&self, id: &StatementID) -> Option<&Statement> {
        self.statements.get(id.0)
    }

    pub fn get_terminator(&self) -> &Terminator {
        &self.terminator
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    locals: usize,
    args: usize,
    blocks: Vec<Block>,
}

impl Function {
    pub fn new(locals: usize, args: usize, blocks: Vec<Block>) -> Self {
        Function {
            locals,
            args,
            blocks,
        }
    }

    pub fn get_block(&self, id: &BlockID) -> Option<&Block> {
        self.blocks.get(id.0)
    }

    pub fn locals(&self) -> usize {
        self.locals
    }

    pub fn args(&self) -> usize {
        self.args
    }
}

// impl Eval for Operand {
//     fn eval(&self, ctx: &mut Context) -> Expr {
//     }
// }
//
// impl Eval for Rvalue {
//     fn eval(&self, ctx: &mut Context) -> Expr {
//         match self {
//             Rvalue::Add(op1, op2) => {
//                 let e1 = op1.eval(ctx);
//                 let e2 = op2.eval(ctx);
//                 match (e1, e2) {
//                     (Expr::Const(c1), Expr::Const(c2)) => Expr::Const(c1 + c2),
//                     (e1, e2) => Expr::Add(Box::new(e1), Box::new(e2)),
//                 }
//             }
//             Rvalue::Ref(place) => ctx.get_var(*place).clone(),
//         }
//     }
// }
//
// impl Eval for Statement {
//     fn eval(&self, ctx: &mut Context) -> Expr {
//         match self {
//             Statement::Assign(place, rvalue) => {
//                 let e = rvalue.eval(ctx);
//                 ctx.add_var(*place, e);
//             }
//         };
//         Expr::Unit
//     }
// }
//
// impl Eval for Terminator {
//     fn eval(&self, ctx: &mut Context) -> Expr {
//         match self {
//             Terminator::Return => ctx.get_var(0).clone(),
//             Terminator::Jump(block_id) => {
//                 let block = ctx.get_block(*block_id).clone();
//                 block.eval(ctx)
//             }
//         }
//     }
// }
//
// #[derive(Clone)]
// struct Block {
//     stmts: Vec<Statement>,
//     term: Terminator,
// }
//
// impl Eval for Block {
//     fn eval(&self, ctx: &mut Context) -> Expr {
//         for stmt in &self.stmts {
//             stmt.eval(ctx);
//         }
//         self.term.eval(ctx)
//     }
// }
//
//
// impl Eval for Function {
//     fn eval(&self, ctx: &mut Context) -> Expr {
//         for i in 0..self.vars {
//             ctx.add_var(i, Expr::Unit);
//         }
//
//         for i in 0..self.args {
//             ctx.add_var(i + 1, Expr::Var(i + 1))
//         }
//
//         for (i, block) in self.blocks.iter().enumerate() {
//             ctx.add_block(i, block.clone());
//         }
//         self.blocks.get(0).unwrap().eval(ctx)
//     }
// }
