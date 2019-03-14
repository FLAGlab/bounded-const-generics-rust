use crate::mir::*;

#[derive(Clone, Debug)]
pub enum Expr {
    Place(Place),
    Value(i32),
    Add(Box<Expr>, Box<Expr>),
    Nil,
}

struct Frame<'a> {
    locals: Vec<usize>,
    block: BlockID,
    statement: StatementID,
    function: &'a Function,
}

pub struct Interpreter<'a> {
    stack: Vec<Frame<'a>>,
    state: Vec<Expr>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Interpreter {
            stack: Vec::new(),
            state: Vec::new()
        }
    }

    pub fn eval_function(&mut self, function: &'a Function) -> Option<()> {
        let mut locals = Vec::new();
        for i in 0..function.locals() {
            locals.push(self.state.len());
            if i > 0 && i <= function.args() {
                self.state.push(Expr::Place(Place(i)));
            } else {
                self.state.push(Expr::Nil);
            }
        }  
        self.stack.push(Frame{
            locals,
            block :BlockID(0),
            statement: StatementID(0),
            function,
        });
        while () == self.step()? {
        }
        Some(())
    }

    pub fn step(&mut self) -> Option<()> {
        let frame = self.stack.first()?;
        let block = frame.function.get_block(&frame.block)?;

        if block.is_statement(&frame.statement) {
            let statement = block.get_statement(&frame.statement)?;
            self.eval_statement(statement)
        } else {
            let terminator = block.get_terminator();
            self.eval_terminator(terminator)
        }
    }

    fn eval_statement(&mut self, statement: &Statement) -> Option<()> {
        let sid = &mut self.stack.first_mut()?.statement;
        *sid = StatementID(sid.0 + 1);
        match statement {
            Statement::Assign(place, rvalue) => self.eval_rvalue_to_place(rvalue, place),
        }
    }
    
    fn eval_terminator(&mut self, terminator: &Terminator) -> Option<()> {
        match terminator {
            Terminator::Return => {
                let frame = self.stack.pop()?;
                println!("{:?}", self.state.get(frame.locals[0]));
            }
            Terminator::Jump(block_id) => {
                let frame = self.stack.first_mut()?;
                frame.block = block_id.clone();
                frame.statement = StatementID(0);
            }
        }
        Some(())
    }

    fn eval_rvalue_to_place(&mut self, rvalue: &Rvalue, place: &Place) -> Option<()> {
        let locals = &self.stack.first()?.locals;
        let address = *locals.get(place.0)?;
        let value = match rvalue {
            Rvalue::Add(op1, op2) => {
                let e1 = self.eval_operand(op1);
                let e2 = self.eval_operand(op2);
                match (e1, e2) {
                    (Expr::Value(c1), Expr::Value(c2)) => Expr::Value(c1 + c2),
                    (e1, e2) => Expr::Add(Box::new(e1), Box::new(e2)),
                }
            }
            Rvalue::Ref(place) => {
                self.state.get(*locals.get(place.0)?)?.clone()
            }
        };
        *self.state.get_mut(address)? = value;
        Some(())
    }

    fn eval_operand(&self, operand: &Operand) -> Expr {
        match operand {
            Operand::Use(place) => self.state.get(place.0).cloned().unwrap_or(Expr::Place(place.clone())),
            Operand::Const(constant) => Expr::Value(constant.0)
        }
    }
}
