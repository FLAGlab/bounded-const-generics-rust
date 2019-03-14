mod interpreter;
mod mir;

use crate::interpreter::*;
use crate::mir::*;

// struct Context {
//     vars: Vec<Expr>,
//     blocks: Vec<Block>,
// }
//
// impl Context {
//     fn new() -> Self {
//         Context {
//             vars: Vec::new(),
//             blocks: Vec::new(),
//         }
//     }
//
//     fn add_var(&mut self, place: Place, expr: Expr) {
//         if place == self.vars.len() {
//             self.vars.push(expr);
//         } else if place < self.vars.len() {
//             self.vars.insert(place, expr);
//         } else {
//             panic!("out of bounds")
//         }
//     }
//
//     fn get_var(&mut self, place: Place) -> &Expr {
//         self.vars.get(place).unwrap()
//     }
//
//     fn add_block(&mut self, block_id: BlockID, block: Block) {
//         if block_id == self.blocks.len() {
//             self.blocks.push(block);
//         } else if block_id < self.blocks.len() {
//             self.blocks.insert(block_id, block);
//         } else {
//             panic!("out of bounds")
//         }
//     }
//
//     fn get_block(&mut self, block_id: BlockID) -> &Block {
//         self.blocks.get(block_id).unwrap()
//     }
// }

// fn foo(x: usize, y: usize) -> usize {
//     let z = x + y;
//     return 2 + z
// }
//
// fn bar(x: usize, y: usize) -> usize {
//     return y + 2 + x
// }
//
//
// fn foo(_1: usize, _2: usize) -> usize {
//     let mut _0: usize;                   // return place
//     let _3: usize;                   // "z" in scope 2 at src/main.rs:3:9: 3:10
//     let mut _4: usize;
//     let mut _5: usize;
//     let mut _6: usize;
//
//     bb0: {
//         _4 = _1;                         // bb0[2]: scope 0 at src/main.rs:3:13: 3:14
//         _5 = _2;                         // bb0[4]: scope 0 at src/main.rs:3:17: 3:18
//         _3 = Add(move _4, move _5);      // bb0[5]: scope 0 at src/main.rs:3:13: 3:18
//         _6 = _3;                         // bb0[9]: scope 1 at src/main.rs:4:16: 4:17
//         _0 = Add(const 2usize, move _6); // bb0[10]: scope 1 at src/main.rs:4:12: 4:17
//         return;                          // bb0[13]: scope 0 at src/main.rs:5:2: 5:2
//     }
// }
//
//
// fn bar(_1: usize, _2: usize) -> usize {
//     let mut _0: usize;                   // return place
//     let mut _3: usize;
//     let mut _4: usize;
//     let mut _5: usize;
//
//     bb0: {
//         _4 = _2;                         // bb0[2]: scope 0 at src/main.rs:4:12: 4:13
//         _3 = Add(move _4, const 2usize); // bb0[3]: scope 0 at src/main.rs:4:12: 4:17
//         _5 = _1;                         // bb0[6]: scope 0 at src/main.rs:4:20: 4:21
//         _0 = Add(move _3, move _5);      // bb0[7]: scope 0 at src/main.rs:4:12: 4:21
//         return;                          // bb0[10]: scope 0 at src/main.rs:5:2: 5:2
//     }
// }
//
// fn fib(_1: i32) -> i32{
//     let mut _0: i32;                     // return place
//     let mut _3: i32;
//     let mut _4: i32;
//     let mut _5: i32;
//     let mut _6: i32;
//     let mut _7: i32;
//     let mut _8: i32;
//
//     bb0: {
//         switchInt(_1) -> [0i32: bb1, 1i32: bb2, otherwise: bb3]; // bb0[0]: scope 0 at src/main.rs:3:9: 3:10
//     }
//
//     bb1: {
//         _0 = const 1i32;                 // bb1[0]: scope 0 at src/main.rs:3:14: 3:15
//         goto -> bb4;                     // bb1[1]: scope 0 at src/main.rs:2:5: 6:6
//     }
//
//     bb2: {
//         _0 = const 1i32;                 // bb2[0]: scope 0 at src/main.rs:4:14: 4:15
//         goto -> bb4;                     // bb2[1]: scope 0 at src/main.rs:2:5: 6:6
//     }
//
//     bb3: {
//         _2 = _1;                         // bb3[1]: scope 0 at src/main.rs:5:9: 5:10
//         _5 = _2;                         // bb3[5]: scope 1 at src/main.rs:5:18: 5:19
//         _4 = Sub(move _5, const 1i32);   // bb3[6]: scope 1 at src/main.rs:5:18: 5:23
//         _3 = const fib(move _4) -> bb5;  // bb3[8]: scope 1 at src/main.rs:5:14: 5:24
//     }
//
//     bb4: {
//         return;                          // bb4[1]: scope 0 at src/main.rs:7:2: 7:2
//     }
//
//     bb5: {
//         _8 = _2;                         // bb5[4]: scope 1 at src/main.rs:5:32: 5:33
//         _7 = Sub(move _8, const 2i32);   // bb5[5]: scope 1 at src/main.rs:5:32: 5:37
//         _6 = const fib(move _7) -> bb6;  // bb5[7]: scope 1 at src/main.rs:5:28: 5:38
//     }
//
//     bb6: {
//         _0 = Add(move _3, move _6);      // bb6[1]: scope 1 at src/main.rs:5:14: 5:38
//         goto -> bb4;                     // bb6[4]: scope 0 at src/main.rs:2:5: 6:6
//     }
// }

fn main() {
    let mut interpreter = Interpreter::new();

    let foo = Function::new(
        7,
        2,
        vec![Block::new(
            vec![
                Statement::Assign(Place(4), Rvalue::Ref(Place(1))),
                Statement::Assign(Place(5), Rvalue::Ref(Place(2))),
                Statement::Assign(
                    Place(3),
                    Rvalue::Add(Operand::Use(Place(4)), Operand::Use(Place(5))),
                ),
                Statement::Assign(Place(6), Rvalue::Ref(Place(3))),
                Statement::Assign(
                    Place(0),
                    Rvalue::Add(Operand::Const(Constant(2)), Operand::Use(Place(6))),
                ),
            ],
            Terminator::Return,
        )],
    );

    let bar = Function::new(
        6,
        2,
        vec![Block::new(
            vec![
                Statement::Assign(Place(4), Rvalue::Ref(Place(2))),
                Statement::Assign(
                    Place(5),
                    Rvalue::Add(Operand::Use(Place(4)), Operand::Const(Constant(2))),
                ),
                Statement::Assign(Place(3), Rvalue::Ref(Place(1))),
                Statement::Assign(
                    Place(0),
                    Rvalue::Add(Operand::Use(Place(3)), Operand::Use(Place(5))),
                ),
            ],
            Terminator::Return,
        )],
    );

    interpreter.eval_function(&foo);
    interpreter.eval_function(&bar);
}
