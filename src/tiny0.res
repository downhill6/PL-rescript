// Concrete syntax
// expr :
// | INT // 1
// | expr "+" expr // 1 + 2 , (1+2) + 3
// | expr "*" expr // 1 * 2
// | "(" expr ")"

module Tiny0 = {
    // Abstract syntax
    type rec expr = 
        | Cst (int) // i
        | Add (expr, expr) // a + b
        | Mul (expr, expr) // a * b 

    // interpreter
    let rec eval = (expr: expr) => {
        switch expr {
        | Cst(i) => i
        | Add(a, b) => eval(a) + eval(b)     // 隐式的使用了宿主语言的栈
        | Mul(a, b) => eval(a) * eval(b)
        }
    }
}

module StackMachine = {
    // lowering to a stack machine and interpret
    type instr = Cst(int) | Add | Mul     // instruction set 指令集 no recursive
    type instrs = list<instr>             // 指令集流，相当于计算机里的一段代码段，用来动态操作一堆操作数
    type operand = int                    // 操作数
    type stack = list<operand>            // 操作数数组

    let rec eval = (instrs: instrs, stack: stack) => {
        switch (instrs, stack) {
            | (list{ Cst(i), ...rest}, _) => eval(rest, list{i, ...stack})                  // 将操作数压入栈
            | (list{ Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a+b, ...stk})    // 从栈中取出操作数，执行操作，将结果压入栈
            | (list{ Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a*b, ...stk})
            | (list{}, list{a}) => a                                                        // 返回栈顶元素
            | _ => assert false
        }
    }
}



// homework0 compiler
module Compiler = {
    let concat = List.concat;

    let rec compile = (expr: Tiny0.expr): StackMachine.instrs => {
        switch expr {
            | Cst(i) => list{ Cst(i) }
            | Add(a, b) => concat(concat(compile(a), compile(b)), list{ StackMachine.Add })
            | Mul(a, b) => concat(concat(compile(a), compile(b)), list{ StackMachine.Mul })
        }
    }
}

module Test = {
    let test = (e) => if e { Console.log("test success") } else { Console.error("test faile")}

    let example_expr: Tiny0.expr = Add(Cst(1), Mul(Cst(2), Cst(3)))
    test(Tiny0.eval(example_expr) == 7)
    let example_st_code: StackMachine.instrs = list{Cst(40), Cst(2), Add, Cst(10), Mul}
    test(StackMachine.eval(example_st_code, list{}) == 420)
    let compiled = Compiler.compile(example_expr)
    test(StackMachine.eval(compiled, list{}) == 7)
}