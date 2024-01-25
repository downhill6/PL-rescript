// Abstract syntax
type rec expr = 
    | Cst (int) // i
    | Add (expr, expr)
    | Mul (expr, expr)
    | Var (string)
    | Let (string, expr, expr)

type env = list<(string, int)>

module Nameless = {
    // Abstract syntax
    type rec expr = 
        | Cst (int)         // i
        | Add (expr, expr)
        | Mul (expr, expr)
        | Var (int)         // 加快寻址
        | Let (expr, expr)

    type env = list<(int)>
    
    // interpreter
    let rec eval = (expr: expr, env: env) => {
        switch expr {
        | Cst(i) => i
        | Add(a, b) => eval(a, env) + eval(b, env)
        | Mul(a, b) => eval(a, env) * eval(b, env)
        | Var(n) => env->List.getExn(n)
        | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
        }
    }
}

// compile1: Compile expr with variable names to expr with indices
type cenv = list<string>

let index = (cenv, x) => {
    let rec go = (cenv, n) => {
        switch cenv {
            | list{} => assert false
            | list{a, ...rest} => if a == x { n } else {go (rest, n+1)}
        }
    }
    go(cenv, 0)
}

let rec compile1 = (expr: expr, cenv: cenv): Nameless.expr => {
    switch expr {
        | Cst(i) => Cst(i)
        | Add(a, b) => Add(compile1(a, cenv), compile1(b, cenv))
        | Mul(a, b) => Mul(compile1(a, cenv), compile1(b, cenv))
        | Var(x) => Var(index(cenv, x))
        | Let(x, e1, e2) => Let(compile1(e1, cenv), compile1(e2, list{x, ...cenv}))
    }
}

module Instr1 = {
    // Machine Instructions with variables
    type instr = Cst(int) | Add | Mul | Var(int) | Pop | Swap
    type instrs = list<instr>
    type operand = int
    type stack = list<operand>

    // Homework1: Interpreter for stack machine with variables
    let rec eval = (instrs: instrs, stack: stack) => {
        switch (instrs, stack) {
            | (list{ Cst(i), ...rest}, _) => eval(rest, list{i, ...stack})
            | (list{ Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a+b, ...stk})
            | (list{ Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a*b, ...stk})
            | (list{ Var(i), ...rest }, _) => eval(rest, list{stack->List.getExn(i) , ...stack})
            | (list{ Pop, ...rest}, _) => eval(rest, stack->List.tailExn)
            | (list{ Swap, ...rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
            | (list{}, list{a}) => a
            | _ => assert false
        }
    }
    // eval(list{Cst(2),Cst(2), Var(0), Cst(7), Add, Swap, Pop, Add }, list{})->Js.log

    // Homework2: Compile Nameless.expr to Machine Instructions
    type sv = Slocal | Stemp
    type senv = list<sv>

    let sindex = (senv, i) => {
        let rec go = (senv, i, acc) => {
            switch senv {
                | list{Stemp, ...rest} => go(rest, i, acc + 1)
                | list{Slocal, ...rest} => if i === 0 { acc } else { go(rest, i - 1, acc + 1) }
                | list{} => assert false
            }
        }
        go(senv, i, 0)
    }

    let scompile = (expr): instrs => {
        let rec comp = (expr: Nameless.expr, senv: senv) => {
            switch (expr) {
                | Cst(i) => list{ Cst(i) }
                | Var(i) => list{ Var(sindex(senv, i)) }
                | Add(e1, e2) => List.concatMany([comp(e1, senv), comp(e2, list{Stemp, ...senv}), list{ Add }])
                | Mul(e1, e2) => List.concatMany([comp(e1, senv), comp(e2, list{Stemp, ...senv}), list{ Mul }])
                | Let(e1, e2) => List.concatMany([comp(e1, senv), comp(e2, list{Slocal, ...senv }), list{ Swap, Pop } ])
            }
        }
        comp(expr, list{})
    }    
}

module Instr2 = {
    // Homework3: TinyLang -> Stack Machine Nameless
    type sv = Slocal(string) | Stmp
    type senv = list<sv>

    let sindex = (senv, s) => {
        let rec go = (senv, acc) => {
        switch senv {
            | list{} => raise (Not_found)
            | list{Slocal(x), ...rest} => if x == s { acc } else {go (rest, acc+1)}
            | list{Stmp, ...rest} => go (rest, acc+1)
        }
        }
        go(senv, 0)
    }

    let scompile = (expr) => {
        let rec go = (expr: expr, senv: senv) : list<Instr1.instr> => {
            switch expr {
                | Cst(i) => list{ Cst(i) }
                | Var(s) => list{ Var(sindex(senv, s)) }
                | Add(e1, e2) => List.concatMany([ go(e1, senv), go(e2, list{Stmp,... senv}), list{ Instr1.Add } ])
                | Mul(e1, e2) => List.concatMany([ go(e1, senv), go(e2, list{Stmp,... senv}), list{ Instr1.Mul } ])
                | Let(x, e1, e2) => List.concatMany([ go(e1, senv), go(e2, list{Slocal(x),... senv}), list{ Swap, Pop } ])
            }
        }
        go(expr, list{})
    }
}

module Test = {
    let test = (result, target) => if result === target { Console.log("test success") } else { Console.error("test faile")}

    let code = Mul(Add(Cst(1), Let("x", Cst(2), Mul(Var("x"), Cst(5)))), Let("x", Cst(4), Let("y", Cst(5), Mul(Var("x"), Var("y")))))
    // Instr1
    compile1(code, list{})->Nameless.eval(list{})->test(220)
    code->compile1(list{})->Instr1.scompile->Instr1.eval(list{})->test(220)
    // Instr2
    code->Instr2.scompile->Instr1.eval(list{})->test(220)
}