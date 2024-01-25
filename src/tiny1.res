module Tiny1 = {
    // Abstract syntax
    type rec expr = 
        | Cst (int) // i
        | Add (expr, expr)
        | Mul (expr, expr)
        | Var (string)
        | Let (string, expr, expr)

    type env = list<(string, int)>

    let assoc = (x, env) => {
        let val = env->List.getAssoc(x, (k, item) => k == item)
        switch val {
            | Some(value) => value
            | None => assert false
        }
    }
    
    // interpreter
    let rec eval = (expr: expr, env: env) => {
        switch expr {
        | Cst(i) => i
        | Add(a, b) => eval(a, env) + eval(b, env)
        | Mul(a, b) => eval(a, env) * eval(b, env)
        | Var(x) => assoc(x, env)
        | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
        }
    }

    let test = (e) => if e { Console.log("test success") } else { Console.error("test faile")}
    test(eval(Add(Cst(6), Let("x", Cst(4), Mul(Cst(2), Var("x")))), list{}) == 14)
}