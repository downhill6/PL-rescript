module Tiny3 = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(string)
    | Let(string, expr, expr)
    | Fn(list<string>, expr) // Func (prameter, body)
    | App(expr, list<expr>) // Apply function(fn, args)

  // interpreter
  type rec value =
    | Vint(int)
    | Vclosure(env, list<string>, expr) // 实现静态作用域 lexical scope
  and env = list<(string, value)>

  // type error when add int and closure
  let vadd = (v1, v2): value => {
    switch (v1, v2) {
    | (Vint(a), Vint(b)) => Vint(a + b)
    | _ => Js.Exn.raiseTypeError("add int and closure")
    }
  }

  let vmul = (v1, v2): value => {
    switch (v1, v2) {
    | (Vint(a), Vint(b)) => Vint(a * b)
    | _ => Js.Exn.raiseTypeError("mul int and closure")
    }
  }

  let rec eval = (expr: expr, env: env): value => {
    switch expr {
    | Cst(i) => Vint(i)
    | Add(a, b) => vadd(eval(a, env), eval(b, env))
    | Mul(a, b) => vmul(eval(a, env), eval(b, env))
    | Var(x) => Util.assoc(x, env)
    | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
    | Fn(xs, e) => Vclosure(env, xs, e) // computation suspended for application
    | App(e, es) => {
        let val = eval(e, env)
        switch val {
        | Vint(_) => assert(false)
        | Vclosure(env_closure, xs, body) => {
            let vs = List.map(es, e => eval(e, env))
            let fun_env = List.concatMany([List.zip(xs, vs), env_closure])
            eval(body, fun_env)
          }
        }
      }
    }
  }

  Console.log(
    eval(
      Add(Let("x", Cst(2), Var("x")), App(Fn(list{"x"}, Mul(Cst(2), Var("x"))), list{Cst(3)})),
      list{},
    ),
  )
}

module Nameless = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)
    | Fn(expr) // no need to store the arity, precompute the index of parameters
    | App(expr, list<expr>) // we need semantics checkig!

  type rec value =
    | Vint(int)
    | Vclosure(env, expr)
  and env = list<value>

  // type error when add int and closure
  let vadd = (v1, v2): value => {
    switch (v1, v2) {
    | (Vint(a), Vint(b)) => Vint(a + b)
    | _ => Js.Exn.raiseTypeError("add int and closure")
    }
  }

  let vmul = (v1, v2): value => {
    switch (v1, v2) {
    | (Vint(a), Vint(b)) => Vint(a * b)
    | _ => Js.Exn.raiseTypeError("mul int and closure")
    }
  }

  let rec eval = (expr: expr, env: env): value => {
    switch expr {
    | Cst(i) => Vint(i)
    | Add(a, b) => vadd(eval(a, env), eval(b, env))
    | Mul(a, b) => vmul(eval(a, env), eval(b, env))
    | Var(n) => env->List.getExn(n)
    | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
    | Fn(e) => Vclosure(env, e)
    | App(e, es) => {
        let val = eval(e, env)
        switch val {
        | Vint(_) => assert(false)
        | Vclosure(env_closure, body) => {
            let vs = List.map(es, e => eval(e, env))
            let fun_env = List.concatMany([vs, env_closure]) // piece together
            eval(body, fun_env)
          }
        }
      }
    }
  }

  // homework1: compile expr to Nameless expr
  // compile1: Compile expr with variable names to expr with indices
  type cenv = list<string>

  let index = (cenv, x) => {
    let rec go = (cenv, n) => {
      switch cenv {
      | list{} => assert(false)
      | list{a, ...rest} =>
        if a == x {
          n
        } else {
          go(rest, n + 1)
        }
      }
    }
    go(cenv, 0)
  }

  let rec compile = (expr: Tiny3.expr, cenv: cenv): expr => {
    switch expr {
    | Cst(i) => Cst(i)
    | Add(a, b) => Add(compile(a, cenv), compile(b, cenv))
    | Mul(a, b) => Mul(compile(a, cenv), compile(b, cenv))
    | Var(x) => Var(index(cenv, x))
    | Let(x, e1, e2) => Let(compile(e1, cenv), compile(e2, list{x, ...cenv}))
    | Fn(xs, e) => Fn(compile(e, list{...xs, ...cenv}))
    | App(e, es) => App(compile(e, cenv), List.map(es, e => compile(e, cenv)))
    }
  }

  let expr1: Tiny3.expr = Let(
    "fn1",
    Fn(list{"x"}, Mul(Cst(2), Var("x"))),
    App(Var("fn1"), list{Cst(7)}),
  )
  Console.log(eval(compile(expr1, list{}), list{}))
}

// homework2: 解决 Vclosure 捕获多余变量导致内存泄露的问题
