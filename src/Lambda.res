type rec lambda =
  | Var(string)
  | Fn(string, lambda) // Fn(parameter, body)
  | App(lambda, lambda) // App(f, arg)

// Homework: implement the substitution
// v must be closed/ a[v/x], 当 v 没有捕获 free value 时
// let rec subst = (x: string, v: lambda, a: lambda): lambda => {
//   switch a {
//   | Var(y) => x == y ? v : a
//   | Fn(y, b) => x == y ? a : Fn(y, subst(x, v, b))
//   | App(fn, arg) => App(subst(x, v, fn), subst(x, v, arg))
//   }
// }

// the new name must be unique like JS new symbol
let rename = (t, old, new) => {
  let rec go = t => {
    switch t {
    | Var(x) =>
      if x == old {
        Var(new)
      } else {
        t
      }
    | Fn(x, a) =>
      if x == old {
        Fn(new, go(a))
      } else {
        Fn(x, go(a))
      }
    | App(a, b) => App(go(a), go(b))
    }
  }
  go(t)
}

let get_fresh_name = () => {
  let name = ref("name")
  let index = ref(0)
  () => {
    index := index.contents + 1
    name.contents ++ Js.Int.toString(index.contents)
  }
}
let fresh_name = get_fresh_name()

// t[u/x] where u might have free variables
let rec subst = (x, u, t) => {
  switch t {
  | Var(y) =>
    if x == y {
      u
    } else {
      t
    }
  | Fn(y, b) =>
    if x == y {
      t
    } else {
      // 不相等说明要把 body 里是 x 的给替换掉，如果 freeVar 里有值等于 y，要把 y rename 了
      let y' = fresh_name()
      let b' = rename(b, y, y')
      Fn(y', subst(x, u, b'))
    }
  | App(a, b) => App(subst(x, u, a), subst(x, u, b))
  }
}

// Evaluate closed term, call by value
let rec eval = (t: lambda) => {
  switch t {
  | Var(_) => assert(false)
  | Fn(_, _) => t
  | App(f, arg) => {
      let val = eval(f)
      switch val {
      | Fn(x, body) => {
          let va = eval(arg)
          eval(subst(x, va, body))
        }
      | _ => assert(false)
      }
    }
  }
}

let print_lambda = l => {
  let print_paren = (b, s) => {
    if b {
      "(" ++ s ++ ")"
    } else {
      s
    }
  }
  let rec go = (l, p) => {
    switch l {
    | Var(x) => x
    | Fn(x, a) => print_paren(p > 0, "fun " ++ x ++ " -> " ++ go(a, 0))
    | App(a, b) => print_paren(p > 1, go(a, 1) ++ " " ++ go(b, 2))
    }
  }
  go(l, 0)
}

let test1 = Fn("x", Var("x"))
let test2 = App(Fn("x", Var("x")), Fn("x", Var("x")))
let test3 = App(
  Fn("y", Fn("x", App(Var("x"), Var("y")))),
  Fn("y", Fn("x", App(Var("x"), Var("y")))),
)
let test4 = App(
  App(Fn("y", Fn("x", App(Var("x"), Var("y")))), Fn("y", Fn("x", App(Var("x"), Var("y"))))),
  Fn("z", Var("z")),
)
let test5 = App(Fn("u", App(Var("u"), Fn("y", App(Var("y"), Var("z"))))), Fn("z", Var("z")))

Js.log(print_lambda(test1))
Js.log(print_lambda(eval(test1)))

Js.log(print_lambda(test2))
Js.log(print_lambda(eval(test2)))

Js.log(print_lambda(eval(Fn("z", App(Fn("x", Var("x")), Var("y"))))))
Js.log(print_lambda(test3))
Js.log(print_lambda(eval(test3)))

Js.log(print_lambda(test4))
Js.log(print_lambda(eval(test4)))

Js.log(print_lambda(test5))
Js.log(print_lambda(eval(test5)))
