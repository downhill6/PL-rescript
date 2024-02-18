// De Bruijn index IR
// https://zhuanlan.zhihu.com/p/94601499
// https://magic.huohuo.moe/html/DBI.html
type rec debru =
  | Var(int)
  | App(debru, debru)
  | Fn(debru)

// Var(j) becomes Var(i+j) if j >= d
let rec shift_aux = (i, d, u) => {
  switch u {
  | Var(j) =>
    if j >= d {
      Var(i + j)
    } else {
      u
    }
  | Fn(b) => Fn(shift_aux(i, d + 1, b))
  | App(a, b) => App(shift_aux(i, d, a), shift_aux(i, d, b))
  }
}

let shift = (i, u) => shift_aux(i, 0, u)

// t[u/i]: use u to replace Var(i) in term t
let rec subst = (t, i, u) => {
  switch t {
  | Var(j) => j == i ? u : t
  | Fn(b) => Fn(subst(b, i + 1, shift(1, u)))
  | App(a, b) => App(subst(a, i, u), subst(b, i, u))
  }
}

let rec eval = (t: debru): debru => {
  switch t {
  | Var(_) => t
  | Fn(b) => Fn(eval(b))
  | App(f, a) =>
    switch eval(f) {
    // subst 归约后少了一层 λ 绑定，需要 shift -1
    | Fn(b) => eval(shift(-1, subst(b, 0, shift(1, eval(a)))))
    | b => App(b, eval(a))
    }
  }
}

/* Example usage */
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
    | Var(x) => Js.Int.toString(x)
    | Fn(a) => print_paren(p > 0, "fun -> " ++ go(a, 0))
    | App(a, b) => print_paren(p > 1, go(a, 1) ++ " " ++ go(b, 2))
    }
  }
  go(l, 0)
}

let test_1 = App(Fn(App(Var(0), Var(0))), Fn(Var(0)))
Js.log(print_lambda(test_1))
Js.log(print_lambda(eval(test_1)))
let test_2 = Fn(App(Var(0), App(Fn(Var(0)), Fn(Var(0)))))
Js.log(print_lambda(test_2))
Js.log(print_lambda(eval(test_2)))

// Homework: support let
module DebruLet = {
  type rec debruLet =
    | Var(int)
    | App(debruLet, debruLet)
    | Fn(debruLet)
    | Let(debruLet, debruLet)

  // 注意到eval Let(l1, l2) == eval App(Fn(l2),l1)
  // 在实现时将所有Let(l1, l2)替换成App(Fn(l2),l1)即可。

  // Var(j) becomes Var(i+j) if j >= d
  let rec shift_aux = (i, d, u) => {
    switch u {
    | Var(j) =>
      if j >= d {
        Var(i + j)
      } else {
        u
      }
    | Fn(b) => Fn(shift_aux(i, d + 1, b))
    | App(a, b) => App(shift_aux(i, d, a), shift_aux(i, d, b))
    | Let(a, b) => shift_aux(i, d, App(Fn(b), a))
    }
  }

  let shift = (i, u) => shift_aux(i, 0, u)

  // t[u/i]: use u to replace Var(i) in term t
  let rec subst = (t, i, u) => {
    switch t {
    | Var(j) => j == i ? u : t
    | Fn(b) => Fn(subst(b, i + 1, shift(1, u)))
    | App(a, b) => App(subst(a, i, u), subst(b, i, u))
    | Let(a, b) => subst(App(Fn(b), a), i, u)
    }
  }

  let rec eval = (t: debruLet) => {
    switch t {
    | Var(_) => t
    | Fn(b) => Fn(eval(b))
    | App(f, a) =>
      switch eval(f) {
      | Fn(b) => eval(shift(-1, subst(b, 0, shift(1, eval(a)))))
      | b => App(b, eval(a))
      }
    | Let(a, b) => eval(App(Fn(b), a))
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
      | Var(x) => Js.Int.toString(x)
      | Fn(a) => print_paren(p > 0, "fun -> " ++ go(a, 0))
      | App(a, b) => print_paren(p > 1, go(a, 1) ++ " " ++ go(b, 2))
      | Let(a, b) => print_paren(p > 1, "let " ++ go(a, 1) ++ " in " ++ go(b, 2))
      }
    }
    go(l, 0)
  }

  let test_3 = Let(Fn(Var(0)), App(Var(0), Var(0)))
  Js.log(print_lambda(test_3))
  Js.log(print_lambda(eval(test_3)))
}
