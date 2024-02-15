// homework: Implement Church numberals and arithmetic functions using lambda calculus

// Peano numbers
type rec nat = Z | S(nat)
let peano_zero = Z
let peano_one = S(Z)
let peano_two = S(S(Z))

// Church numbers
type cnum<'a> = ('a => 'a, 'a) => 'a
let church_zero = (_, z) => z
let church_one = (s, z) => s(z)
let church_two = (s, z) => s(s(z))
// successor function
let peano_succ = x => S(x)
let church_succ = n => (s, z) => s(n(s, z))

// isomorphism between Peano and Church numbers
let church_to_peano = n => n(x => S(x), Z)
let rec peano_to_church = n => {
  switch n {
  | Z => church_zero
  | S(n) => church_succ(peano_to_church(n))
  }
}

// predecessor
let pred = n => {
  let init = (church_zero, church_zero)
  let iter = ((_, y)) => (y, church_succ(y))
  let (ans, _) = n(iter, init)
  ans
}

let church_decode = n => n(x => x + 1, 0)
// Homework: implement the arithmetic functions for peano numbers
let rec peano_add = (n: nat, m: nat): nat => {
  switch n {
  | Z => m
  // hint: use peano_succ
  | S(n') => peano_succ(peano_add(n', m))
  }
}
let rec peano_mul = (n: nat, m: nat): nat => {
  switch n {
  | Z => peano_zero
  // hint: use peano_succ
  | S(n') => peano_add(m, peano_mul(n', m))
  }
}

// Homework: implement the arithmetic functions for church numbers
let church_add = (n: cnum<_>, m: cnum<_>): cnum<_> => {
  (s, z) => n(s, m(s, z))
}
let church_mul = (n: cnum<_>, m: cnum<_>): cnum<_> => {
  (s, z) => n(z => m(s, z), z)
}
