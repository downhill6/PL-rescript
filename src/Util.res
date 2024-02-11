let assoc = (x, env) => {
  let val = env->List.getAssoc(x, (k, item) => k == item)
  switch val {
  | Some(value) => value
  | None => assert(false)
  }
}
