let assoc = (x, env) => {
  let val = env->List.getAssoc(x, (k, item) => k == item)
  switch val {
  | Some(value) => value
  | None => assert(false)
  }
}

let get_fresh_name = (name: string) => {
  let name = ref(name)
  let index = ref(0)
  () => {
    index := index.contents + 1
    name.contents ++ Js.Int.toString(index.contents)
  }
}

let test = (v1, v2) => {
  if v1 === v2 {
    Js.log("success")
  } else {
    Js.log("failure: v1: " ++ Js.Int.toString(v1) ++ ", v2: " ++ Js.Int.toString(v2))
  }
}
