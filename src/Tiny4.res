module VM = {
  type label = string
  type instr =
    | Cst(int)
    | Add
    | Mul
    | Var(int)
    | Swap
    | Pop
    | Label(string)
    | Call(string, int) // Call(f, num_of_args)
    | Ret(int) // num of args
    | Goto(label)
    | IfZero(label)
    | Exit
  type instrs = array<instr>

  // Encoding specification
  // |===========================================================================
  // | Instr     | Opcode | Oprand1     | Oprand2 | Size | Desc
  // | Cst(i)    | 0      | i           |         | 2    | 添加 i 到栈顶
  // | Add       | 1      |             |         | 1    | 加法，栈顶前两个元素相加
  // | Mul       | 2      |             |         | 1    | 乘法
  // | Var(i)    | 3      | i           |         | 2    | 引用变量，把距离栈顶深度 i 的值添加到栈顶
  // | Pop       | 4      |             |         | 1    | 丢弃栈顶元素
  // | Swap      | 5      |             |         | 1    | 交换栈顶前两个元素
  // | Call(l,n) | 6      | get_addr(l) | n       | 3    | 函数调用，pc 指向 l
  // | Ret(n)    | 7      | n           |         | 2    | 函数返回
  // | IfZero(l) | 8      | get_addr(l) |         | 2    | if_then_else
  // | Goto(l)   | 9      | get_addr(l) |         | 2    | goto l, pc 指向 l
  // | Exit      | 10     |             |         | 1    | 退出
  // |           |        |             |         |      |
  // |===========================================================================

  let size_of_instr = (instr: instr): int => {
    switch instr {
    | Label(_) => 0
    | Add | Mul | Swap | Pop | Exit => 1
    | Cst(_) | Var(_) | Ret(_) | Goto(_) | IfZero(_) => 2
    | Call(_, _) => 3
    }
  }

  // homework1: assembler
  let encode = (instrs: instrs) => {
    let position = ref(0) // program counter/index to the code_seg
    let label_map: Map.t<string, int> = Map.make()
    let code_seg = Array.make(~length=0xffff, 0)
    // construct the label_map
    for cur in 0 to Array.length(instrs) - 1 {
      switch instrs[cur] {
      | Some(Label(l)) => label_map->Map.set(l, position.contents)
      | Some(instr) => position := position.contents + size_of_instr(instr)
      | None => ()
      }
    }
    position := 0
    for cur in 0 to Array.length(instrs) - 1 {
      switch instrs[cur] {
      | Some(instr) =>
        switch instr {
        | Cst(i) => {
            code_seg->Array.set(position.contents, 0) // opcode of Cst is 0
            code_seg->Array.set(position.contents + 1, i)
            position := position.contents + 2
          }
        | Add => {
            code_seg->Array.set(position.contents, 1)
            position := position.contents + 1
          }
        | Mul => {
            code_seg->Array.set(position.contents, 2)
            position := position.contents + 1
          }
        | Pop => {
            code_seg->Array.set(position.contents, 4)
            position := position.contents + 1
          }
        | Swap => {
            code_seg->Array.set(position.contents, 5)
            position := position.contents + 1
          }
        | Exit => {
            code_seg->Array.set(position.contents, 10)
            position := position.contents + 1
          }
        | Var(i) => {
            code_seg->Array.set(position.contents, 3)
            code_seg->Array.set(position.contents + 1, i)
            position := position.contents + 2
          }
        | Label(_) => ()
        | Call(l, n) => {
            let label_addr = label_map->Map.get(l)
            switch label_addr {
            | Some(addr) => {
                code_seg->Array.set(position.contents, 6)
                code_seg->Array.set(position.contents + 1, addr)
                code_seg->Array.set(position.contents + 2, n)
                position := position.contents + 3
              }
            | None => assert(false)
            }
          }
        | Ret(i) => {
            code_seg->Array.set(position.contents, 7)
            code_seg->Array.set(position.contents + 1, i)
            position := position.contents + 2
          }
        | Goto(l) => {
            let label_addr = label_map->Map.get(l)
            switch label_addr {
            | Some(addr) => {
                code_seg->Array.set(position.contents, 9)
                code_seg->Array.set(position.contents + 1, addr)
                position := position.contents + 2
              }
            | None => assert(false)
            }
          }
        | IfZero(l) => {
            let label_addr = label_map->Map.get(l)
            switch label_addr {
            | Some(addr) => {
                code_seg->Array.set(position.contents, 8)
                code_seg->Array.set(position.contents + 1, addr)
                position := position.contents + 2
              }
            | None => assert(false)
            }
          }
        }
      | None => ()
      }
    }
    code_seg
  }

  let decode = (bytecode: array<int>, len: int): instrs => {
    let instrs = []
    let arr_get = (arr, idx) => {
      switch arr[idx] {
      | Some(v) => v
      | None => assert(false)
      }
    }
    let idx = ref(0)
    let break = ref(false)
    while !break.contents {
      let opcode = arr_get(bytecode, idx.contents)
      switch opcode {
      | 0 => {
          let i = arr_get(bytecode, idx.contents + 1)
          instrs->Array.push(Cst(i))
          idx := idx.contents + 2
        }
      | 1 => {
          instrs->Array.push(Add)
          idx := idx.contents + 1
        }
      | 2 => {
          instrs->Array.push(Mul)
          idx := idx.contents + 1
        }
      | 3 => {
          let i = arr_get(bytecode, idx.contents + 1)
          instrs->Array.push(Var(i))
          idx := idx.contents + 2
        }
      | 4 => {
          instrs->Array.push(Pop)
          idx := idx.contents + 1
        }
      | 5 => {
          instrs->Array.push(Swap)
          idx := idx.contents + 1
        }
      | 6 => {
          let addr = arr_get(bytecode, idx.contents + 1)
          let arity = arr_get(bytecode, idx.contents + 2)
          instrs->Array.push(Call(Js.Int.toString(addr), arity))
          idx := idx.contents + 3
        }
      | 7 => {
          let arity = arr_get(bytecode, idx.contents + 1)
          instrs->Array.push(Ret(arity))
          idx := idx.contents + 2
        }
      | 8 => {
          let addr = arr_get(bytecode, idx.contents + 1)
          instrs->Array.push(IfZero(Js.Int.toString(addr)))
          idx := idx.contents + 2
        }
      | 9 => {
          let addr = arr_get(bytecode, idx.contents + 1)
          instrs->Array.push(Goto(Js.Int.toString(addr)))
        }
      | 10 => {
          instrs->Array.push(Exit)
          idx := idx.contents + 1
        }
      | _ => assert(false)
      }
      break := idx.contents >= len
    }
    instrs
  }

  type operand = int
  type vm = {
    code: array<int>, // code segment immutable
    stack: array<operand>, // runtime stack
    mutable pc: int, // pc register
    mutable sp: int, // sp register
  }

  // stack operators
  let push = (vm: vm, x: operand) => {
    vm.stack[vm.sp] = x
    vm.sp = vm.sp + 1
  }

  let pop = (vm: vm): operand => {
    vm.sp = vm.sp - 1
    switch vm.stack[vm.sp] {
    | Some(val) => val
    | None => assert(false)
    }
  }

  let arr_get = (arr, idx) => {
    switch arr[idx] {
    | Some(v) => v
    | None => assert(false)
    }
  }

  // initail state
  let init_stack = length => {
    Array.make(~length, 0)
  }

  let initVm = (code, pc) => {
    code,
    stack: init_stack(100),
    pc,
    sp: 0,
  }

  let get_init_pc = (code: array<instr>) => {
    Js.Array.findIndex(i => {i == Call("main", 0)}, code)
  }

  // homework2: 使用C/C++/Rust实现支持课程中使用的指令的虚拟机
  // https://github.com/downhill6/stackmachinevm
  let run = (vm: vm): operand => {
    let break = ref(false)
    while !break.contents {
      let opcode = arr_get(vm.code, vm.pc)
      switch opcode {
      | 0 => {
          // Cst(i)
          let i = arr_get(vm.code, vm.pc + 1)
          push(vm, i)
          vm.pc = vm.pc + 2
        }
      | 1 => {
          // Add
          let a = pop(vm)
          let b = pop(vm)
          push(vm, a + b)
          vm.pc = vm.pc + 1
        }
      | 2 => {
          // Mul
          let a = pop(vm)
          let b = pop(vm)
          push(vm, a * b)
          vm.pc = vm.pc + 1
        }
      | 3 => {
          // Var(i)
          let i = arr_get(vm.code, vm.pc + 1)
          let var = arr_get(vm.stack, vm.sp - i - 1)
          push(vm, var)
          vm.pc = vm.pc + 2
        }
      | 4 => {
          // Pop
          let _ = pop(vm)
          vm.pc = vm.pc + 1
        }
      | 5 => {
          // Swap
          let a = pop(vm)
          let b = pop(vm)
          push(vm, a)
          push(vm, b)
          vm.pc = vm.pc + 1
        }
      | 6 => {
          // Call(addr, arity)
          let target_pc = arr_get(vm.code, vm.pc + 1)
          let arity = arr_get(vm.code, vm.pc + 2)
          let next_pc = target_pc
          // 保存 Call 的下一个 pc 位置（也就是 return address)
          vm.stack->Array.splice(~start=vm.sp - arity, ~remove=0, ~insert=[vm.pc + 3])
          vm.sp = vm.sp + 1

          // 跳转到 addr 执行函数体
          vm.pc = next_pc
        }
      | 7 => {
          // Ret(arity)
          let arity = arr_get(vm.code, vm.pc + 1)
          let res = pop(vm)
          vm.sp = vm.sp - arity
          let next_pc = pop(vm)
          push(vm, res)
          vm.pc = next_pc
        }
      | 8 => {
          // IfZero(addr)
          let to = arr_get(vm.code, vm.pc + 1)
          let cond = pop(vm)
          if cond === 0 {
            vm.pc = to
          } else {
            vm.pc = vm.pc + 2
          }
        }
      | 9 => {
          // GOTO(addr)
          let to = arr_get(vm.code, vm.pc + 1)
          vm.pc = to
        }
      | 10 => break := true // Exit
      | _ => assert(false)
      }
    }
    pop(vm)
  }
}

// homework3: 实现 Compiler
module Compiler = {
  type prim = Add | Mul | Self
  type rec expr =
    | Cst(int)
    | Var(string)
    | Let(string, expr, expr)
    | Letfn(string, list<string>, expr, expr) // fnName params body let-scope
    | App(string, list<expr>) // fnName args
    | Prim(prim, list<expr>)
    | If(expr, expr, expr)

  // 预处理后的程序
  module Flat = {
    type rec expr =
      | Cst(int)
      | Var(string)
      | Let(string, expr, expr)
      | App(string, list<expr>)
      | Prim(prim, list<expr>)
      | If(expr, expr, expr)
    type fun = (string, list<string>, expr)
    type prog = list<fun>
  }

  // compile functions
  type var = Local(string) | Temp // 参数和本地变量当做一回事
  type env = list<var>

  // Auxiliary functions
  let rec remove_funs = (expr: expr): Flat.expr => {
    switch expr {
    | Cst(i) => Cst(i)
    | Var(s) => Var(s)
    | Let(s, e1, e2) => Let(s, remove_funs(e1), remove_funs(e2))
    | App(s, es) => App(s, List.map(es, e => remove_funs(e)))
    | Prim(prim, es) => Prim(prim, List.map(es, e => remove_funs(e)))
    | If(e1, e2, e3) => If(remove_funs(e1), remove_funs(e2), remove_funs(e3))
    | Letfn(_, _, _, scope) => remove_funs(scope)
    }
  }

  let rec collect_funs = (expr: expr): Flat.prog => {
    switch expr {
    | Cst(_) => list{}
    | Var(_) => list{}
    | Let(_, e1, e2) => List.concat(collect_funs(e1), collect_funs(e2))
    | Letfn(name, params, body, scope) =>
      List.concatMany([
        list{(name, params, remove_funs(body))},
        collect_funs(body),
        collect_funs(scope),
      ])
    | App(_, args) => List.flatten(List.map(args, collect_funs))
    | Prim(_, es) => List.flatten(List.map(es, collect_funs))
    | If(e1, e2, e3) => List.concatMany([collect_funs(e1), collect_funs(e2), collect_funs(e3)])
    }
  }

  let sindex = (env: env, s) => {
    let rec go = (env, acc) => {
      switch env {
      | list{} => raise(Not_found)
      | list{Local(x), ...rest} =>
        if x == s {
          acc
        } else {
          go(rest, acc + 1)
        }
      | list{Temp, ...rest} => go(rest, acc + 1)
      }
    }
    go(env, 0)
  }

  let fresh_else = Util.get_fresh_name("else")
  let fresh_exit = Util.get_fresh_name("exit")

  // preprocessing function to collect function definitions
  let preprocess = (expr: expr): Flat.prog => {
    let main = ("main", list{}, remove_funs(expr))
    let rest = collect_funs(expr)
    // list{main, ...rest}
    List.concat(list{main}, rest)
  }

  // compile expression under a compile-time environment
  let rec compile_expr = (env: env, expr: Flat.expr, name: string, num: int): list<VM.instr> => {
    switch expr {
    | Cst(i) => list{Cst(i)}
    | Var(s) => list{Var(sindex(env, s))}
    | Let(s, e1, e2) =>
      List.concatMany([
        compile_expr(env, e1, name, num),
        compile_expr(list{Local(s), ...env}, e2, name, num),
        list{VM.Swap, VM.Pop},
      ])
    | App(name, args) => {
        let n = List.length(args)
        let args_code = compile_exprs(env, args, name, n)
        list{...args_code, VM.Call(name, n)}
      }
    | Prim(op, es) => {
        let es_code = compile_exprs(env, es, name, num)
        let op_code = switch op {
        | Add => VM.Add
        | Mul => VM.Mul
        | Self => Call(name, num) // homework4: 支持递归函数
        }
        List.concat(es_code, list{op_code})
      }
    | If(cond, then_body, else_body) => {
        let label_else = fresh_else()
        let label_exit = fresh_exit()
        List.concatMany([
          compile_expr(env, cond, name, num),
          list{IfZero(label_else)},
          compile_expr(env, then_body, name, num),
          list{Goto(label_exit)},
          list{Label(label_else)},
          compile_expr(env, else_body, name, num),
          list{Label(label_exit)},
        ])
      }
    }
  }

  and compile_exprs = (env: env, exprs: list<Flat.expr>, name: string, num: int) => {
    let rec compile_exprs_aux = (env, exprs, acc) => {
      switch exprs {
      | list{} => acc
      | list{expr, ...rest} => {
          let expr_code = compile_expr(env, expr, name, num)
          compile_exprs_aux(list{Temp, ...env}, rest, List.concat(acc, expr_code))
        }
      }
    }
    compile_exprs_aux(env, exprs, list{})
  }

  let compile_fun = ((name, args, body): Flat.fun): list<VM.instr> => {
    let n = List.length(args)
    let env = List.reverse(List.map(args, a => Local(a)))
    list{VM.Label(name), ...compile_expr(env, body, name, n), Ret(n)}
  }

  // compile the whole program
  let compile = (prog: Flat.prog): list<VM.instr> => {
    // translate each function to machine code
    let funs_code = prog->List.map(compile_fun)->List.flatten
    // add entrance and exit
    list{Call("main", 0), Exit, ...funs_code}
  }

  let compile_encode = prog => {
    let vmcode = prog->preprocess->compile->List.toArray
    Js.log(vmcode)
    let encoded = VM.encode(vmcode)
    encoded
  }
}

// driver
let compile_encode_and_execute = (program: Compiler.expr) => {
  let vmcode = program->Compiler.preprocess->Compiler.compile->List.toArray
  let encoded = VM.encode(vmcode)
  let init_pc = VM.get_init_pc(vmcode)
  let real_vm = VM.initVm(encoded, init_pc)
  VM.run(real_vm)
}

module Test = {
  open Compiler

  let fact = e => Letfn(
    "fact",
    list{"n"},
    If(
      Var("n"),
      Prim(Mul, list{Var("n"), Prim(Self, list{Prim(Add, list{Var("n"), Cst(-1)})})}),
      Cst(1),
    ),
    e,
  )

  let fib = e => Letfn(
    "fib",
    list{"n"},
    If(
      Var("n"),
      If(
        Prim(Add, list{Var("n"), Cst(-1)}),
        Prim(
          Add,
          list{
            Prim(Self, list{Prim(Add, list{Var("n"), Cst(-1)})}),
            Prim(Self, list{Prim(Add, list{Var("n"), Cst(-2)})}),
          },
        ),
        Cst(1),
      ),
      Cst(1),
    ),
    e,
  )

  let fact_tail = e => Letfn(
    "fact_tail",
    list{"n", "acc"},
    If(
      Var("n"),
      Prim(Self, list{Prim(Add, list{Var("n"), Cst(-1)}), Prim(Mul, list{Var("n"), Var("acc")})}),
      Var("acc"),
    ),
    Letfn("fact", list{"n"}, App("fact_tail", list{Var("n"), Cst(1)}), e),
  )

  let cst_test = Cst(1)
  let add_test = Prim(Add, list{Cst(1), Cst(2)})
  let mul_test = Prim(Mul, list{Cst(2), Cst(3)})
  let let_test = Let("x", Cst(3), Prim(Mul, list{Cst(3), Var("x")}))
  let if_test1 = If(Cst(0), add_test, mul_test)
  let if_test2 = If(Cst(1), add_test, mul_test)
  let call_test1 = Letfn("f", list{}, Cst(10), App("f", list{}))
  let call_test2 = Letfn(
    "f",
    list{"x", "y"},
    Prim(Add, list{Var("x"), Var("y")}),
    App("f", list{Cst(2), Cst(3)}),
  )
  let test_fact = fact(App("fact", list{Cst(5)}))
  let test_fib = fib(App("fib", list{Cst(7)}))
  let test_fact_tail = fact_tail(
    Let(
      "a",
      Cst(5),
      Letfn("id", list{"x"}, Var("x"), App("id", list{App("fact", list{Var("a")})})),
    ),
  )

  Util.test(compile_encode_and_execute(cst_test), 1)
  Util.test(compile_encode_and_execute(add_test), 3)
  Util.test(compile_encode_and_execute(mul_test), 6)
  Util.test(compile_encode_and_execute(let_test), 9)
  Util.test(compile_encode_and_execute(if_test1), 6)
  Util.test(compile_encode_and_execute(if_test2), 3)
  Util.test(compile_encode_and_execute(call_test1), 10)
  Util.test(compile_encode_and_execute(call_test2), 5)
  Util.test(compile_encode_and_execute(test_fact), 120)
  Util.test(compile_encode_and_execute(test_fib), 21)
  Util.test(compile_encode_and_execute(test_fact_tail), 120)
}
