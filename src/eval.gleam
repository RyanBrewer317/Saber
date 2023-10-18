import core.{
  App4, Builtin4, Def4, Downcast4, Expr4, Func4, Global, Ident, Ident4, Import4,
  Int4, Library4, Local, Module4, ModuleAccess4, Stmt4, TDynamic4, TKind4,
  TLabelKind4, TLabelType4, TPi4, TType4, Upcast4, ident_to_str,
}
import monad.{Monad, State, do, monadic_fold, monadic_map, return}
import gleam/list
import gleam/string
import gleam/result
import gleam/io
import gleam/map.{Map, get, insert}

type Library =
  Library4

type Module =
  Module4

type Stmt =
  Stmt4

type Expr =
  Expr4

pub fn eval_lib(lib: Library, state: State) -> Monad(Nil) {
  use defs, state2 <- do(eval_mod(lib.entry, state))
  case get(defs, Global("main")) {
    Ok(Func4(_, _, [], body)) -> {
      use _, state3 <- do(expr(body, lib.entry, defs, state2))
      return(Nil, state3)
    }
    Ok(_) -> panic("")
    Error(Nil) -> return(Nil, state2)
  }
}

fn eval_mod(mod: Module, state: State) -> Monad(Map(Ident, Expr4)) {
  let assert [s, ..ss] = mod.ast
  use start_heap, state2 <- do(stmt(s, mod, map.new(), state))
  use heap, state3 <- monadic_fold(
    ss,
    start_heap,
    state2,
    fn(heap, s, statex) {
      use heap2, statex2 <- do(stmt(s, mod, heap, statex))
      return(map.merge(heap2, heap), statex2)
    },
  )
  return(heap, state3)
}

fn stmt(
  s: Stmt,
  mod: Module,
  heap: Map(Ident, Expr),
  state: State,
) -> Monad(Map(Ident, Expr)) {
  case s {
    Def4(_, name, val) -> {
      use val2, state2 <- do(expr(
        val,
        mod,
        insert(heap, Global(name), val),
        state,
      ))
      return(insert(map.new(), Global(name), val2), state2)
    }
    Import4(_, name) -> {
      use heap2, state2 <- do(eval_mod(
        mod.subs
        |> list.find(fn(m) { m.path == mod.path <> "/" <> name })
        |> result.lazy_unwrap(fn() { panic("module not found") }),
        state,
      ))
      return(heap2, state2)
    }
  }
}

fn expr(
  e: Expr,
  mod: Module,
  heap: Map(Ident, Expr),
  state: State,
) -> Monad(Expr) {
  // io.println(core.pretty_expr(e))
  // io.debug(e)
  case e {
    Int4(p, i) -> return(Int4(p, i), state)
    Ident4(p, _, id) ->
      case get(heap, id) {
        Ok(val) -> expr(val, mod, heap, state)
        Error(Nil) ->
          panic(
            "undefined variable " <> ident_to_str(id) <> " at runtime, " <> string.inspect(
              p,
            ),
          )
      }
    Builtin4(p, t, n) -> {
      use t2, state2 <- do(expr(t, mod, heap, state))
      return(Builtin4(p, t2, n), state2)
    }
    ModuleAccess4(_, _, path, field) -> {
      let sub =
        mod.subs
        |> list.find(fn(m) { m.path == path })
        |> result.lazy_unwrap(fn() { panic("module not found") })
      use sub_defs, state2 <- do(eval_mod(sub, state))
      let assert Ok(val) = map.get(sub_defs, Global(field))
      return(val, state2)
    }
    // no eta reduction
    Func4(p, t, args, body) -> {
      use t2, state2 <- do(expr(t, mod, heap, state))
      use #(args2, _), state3 <- monadic_fold(
        args,
        #([], heap),
        state2,
        fn(s, a, statex) {
          let #(args_so_far, heap_so_far) = s
          let #(argid, argt) = a
          use argt2, statex2 <- do(expr(argt, mod, heap_so_far, statex))
          return(
            #([a, ..args_so_far], insert(heap_so_far, Local(argid), argt2)),
            statex2,
          )
        },
      )
      return(Func4(p, t2, list.reverse(args2), body), state3)
    }
    App4(_, _, func, args) -> {
      use func2, state2 <- do(expr(func, mod, heap, state))
      use args2, state3 <- monadic_map(
        args,
        state2,
        fn(a, statex) { expr(a, mod, heap, statex) },
      )
      case func2 {
        Func4(_, _, formal_args, body) ->
          expr(
            body,
            mod,
            list.fold(
              list.zip(formal_args, args2),
              heap,
              fn(a, b) { map.insert(a, Local({ b.0 }.0), b.1) },
            ),
            state3,
          )
        Builtin4(_, _, "print") -> {
          use args3, state4 <- monadic_map(
            args,
            state3,
            fn(a, statex) { expr(a, mod, heap, statex) },
          )
          let assert [Int4(p, i)] = args3
          io.println(string.inspect(i))
          return(Int4(p, i), state4)
        }
        _ -> panic("application of non-function")
      }
    }
    Downcast4(_, e, _, _) | Upcast4(_, e, _, _) -> expr(e, mod, heap, state)
    // no eta reduction
    TPi4(p, args, body) -> {
      use #(args2, _), state2 <- monadic_fold(
        args,
        #([], heap),
        state,
        fn(s, a, statex) {
          let #(args_so_far, heap_so_far) = s
          let #(argid, argt) = a
          use argt2, statex2 <- do(expr(argt, mod, heap_so_far, statex))
          return(
            #([a, ..args_so_far], insert(heap_so_far, Local(argid), argt2)),
            statex2,
          )
        },
      )
      return(TPi4(p, list.reverse(args2), body), state2)
    }
    TDynamic4(_) -> return(e, state)
    TType4(_) -> return(e, state)
    TKind4(_) -> return(e, state)
    TLabelType4(_) -> return(e, state)
    TLabelKind4(_) -> return(e, state)
  }
}
