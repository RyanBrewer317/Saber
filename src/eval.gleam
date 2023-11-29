import core.{
  type Expr3, type Ident, type Library3, type Module3, type Monad, type State,
  type Stmt3, App3, Builtin3, Def3, Func3, Global, Ident3, Import3, Int3, Inter3,
  Local, Projection3, TInter3, TKind3, TPi3, TType3, do, ident_to_str,
  monadic_fold, monadic_map, return,
}
import gleam/list
import gleam/string
import gleam/result
import gleam/io
import gleam/map.{type Map, get, insert}

type Library =
  Library3

type Module =
  Module3

type Stmt =
  Stmt3

type Expr =
  Expr3

pub fn eval_lib(lib: Library, state: State) -> Monad(Nil) {
  use defs, state <- do(eval_mod(lib.entry, state))
  case get(defs, Global("main")) {
    Ok(Func3(_, _, [], body)) -> {
      use _, state <- do(expr(body, lib.entry, defs, Error(Nil), state))
      return(Nil, state)
    }
    Ok(_) -> panic("")
    Error(Nil) -> return(Nil, state)
  }
}

fn eval_mod(mod: Module, state: State) -> Monad(Map(Ident, Expr3)) {
  let assert [s, ..ss] = mod.ast
  use start_heap, state <- do(stmt(s, mod, map.new(), state))
  use heap, state <- monadic_fold(
    ss,
    start_heap,
    state,
    fn(heap, s, state) {
      use heap2, state <- do(stmt(s, mod, heap, state))
      return(map.merge(heap2, heap), state)
    },
  )
  return(heap, state)
}

fn stmt(
  s: Stmt,
  mod: Module,
  heap: Map(Ident, Expr),
  state: State,
) -> Monad(Map(Ident, Expr)) {
  case s {
    Def3(_, name, t, val) -> {
      use _, state <- do(expr(t, mod, heap, Ok(val), state))
      use val2, state <- do(expr(
        val,
        mod,
        insert(heap, Global(name), val),
        Error(Nil),
        state,
      ))
      return(insert(map.new(), Global(name), val2), state)
    }
    Import3(_, name) -> {
      use heap2, state <- do(eval_mod(
        mod.subs
        |> list.find(fn(m) { m.path == mod.path <> "/" <> name })
        |> result.lazy_unwrap(fn() { panic("module not found") }),
        state,
      ))
      return(heap2, state)
    }
  }
}

fn expr(
  e: Expr,
  mod: Module,
  heap: Map(Ident, Expr),
  mbval: Result(Expr, Nil),
  state: State,
) -> Monad(Expr) {
  // io.println(core.pretty_expr(e))
  // io.debug(e)
  case e {
    Int3(p, i) -> return(Int3(p, i), state)
    Ident3(p, t, id) -> {
      use _, state <- do(expr(t, mod, heap, Ok(e), state))
      case get(heap, id) {
        Ok(val) -> expr(val, mod, heap, mbval, state)
        Error(Nil) ->
          panic(
            "undefined variable " <> ident_to_str(id) <> " at runtime, " <> string.inspect(
              p,
            ),
          )
      }
    }
    Builtin3(p, t, n) -> {
      use t2, state <- do(expr(t, mod, heap, Ok(e), state))
      return(Builtin3(p, t2, n), state)
    }
    // no eta reduction
    Func3(p, t, args, body) -> {
      use t2, state <- do(expr(t, mod, heap, Ok(e), state))
      use #(args2, _), state <- monadic_fold(
        args,
        #([], heap),
        state,
        fn(s, a, state) {
          let #(args_so_far, heap_so_far) = s
          use argt2, state <- do(expr(a.t, mod, heap_so_far, Error(Nil), state))
          return(
            #([a, ..args_so_far], insert(heap_so_far, Local(a.id), argt2)),
            state,
          )
        },
      )
      return(Func3(p, t2, list.reverse(args2), body), state)
    }
    App3(_, _, func, args) -> {
      use func2, state <- do(expr(func, mod, heap, Error(Nil), state))
      use args2, state <- monadic_map(
        args,
        state,
        fn(a, state) { expr(a, mod, heap, Error(Nil), state) },
      )
      case func2 {
        Func3(_, _, formal_args, body) ->
          expr(
            body,
            mod,
            list.fold(
              list.zip(formal_args, args2),
              heap,
              fn(a, b) { map.insert(a, Local({ b.0 }.id), b.1) },
            ),
            Error(Nil),
            state,
          )
        Builtin3(_, _, "print") -> {
          use args3, state <- monadic_map(
            args,
            state,
            fn(a, state) { expr(a, mod, heap, Error(Nil), state) },
          )
          let assert [Int3(p, i)] = args3
          io.println(string.inspect(i))
          return(Int3(p, i), state)
        }
        _ -> panic("application of non-function")
      }
    }
    // no eta reduction
    TPi3(p, args, body) -> {
      use #(args2, _), state <- monadic_fold(
        args,
        #([], heap),
        state,
        fn(s, a, state) {
          let #(args_so_far, heap_so_far) = s
          use argt2, state <- do(expr(a.t, mod, heap_so_far, Error(Nil), state))
          return(
            #([a, ..args_so_far], insert(heap_so_far, Local(a.id), argt2)),
            state,
          )
        },
      )
      return(TPi3(p, list.reverse(args2), body), state)
    }
    TType3(_) -> return(e, state)
    TKind3(_) -> return(e, state)
    TInter3(p, ts) -> {
      return(TInter3(p, ts), state)
    }
    Projection3(_, _, _, _) -> todo
    Inter3(_, _, _) -> todo
  }
  // InterAccess3(p, t, e, field) -> return(e, state) // TODO: eval will be operating on pure lambda terms, not annotated ones
  // type code won't execute unless from a value
}
