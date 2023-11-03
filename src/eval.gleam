import core.{
  App4, Builtin4, Def4, Downcast4, Expr4, Func4, Global, Ident, Ident4, Import4,
  Int4, Library4, Local, Module4, ModuleAccess4, Stmt4, Struct4, StructAccess4,
  TDynamic4, TInter4, TKind4, TPi4, TStruct4, TType4, Upcast4, ident_to_str,
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
  use defs, state <- do(eval_mod(lib.entry, state))
  case get(defs, Global("main")) {
    Ok(Func4(_, _, [], body)) -> {
      use _, state <- do(expr(body, lib.entry, defs, Error(Nil), state))
      return(Nil, state)
    }
    Ok(_) -> panic("")
    Error(Nil) -> return(Nil, state)
  }
}

fn eval_mod(mod: Module, state: State) -> Monad(Map(Ident, Expr4)) {
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
    Def4(_, name, t, val) -> {
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
    Import4(_, name) -> {
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
    Int4(p, i) -> return(Int4(p, i), state)
    Ident4(p, t, id) -> {
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
    Builtin4(p, t, n) -> {
      use t2, state <- do(expr(t, mod, heap, Ok(e), state))
      return(Builtin4(p, t2, n), state)
    }
    ModuleAccess4(_, t, path, field) -> {
      use _, state <- do(expr(t, mod, heap, Ok(e), state))
      let sub =
        mod.subs
        |> list.find(fn(m) { m.path == path })
        |> result.lazy_unwrap(fn() { panic("module not found") })
      use sub_defs, state <- do(eval_mod(sub, state))
      let assert Ok(val) = map.get(sub_defs, Global(field))
      return(val, state)
    }
    StructAccess4(_, _, e, field) -> {
      use e2, state <- do(expr(e, mod, heap, Error(Nil), state))
      let assert Struct4(_, _, fields) = e2
      let assert Ok(#(_, val)) = list.find(fields, fn(f) { f.0 == field })
      return(val, state)
    }
    // no eta reduction
    Func4(p, t, args, body) -> {
      use t2, state <- do(expr(t, mod, heap, Ok(e), state))
      use #(args2, _), state <- monadic_fold(
        args,
        #([], heap),
        state,
        fn(s, a, state) {
          let #(args_so_far, heap_so_far) = s
          let #(argid, argt) = a
          use argt2, state <- do(expr(argt, mod, heap_so_far, Error(Nil), state))
          return(
            #([a, ..args_so_far], insert(heap_so_far, Local(argid), argt2)),
            state,
          )
        },
      )
      return(Func4(p, t2, list.reverse(args2), body), state)
    }
    App4(_, _, func, args) -> {
      use func2, state <- do(expr(func, mod, heap, Error(Nil), state))
      use args2, state <- monadic_map(
        args,
        state,
        fn(a, state) { expr(a, mod, heap, Error(Nil), state) },
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
            Error(Nil),
            state,
          )
        Builtin4(_, _, "print") -> {
          use args3, state <- monadic_map(
            args,
            state,
            fn(a, state) { expr(a, mod, heap, Error(Nil), state) },
          )
          let assert [Int4(p, i)] = args3
          io.println(string.inspect(i))
          return(Int4(p, i), state)
        }
        _ -> panic("application of non-function")
      }
    }
    Downcast4(_, e, _, _) | Upcast4(_, e, _, _) ->
      expr(e, mod, heap, Error(Nil), state)
    // no eta reduction
    TPi4(p, args, body) -> {
      use #(args2, _), state <- monadic_fold(
        args,
        #([], heap),
        state,
        fn(s, a, state) {
          let #(args_so_far, heap_so_far) = s
          let #(argid, argt) = a
          use argt2, state <- do(expr(argt, mod, heap_so_far, Error(Nil), state))
          return(
            #([a, ..args_so_far], insert(heap_so_far, Local(argid), argt2)),
            state,
          )
        },
      )
      return(TPi4(p, list.reverse(args2), body), state)
    }
    TDynamic4(_) -> return(e, state)
    TType4(_) -> return(e, state)
    TKind4(_) -> return(e, state)
    Struct4(p, t, fields) -> {
      use t2, state <- do(expr(t, mod, heap, Ok(e), state))
      use fields2, state <- monadic_map(
        fields,
        state,
        fn(f, state) {
          use val, state <- do(expr(f.1, mod, heap, Error(Nil), state))
          return(#(f.0, val), state)
        },
      )
      return(Struct4(p, t2, fields2), state)
    }
    TStruct4(p, fields) -> {
      use fields2, state <- monadic_map(
        fields,
        state,
        fn(f, state) {
          use t, state <- do(expr(f.1, mod, heap, Error(Nil), state))
          return(#(f.0, t), state)
        },
      )
      return(TStruct4(p, fields2), state)
    }
    TInter4(p, ts) -> {
      return(TInter4(p, ts), state)
    }
  }
  // type code won't execute unless from a value
}
