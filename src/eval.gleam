import core.{
  App4, Builtin4, Def4, Downcast4, Expr4, Func4, Global, Ident, Ident4, Import4,
  Int4, Library4, Local, Module4, ModuleAccess4, Stmt4, TDynamic4, TKind4,
  TLabelKind4, TLabelType4, TPi4, TType4, Upcast4, ident_to_str,
}
import monad.{Monad, do, return}
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

pub fn eval_lib(lib: Library) -> Monad(Nil) {
  use _ <- do(eval_mod(lib.entry))
  return(Nil)
}

fn eval_mod(mod: Module) -> Monad(Map(Ident, Expr4)) {
  let assert [s, ..ss] = mod.ast
  use start_heap <- do(stmt(s, mod, map.new()))
  use heap <- do(monad.reduce(
    ss,
    start_heap,
    fn(s, heap) {
      use heap2 <- do(stmt(s, mod, heap))
      return(map.merge(heap2, heap))
    },
  ))
  return(heap)
}

fn stmt(s: Stmt, mod: Module, heap: Map(Ident, Expr)) -> Monad(Map(Ident, Expr)) {
  case s {
    Def4(_, name, val) -> {
      use val2 <- do(expr(val, mod, insert(heap, Global(name), val)))
      return(insert(map.new(), Global(name), val2))
    }
    Import4(_, name) -> {
      use heap2 <- do(eval_mod(
        mod.subs
        |> list.find(fn(m) { m.path == mod.path <> "/" <> name })
        |> result.lazy_unwrap(fn() { panic("module not found") }),
      ))
      return(heap2)
    }
  }
}

fn expr(e: Expr, mod: Module, heap: Map(Ident, Expr)) -> Monad(Expr) {
  // io.debug(e)
  case e {
    Int4(p, i) -> return(Int4(p, i))
    Ident4(p, _, id) ->
      case get(heap, id) {
        Ok(val) -> expr(val, mod, heap)
        Error(Nil) ->
          panic(
            "undefined variable " <> ident_to_str(id) <> " at runtime, " <> string.inspect(
              p,
            ),
          )
      }
    Builtin4(p, t, n) -> {
      use t2 <- do(expr(t, mod, heap))
      return(Builtin4(p, t2, n))
    }
    ModuleAccess4(_, _, path, field) -> {
      let sub =
        mod.subs
        |> list.find(fn(m) { m.path == path })
        |> result.lazy_unwrap(fn() { panic("module not found") })
      use sub_defs <- do(eval_mod(sub))
      let assert Ok(val) = map.get(sub_defs, Global(field))
      return(val)
    }
    // no eta reduction
    Func4(p, t, args, body) -> {
      use t2 <- do(expr(t, mod, heap))
      use #(args2, _) <- do(monad.reduce(
        args,
        #([], heap),
        fn(a, state) {
          let #(args_so_far, heap_so_far) = state
          let #(argid, argt) = a
          use argt2 <- do(expr(argt, mod, heap_so_far))
          return(#([a, ..args_so_far], insert(heap_so_far, Local(argid), argt2)))
        },
      ))
      return(Func4(p, t2, list.reverse(args2), body))
    }
    App4(_, _, func, args) -> {
      use func2 <- do(expr(func, mod, heap))
      use args2 <- do(monad.map(args, expr(_, mod, heap)))
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
          )
        Builtin4(_, _, "print") -> {
          use args3 <- do(monad.map(args, expr(_, mod, heap)))
          let assert [Int4(p, i)] = args3
          io.println(string.inspect(i))
          return(Int4(p, i))
        }
        _ -> panic("application of non-function")
      }
    }
    Downcast4(_, e, _, _) | Upcast4(_, e, _, _) -> expr(e, mod, heap)
    // no eta reduction
    TPi4(p, args, body) -> {
      use #(args2, _) <- do(monad.reduce(
        args,
        #([], heap),
        fn(a, state) {
          let #(args_so_far, heap_so_far) = state
          let #(argid, argt) = a
          use argt2 <- do(expr(argt, mod, heap_so_far))
          return(#([a, ..args_so_far], insert(heap_so_far, Local(argid), argt2)))
        },
      ))
      return(TPi4(p, list.reverse(args2), body))
    }
    TDynamic4(_) -> return(e)
    TType4(_) -> return(e)
    TKind4(_) -> return(e)
    TLabelType4(_) -> return(e)
    TLabelKind4(_) -> return(e)
  }
}
