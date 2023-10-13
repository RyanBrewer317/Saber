import core.{
  App4, Builtin4, Def4, DotAccess4, Downcast4, Expr4, Func4, Global, Ident,
  Ident4, Import4, Int4, Library4, Local, Module4, Stmt4, TDynamic4, TKind4,
  TLabelKind4, TLabelType4, TPi4, TType4, Upcast4, ident_to_str,
}
import monad.{Monad, do, return}
import gleam/list
import gleam/string
import gleam/map.{Map, get, insert}

type Library =
  Library4

type Module =
  Module4

type Stmt =
  Stmt4

type Expr =
  Expr4

pub fn eval_lib(lib: Library) -> Monad(Expr) {
  eval_mod(lib.entry)
}

fn eval_mod(mod: Module) -> Monad(Expr) {
  let assert [s, ..ss] = mod.ast
  use #(b, id) <- do(stmt(s, map.new()))
  use #(res, _) <- do(monad.reduce(ss, #(b, insert(map.new(), id, b)), iteratee))
  return(res)
}

fn iteratee(
  s: Stmt,
  so_far: #(Expr, Map(Ident, Expr)),
) -> Monad(#(Expr, Map(Ident, Expr))) {
  let #(_, heap) = so_far
  use #(val, id) <- do(stmt(s, heap))
  return(#(val, insert(heap, id, val)))
}

fn stmt(s: Stmt, heap: Map(Ident, Expr)) -> Monad(#(Expr, Ident)) {
  case s {
    Def4(_, name, val) -> {
      use val2 <- do(expr(val, insert(heap, Global(name), val)))
      return(#(val2, Global(name)))
    }
    Import4(_, _) -> todo
  }
}

fn expr(e: Expr, heap: Map(Ident, Expr)) -> Monad(Expr) {
  case e {
    Int4(p, i) -> return(Int4(p, i))
    Ident4(p, _, id) ->
      case get(heap, id) {
        Ok(val) -> expr(val, heap)
        Error(Nil) ->
          panic(
            "undefined variable " <> ident_to_str(id) <> " at runtime, " <> string.inspect(
              p,
            ),
          )
      }
    Builtin4(p, t, n) -> {
      use t2 <- do(expr(t, heap))
      return(Builtin4(p, t2, n))
    }
    DotAccess4(_, _, e2, field) -> todo
    // no eta reduction
    Func4(p, t, args, body) -> {
      use t2 <- do(expr(t, heap))
      use #(args2, _) <- do(monad.reduce(
        args,
        #([], heap),
        fn(a, state) {
          let #(args_so_far, heap_so_far) = state
          let #(argid, argt) = a
          use argt2 <- do(expr(argt, heap_so_far))
          return(#([a, ..args_so_far], insert(heap_so_far, Local(argid), argt2)))
        },
      ))
      return(Func4(p, t2, list.reverse(args2), body))
    }
    App4(_, _, func, args) -> {
      use func2 <- do(expr(func, heap))
      use args2 <- do(monad.map(args, expr(_, heap)))
      let assert Func4(_, _, formal_args, body) = func2
      expr(
        body,
        list.fold(
          list.zip(formal_args, args2),
          heap,
          fn(a, b) { map.insert(a, Local({ b.0 }.0), b.1) },
        ),
      )
    }
    Downcast4(_, e, _, _) | Upcast4(_, e, _, _) -> expr(e, heap)
    // no eta reduction
    TPi4(p, args, body) -> {
      use #(args2, _) <- do(monad.reduce(
        args,
        #([], heap),
        fn(a, state) {
          let #(args_so_far, heap_so_far) = state
          let #(argid, argt) = a
          use argt2 <- do(expr(argt, heap_so_far))
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
