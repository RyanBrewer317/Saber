import core.{
  App3, App4, Builtin3, Builtin4, Def3, Def4, Downcast3, Downcast4, Expr3, Expr4,
  Func3, Func4, Ident3, Ident4, Int3, Int4, Stmt3, Stmt4, TDynamic3, TDynamic4,
  TKind3, TKind4, TLabelKind3, TLabelKind4, TLabelType3, TLabelType4, TPi3, TPi4,
  TType3, TType4, Upcast3, Upcast4,
}
import monad.{Monad, do, return}
import gleam/list

pub fn iteratee(s: Stmt3, so_far: #(List(Stmt4))) -> Monad(#(List(Stmt4))) {
  use s2 <- do(stmt(s))
  let #(so_far2) = so_far
  return(#([s2, ..so_far2]))
}

pub fn stmt(s: Stmt3) -> Monad(Stmt4) {
  case s {
    Def3(p, id, e) ->
      expr(e)
      |> monad.fmap(Def4(p, id, _))
  }
}

fn expr(e: Expr3) -> Monad(Expr4) {
  case e {
    Int3(p, i) -> return(Int4(p, i))
    Ident3(p, t, id) ->
      expr(t)
      |> monad.fmap(Ident4(p, _, id))
    Builtin3(p, t, name) ->
      expr(t)
      |> monad.fmap(Builtin4(p, _, name))
    Func3(p, t, imp_args, args, body) -> {
      use t2 <- do(expr(t))
      let imp_args2 = list.map(imp_args, fn(a) { #(a, TType4(p)) })
      use args2 <- do(monad.map(
        args,
        fn(a) {
          expr(a.1)
          |> monad.fmap(fn(t) { #(a.0, t) })
        },
      ))
      let args3 = list.append(imp_args2, args2)
      use body2 <- do(expr(body))
      return(Func4(p, t2, args3, body2))
    }
    App3(p, t, func, args) -> {
      use func2 <- do(expr(func))
      use t2 <- do(expr(t))
      use args2 <- do(monad.map(args, expr))
      return(App4(p, t2, func2, args2))
    }
    TPi3(p, imp_args, args, body) -> {
      let imp_args2 = list.map(imp_args, fn(a) { #(a, TType4(p)) })
      use args2 <- do(monad.map(
        args,
        fn(a) {
          expr(a.1)
          |> monad.fmap(fn(t) { #(a.0, t) })
        },
      ))
      let args3 = list.append(imp_args2, args2)
      use body2 <- do(expr(body))
      return(TPi4(p, args3, body2))
    }
    Downcast3(p, e, to, from) -> {
      use e2 <- do(expr(e))
      use to2 <- do(expr(to))
      use from2 <- do(expr(from))
      return(Downcast4(p, e2, to2, from2))
    }
    Upcast3(p, e, to, from) -> {
      use e2 <- do(expr(e))
      use to2 <- do(expr(to))
      use from2 <- do(expr(from))
      return(Upcast4(p, e2, to2, from2))
    }
    TDynamic3(p) -> return(TDynamic4(p))
    TType3(p) -> return(TType4(p))
    TKind3(p) -> return(TKind4(p))
    TLabelType3(p) -> return(TLabelType4(p))
    TLabelKind3(p) -> return(TLabelKind4(p))
  }
}
