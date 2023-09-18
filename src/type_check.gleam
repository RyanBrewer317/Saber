import monad.{Monad, do, return}
import core.{
  App2, App3, Def2, Def3, Downcast3, Expr2, Expr3, Id, Ident2, Ident3, Int2,
  Int3, Lam2, Lam3, Stmt2, Stmt3, TApp2, TApp3, TConstr2, TConstr3, TDynamic2,
  TDynamic3, TForall2, TForall3, TFuncType2, TFuncType3, TLam2, TLam3, TVar2,
  TVar3, Type2, Type3, Upcast3,
}
import gleam/map.{Map, get, insert}
import gleam/int.{to_string}
import gleam/list
import gleam/result

pub fn iteratee(s: Stmt2, so_far: #(List(Stmt3), Map(Id, Type3))) {
  let #(ast, gamma) = so_far
  use #(s2, id, t) <- do(stmt(s, gamma))
  return(#([s2, ..ast], insert(gamma, id, t)))
}

fn stmt(s: Stmt2, types: Map(Id, Type3)) -> Monad(#(Stmt3, Id, Type3)) {
  case s {
    Def2(id, val) -> {
      use val2 <- do(expr(val, [], types))
      return(#(Def3(id, val2), id, typeof(val2)))
    }
  }
}

fn expr(e: Expr2, typevars: List(Id), gamma: Map(Id, Type3)) -> Monad(Expr3) {
  case e {
    Int2(p, i) -> return(Int3(p, i))
    Ident2(p, id) ->
      case get(gamma, id) {
        Ok(t) -> return(Ident3(p, t, id))
        Error(Nil) ->
          panic(
            "undefined variable " <> to_string(id) <> " during typechecking",
          )
      }
    Lam2(p, arg_id, argt, body) -> {
      use t2 <- do(typ(argt))
      use body2 <- do(expr(body, typevars, insert(gamma, arg_id, t2)))
      return(Lam3(p, TFuncType3(t2, typeof(body2)), arg_id, t2, body2))
    }
    TLam2(p, arg_id, body) -> {
      use body2 <- do(expr(body, [arg_id, ..typevars], gamma))
      return(TLam3(p, TForall3(arg_id, typeof(body2)), arg_id, body2))
    }
    App2(p, foo, bar) -> {
      use foo2 <- do(expr(foo, typevars, gamma))
      case typeof(foo2) {
        TFuncType3(a, b) -> {
          use bar2 <- do(expr(bar, typevars, gamma))
          use bar3 <- do(simplify(Downcast3(p, Upcast3(p, bar2, typeof(bar2), TDynamic3), TDynamic3, a)))
          return(App3(p, b, foo2, bar3))
        }
        TDynamic3 -> {
          use bar2 <- do(expr(bar, typevars, gamma))
          return(App3(p, TDynamic3, Downcast3(p, foo2, TDynamic3, TFuncType3(typeof(bar2), TDynamic3)), bar2))
        }
        t -> monad.fail(monad.CallingNonFunction(t))
      }
    }
    TApp2(p, foo, bar) -> {
      use foo2 <- do(expr(foo, typevars, gamma))
      case typeof(foo2) {
        TForall3(arg, body) -> {
          use bar2 <- do(typ(bar))
          return(TApp3(p, substitute(arg, bar2, body), foo2, bar2))
        }
        t -> monad.fail(monad.CallingNonForall(t))
      }
    }
  }
}

fn substitute(id: Id, new: Type3, t: Type3) -> Type3 {
  case t {
    TVar3(x) if id == x -> new
    TVar3(_) -> t
    TConstr3(s, ts) -> TConstr3(s, list.map(ts, substitute(id, new, _)))
    TForall3(arg, _) if arg == id ->
      panic("same id used for different variables")
    TForall3(arg, body) -> TForall3(arg, substitute(id, new, body))
    TFuncType3(a, b) ->
      TFuncType3(substitute(id, new, a), substitute(id, new, b))
    TDynamic3 -> TDynamic3
  }
}

fn typeof(e: Expr3) -> Type3 {
  case e {
    Int3(_, _) -> TConstr3("Int", [])
    Ident3(_, t, _) -> t
    Lam3(_, t, _, _, _) -> t
    TLam3(_, t, _, _) -> t
    App3(_, t, _, _) -> t
    TApp3(_, t, _, _) -> t
    Downcast3(_, _, _, t) -> t
    Upcast3(_, _, _, t) -> t
  }
  // todo-- Girard's paradox
}

fn typ(t: Type2) -> Monad(Type3) {
  case t {
    TVar2(x) -> return(TVar3(x))
    TConstr2(s, ts) -> {
      use ts2 <- do(monad.map(ts, typ))
      return(TConstr3(s, ts2))
    }
    TForall2(arg, body) -> {
      use body2 <- do(typ(body))
      return(TForall3(arg, body2))
    }
    TFuncType2(a, b) -> {
      use a2 <- do(typ(a))
      use b2 <- do(typ(b))
      return(TFuncType3(a2, b2))
    }
    TDynamic2 -> return(TDynamic3)
  }
}

fn simplify(expr: Expr3) -> Monad(Expr3) {
  case expr {
    Downcast3(_, e, t1, t2) if t1 == t2 -> simplify(e)
    Upcast3(_, e, t1, t2) if t1 == t2 -> simplify(e)
    Downcast3(p, Downcast3(_, e, t1, t2), t3, t4) if t2 == t3 ->
      simplify(Downcast3(p, e, t1, t4))
    Upcast3(p, Upcast3(_, e, t1, t2), t3, t4) if t2 == t3 ->
      simplify(Upcast3(p, e, t1, t4))
    Downcast3(p, Upcast3(_, e, t1, t2), t3, t4) if t2 == t3 ->
      case subtype(t1, t4) {
        Ok(t) -> {
          simplify(Upcast3(p, Downcast3(p, e, t1, t), t, t4))
        }
        Error(Nil) -> monad.fail(monad.TypeError(t1, t4))
      }
    e -> return(e)
  }
}

fn subtype(t1: Type3, t2: Type3) -> Result(Type3, Nil) {
  case t1, t2 {
    t1, t2 if t1 == t2 -> Ok(t1)
    TForall3(arg1, body1), TForall3(arg2, _) -> 
      case TForall3(arg2, substitute(arg1, TVar3(arg2), body1)) == t2 {
        True -> Ok(t1)
        False -> Error(Nil)
      }
    TDynamic3, _ -> Ok(t2)
    _, TDynamic3 -> Ok(t1)
    TFuncType3(u1, u2), TFuncType3(u3, u4) -> {
      use sup <- result.try(supertype(u1, u3))
      use sub <- result.try(subtype(u2, u4))
      Ok(TFuncType3(sup, sub))
    }
    _, _ -> Error(Nil)
  }
}

fn supertype(t1: Type3, t2: Type3) -> Result(Type3, Nil) {
  case t1, t2 {
    t1, t2 if t1 == t2 -> Ok(t1)
    TDynamic3, _ -> Ok(t1)
    _, TDynamic3 -> Ok(t2)
    TFuncType3(u1, u2), TFuncType3(u3, u4) -> {
      use sub <- result.try(subtype(u1, u3))
      use sup <- result.try(supertype(u2, u4))
      Ok(TFuncType3(sub, sup))
    }
    _, _ -> Error(Nil)
  }
}
