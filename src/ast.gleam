import core.{Id, Stmt1, Stmt2, Def1, Def2, Expr1, Expr2, Int1, Int2, Ident1, Ident2, Lam1, Lam2, TLam1, TLam2, App1, App2, TApp1, TApp2, TDynamic2, Type1, Type2, TVar1, TVar2, TConstr1, TConstr2, TForall1, TForall2, TFuncType1, TFuncType2}
import monad.{Monad, fresh, do, return, fail, Undefined}
import gleam/map.{Map, get, insert}

pub fn iteratee(s: Stmt1, so_far: #(List(Stmt2), Map(String, Id))) -> Monad(#(List(Stmt2), Map(String, Id))) {
  let #(ast, renames) = so_far
  use #(s2, name, id) <- do(stmt(s, renames))
  return(#([s2, ..ast], insert(renames, name, id)))
}

fn stmt(s: Stmt1, renames: Map(String, Id)) -> Monad(#(Stmt2, String, Id)) {
  case s {
    Def1(name, body) -> {
      use id <- fresh()
      use body2 <- do(expr(body, insert(renames, name, id)))
      return(#(Def2(id, body2), name, id))
    }
  }
}

fn expr(e: Expr1, renames: Map(String, Id)) -> Monad(Expr2) {
  case e {
    Int1(p, i) -> return(Int2(p, i))
    Ident1(p, name) -> 
      case get(renames, name) {
        Ok(id) -> return(Ident2(p, id))
        Error(Nil) -> fail(Undefined(p, name))
      }
    Lam1(p, arg, mb_argt, body) -> {
      use arg_id <- fresh()
      use argt <- do(case mb_argt {
        Ok(t) -> typ(t, renames)
        Error(Nil) -> return(TDynamic2)
      })
      use body2 <- do(expr(body, insert(renames, arg, arg_id)))
      return(Lam2(p, arg_id, argt, body2))
    }
    TLam1(p, arg, body) -> {
      use arg_id <- fresh()
      use body2 <- do(expr(body, insert(renames, arg, arg_id)))
      return(TLam2(p, arg_id, body2))
    }
    App1(p, foo, bar) -> {
      use foo2 <- do(expr(foo, renames))
      use bar2 <- do(expr(bar, renames))
      return(App2(p, foo2, bar2))
    }
    TApp1(p, foo, bar) -> {
      use foo2 <- do(expr(foo, renames))
      use bar2 <- do(typ(bar, renames))
      return(TApp2(p, foo2, bar2))
    }
  }
}

fn typ(t: Type1, type_renames: Map(String, Int)) -> Monad(Type2) {
  case t {
    TVar1(p, x) -> 
      case get(type_renames, x) {
        Ok(id) -> return(TVar2(id))
        Error(Nil) -> fail(Undefined(p, x))
      }
    TConstr1("Dyn", []) -> return(TDynamic2)
    TConstr1(s, ts) -> {
      use ts2 <- do(monad.map(ts, typ(_, type_renames)))
      return(TConstr2(s, ts2))
    }
    TForall1(arg, body) -> {
      use arg_id <- fresh()
      use body2 <- do(typ(body, insert(type_renames, arg, arg_id)))
      return(TForall2(arg_id, body2))
    }
    TFuncType1(a, b) -> {
      use a2 <- do(typ(a, type_renames))
      use b2 <- do(typ(b, type_renames))
      return(TFuncType2(a2, b2))
    }
  }
}