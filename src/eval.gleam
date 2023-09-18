import core.{Id, Stmt3, Expr3, Def3, Int3, Ident3, Lam3, TLam3, App3, TApp3, Downcast3, Upcast3}
import monad.{Monad, return, do}
import gleam/map.{Map, get, insert}

type Stmt = Stmt3
type Expr = Expr3

pub fn go(ast: List(Stmt)) -> Monad(Expr) {
  let assert [s, ..ss] = ast
  use #(b, id) <- do(stmt(s, map.new()))
  use #(res, _) <- do(monad.reduce(ss, #(b, insert(map.new(), id, b)), iteratee))
  return(res)
}

fn iteratee(s: Stmt, so_far: #(Expr, Map(Id, Expr))) -> Monad(#(Expr, Map(Id, Expr))) {
  let #(_, heap) = so_far
  use #(val, id) <- do(stmt(s, heap))
  return(#(val, insert(heap, id, val)))
}

fn stmt(s: Stmt, heap: Map(Id, Expr)) -> Monad(#(Expr, Id)) {
  case s {
    Def3(id, val) -> {
      use val2 <- do(expr(val, insert(heap, id, val)))
      return(#(val2, id))
    }
  }
}

fn expr(e: Expr3, heap: Map(Id, Expr3)) -> Monad(Expr3) {
  case e {
    Int3(p, i) -> return(Int3(p, i))
    Ident3(_, _, id) -> 
      case get(heap, id) {
        Ok(val) -> expr(val, heap)
        Error(Nil) -> panic("undefined variable at runtime")
      }
    Lam3(_, _, _, _, _) -> return(e) // no eta reduction
    TLam3(_, _, _, _) -> return(e)   // no eta reduction
    App3(_, _, foo, bar) -> {
      use foo2 <- do(expr(foo, heap))
      use bar2 <- do(expr(bar, heap))
      let assert Lam3(_, _, arg, _, body) = foo2
      expr(body, insert(heap, arg, bar2))
    }
    TApp3(_, _, foo, _) -> {
      use foo2 <- do(expr(foo, heap))
      let assert TLam3(_, _, _, body) = foo2
      expr(body, heap)
    }
    Downcast3(_, e, _, _) | Upcast3(_, e, _, _) -> expr(e, heap)
  }
}