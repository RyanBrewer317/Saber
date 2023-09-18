import concurrent as c
import grammar as g
import gleam/iterator
import fresh

pub fn go(
  id_server: fresh.IdServer(a),
  from input: c.Stream(g.Stmt),
  to output: c.Stream(g.ANFStmt),
) -> Nil {
  use <- c.with(output)
  use stmt <- iterator.each(c.to_iterator(input))
  c.write(output, convert_stmt(id_server, stmt))
}

fn convert_stmt(id_server: fresh.IdServer(a), stmt: g.Stmt) -> g.ANFStmt {
  case stmt {
    g.Def(id, body) ->
      g.ANFDef(
        id,
        convert_expr(id_server, body, k: g.ANFHalt(fresh.get(id_server), _)),
      )
  }
}

fn convert_expr(
  id_server: fresh.IdServer(a),
  expr: g.Expr,
  k k: fn(g.ANFVal) -> g.ANFExpr,
) {
  case expr {
    g.Int(id) -> k(g.ANFInt(id))
    g.Ident(_id, ref) -> k(g.ANFLocal(ref))
    g.Lambda(id, argid, _mbargt, body) | g.TLambda(id, argid, body) -> {
      let body2 =
        convert_expr(id_server, body, k: g.ANFHalt(fresh.get(id_server), _))
      g.ANFFunc(id, [argid], body2, k(g.ANFLocal(id)))
    }
    g.App(id, foo, bar) -> {
      use foo2 <- convert_expr(id_server, foo)
      use bar2 <- convert_expr(id_server, bar)
      let assert g.ANFLocal(fid) = foo2
      let newvar = fresh.get(id_server)
      g.ANFApp(id, newvar, fid, [bar2], k(g.ANFLocal(newvar)))
    }
    g.TApp(id, foo, bar) ->
      convert_expr(
        id_server,
        g.App(id, foo, g.TypeExpr(fresh.get(id_server), bar)),
        k,
      )
    g.Upcast(id, e, _to, _from) -> {
      use e2 <- convert_expr(id_server, e)
      let newvar = fresh.get(id_server)
      g.ANFBox(id, newvar, e2, k(g.ANFLocal(newvar)))
    }
    g.Downcast(id, e, _to, _from) -> {
      use e2 <- convert_expr(id_server, e)
      let newvar = fresh.get(id_server)
      g.ANFUnbox(id, newvar, e2, k(g.ANFLocal(newvar)))
    }
    g.TypeExpr(_id, t) -> k(g.ANFTypeVal(t))
  }
}
