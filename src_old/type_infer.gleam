import concurrent as c
import grammar as g
import gleam/map
import gleam/result
import fresh as f
import gleam/list
import gleam/erlang/process
import gleam/otp/actor

// import gleam/io

type Types(a) =
  map.Map(g.Id(a), g.Type)

pub fn go(
  id_server: f.IdServer(a),
  from input: c.Stream(g.Stmt),
  to output: c.Stream(g.Stmt),
) -> Result(TypeServer(b), String) {
  use <- c.with(output)
  let assert Ok(type_server) = start_type_server()
  let res = {
    use stmt <- c.foreach_unless_error(input)
    // io.println(g.pretty_stmt(stmt))
    use #(_stmt_t, stmt2) <- result.try(type_stmt(id_server, type_server, stmt))
    c.write(output, stmt2)
    Ok(Nil)
  }
  case res {
    Ok(Nil) -> Ok(type_server)
    Error(s) -> {
      end(type_server)
      Error(s)
    }
  }
}

fn type_stmt(
  id_server: f.IdServer(a),
  type_server: TypeServer(b),
  stmt: g.Stmt,
) -> Result(#(g.Type, g.Stmt), String) {
  // rules: for any expression with id n, its type can be found at type variable n
  // this includes stmt ids for top-level names

  case stmt {
    g.Def(id, body) -> {
      use #(bodyt, body2) <- result.try(type_expr(id_server, type_server, body))
      Ok(#(bodyt, g.Def(id, body2)))
    }
  }
}

fn type_expr(
  id_server: f.IdServer(a),
  type_server: TypeServer(b),
  expr: g.Expr,
) -> Result(#(g.Type, g.Expr), String) {
  case expr {
    g.Ident(_id, ref) -> {
      case get_type(type_server, ref) {
        Ok(t) -> Ok(#(t, expr))
        Error(Nil) -> {
          process.sleep(50)
          case get_type(type_server, ref) {
            Ok(t) -> Ok(#(t, expr))
            Error(Nil) -> panic("todo")
          }
        }
      }
    }
    g.Int(id) -> {
      let t = g.TConstr("Int", [])
      set_type(type_server, id, t)
      Ok(#(t, expr))
    }
    g.Lambda(id, argid, mb_arg_t, body) -> {
      case mb_arg_t {
        Ok(argt) -> {
          set_type(type_server, argid, argt)
          use #(bodyt, body2) <- result.try(type_expr(
            id_server,
            type_server,
            body,
          ))
          let t = g.TFunc(argt, bodyt)
          set_type(type_server, id, t)
          Ok(#(t, g.Lambda(id, argid, mb_arg_t, body2)))
        }
        Error(Nil) -> {
          set_type(type_server, argid, g.TDynamic)
          use #(bodyt, body2) <- result.try(type_expr(
            id_server,
            type_server,
            body,
          ))
          let t = g.TFunc(g.TDynamic, bodyt)
          set_type(type_server, id, t)
          Ok(#(t, g.Lambda(id, argid, mb_arg_t, body2)))
        }
      }
    }
    g.TLambda(id, argid, body) -> {
      use #(bodyt, body2) <- result.try(type_expr(id_server, type_server, body))
      let t = g.TForall(argid, bodyt)
      set_type(type_server, id, t)
      Ok(#(t, g.TLambda(id, argid, body2)))
    }
    g.App(id, foo, bar) -> {
      use #(foot, foo2) <- result.try(type_expr(id_server, type_server, foo))
      use #(bart, bar2) <- result.try(type_expr(id_server, type_server, bar))
      case foot {
        g.TFunc(fa, fr) -> {
          use bar3 <- result.try(simplify(g.Downcast(
            f.get(id_server),
            g.Upcast(f.get(id_server), bar2, bart, g.TDynamic),
            g.TDynamic,
            fa,
          )))
          set_type(type_server, id, fr)
          Ok(#(fr, g.App(id, foo2, bar3)))
        }
        g.TDynamic -> {
          set_type(type_server, id, g.TDynamic)
          Ok(#(
            g.TDynamic,
            g.App(
              id,
              g.Downcast(
                f.get(id_server),
                foo2,
                g.TDynamic,
                g.TFunc(bart, g.TDynamic),
              ),
              bar2,
            ),
          ))
        }
        // TODO: more specific error messages
        _ -> Error("calling non-function")
      }
    }
    g.TApp(id, foo, bar) -> {
      use #(foot, foo2) <- result.try(type_expr(id_server, type_server, foo))
      case foot {
        g.TForall(argid, bodyt) -> {
          let t = substitute(argid, bar, bodyt)
          set_type(type_server, id, t)
          Ok(#(t, g.TApp(id, foo2, bar)))
        }
        _ -> Error("type application to non-type-function")
      }
    }
    g.Upcast(_id, _e, _from, _to) -> panic("trying to type Upcast")
    g.Downcast(_id, _e, _from, _to) -> panic("trying to type Downcast")
    g.TypeExpr(_id, _t) -> panic("trying to type infer a type expression")
  }
  // of course possible in principle (give it type Type) but if it ever happens then there must be a compiler bug
}

fn simplify(expr: g.Expr) -> Result(g.Expr, String) {
  case expr {
    g.Downcast(_id, e, t1, t2) if t1 == t2 -> simplify(e)
    g.Upcast(_id, e, t1, t2) if t1 == t2 -> simplify(e)
    g.Downcast(id, g.Downcast(_id, e, t1, t2), t3, t4) if t2 == t3 ->
      simplify(g.Downcast(id, e, t1, t4))
    g.Upcast(id, g.Upcast(_id, e, t1, t2), t3, t4) if t2 == t3 ->
      simplify(g.Upcast(id, e, t1, t4))
    g.Downcast(id1, g.Upcast(id2, e, t1, t2), t3, t4) if t2 == t3 ->
      case subtype(t1, t4) {
        Ok(t) -> {
          simplify(g.Upcast(id1, g.Downcast(id2, e, t1, t), t, t4))
        }
        Error(Nil) ->
          Error(g.pretty_type(t1) <> " can't be cast to " <> g.pretty_type(t4))
      }
    e -> Ok(e)
  }
}

fn substitute(id: g.Id(a), new: g.Type, t: g.Type) -> g.Type {
  case t {
    g.TVar(id2) if id == id2 -> new
    g.TVar(_) -> t
    g.TForall(id2, t2) -> g.TForall(id2, substitute(id, new, t2))
    g.TFunc(foo, bar) ->
      g.TFunc(substitute(id, new, foo), substitute(id, new, bar))
    g.TDynamic -> g.TDynamic
    g.TConstr(s, args) -> g.TConstr(s, list.map(args, substitute(id, new, _)))
  }
}

fn instantiate_with_dyn(t: g.Type, ignoring: List(g.Id(a))) -> g.Type {
  case t {
    g.TVar(id) ->
      case list.contains(ignoring, id) {
        True -> t
        False -> g.TDynamic
      }
    g.TForall(id, t2) -> instantiate_with_dyn(t2, [id, ..ignoring])
    g.TConstr(s, ts) ->
      g.TConstr(s, list.map(ts, instantiate_with_dyn(_, ignoring)))
    g.TDynamic -> g.TDynamic
    g.TFunc(a, b) ->
      g.TFunc(
        instantiate_with_dyn(a, ignoring),
        instantiate_with_dyn(b, ignoring),
      )
  }
}

fn subtype(t1: g.Type, t2: g.Type) -> Result(g.Type, Nil) {
  case t1, t2 {
    t1, t2 if t1 == t2 -> Ok(t1)
    g.TDynamic, _ -> Ok(t2)
    _, g.TDynamic -> Ok(t1)
    g.TFunc(u1, u2), g.TFunc(u3, u4) -> {
      use sup <- result.try(supertype(u1, u3))
      use sub <- result.try(subtype(u2, u4))
      Ok(g.TFunc(sup, sub))
    }
    _, _ -> Error(Nil)
  }
}

fn supertype(t1: g.Type, t2: g.Type) -> Result(g.Type, Nil) {
  case t1, t2 {
    t1, t2 if t1 == t2 -> Ok(t1)
    g.TDynamic, _ -> Ok(t1)
    _, g.TDynamic -> Ok(t2)
    g.TFunc(u1, u2), g.TFunc(u3, u4) -> {
      use sub <- result.try(subtype(u1, u3))
      use sup <- result.try(supertype(u2, u4))
      Ok(g.TFunc(sub, sup))
    }
    _, _ -> Error(Nil)
  }
}

pub opaque type TypeMessage(a) {
  Set(g.Id(a), g.Type)
  Get(g.Id(a), process.Subject(Result(g.Type, Nil)))
  Stop
}

pub type TypeServer(a) =
  process.Subject(TypeMessage(a))

fn start_type_server() -> g.MaybeActor(TypeServer(a)) {
  actor.start(map.new(), handle_message)
}

fn handle_message(msg: TypeMessage(a), types: Types(b)) -> actor.Next(Types(b)) {
  case msg {
    Set(id, t) -> actor.Continue(map.insert(types, id, t))
    Get(id, client) -> {
      process.send(client, map.get(types, id))
      actor.Continue(types)
    }
    Stop -> actor.Stop(process.Normal)
  }
}

fn set_type(type_server: TypeServer(a), id: g.Id(a), t: g.Type) -> Nil {
  process.send(type_server, Set(id, t))
}

pub fn get_type(type_server: TypeServer(a), id: g.Id(a)) -> Result(g.Type, Nil) {
  actor.call(type_server, Get(id, _), 15 * 60 * 1000)
}

pub fn end(type_server: TypeServer(a)) -> Nil {
  actor.send(type_server, Stop)
}
