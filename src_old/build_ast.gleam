import grammar as g
import fresh as fresh
import gleam/map
import gleam/erlang/process
import gleam/otp/actor
import constants
import gleam/list
import gleam/result
import concurrent as c
import gleam/iterator

type ExprRenames =
  map.Map(String, g.Id(g.ExprPh))

type TypeRenames =
  map.Map(String, g.Id(g.TypePh))

type StmtRenames =
  map.Map(String, g.Id(g.ExprPh))

pub fn go(
  id_server: fresh.IdServer(a),
  constant_server: constants.ConstantServer,
  from in: c.Stream(g.SyntaxStmt),
  to out: c.Stream(g.Stmt),
) -> Nil {
  let assert Ok(tlis) = top_level_id_server(id_server)
  use <- c.defer(fn() {
    c.finish(out)
    end_tlis(tlis)
  })
  let it: iterator.Iterator(g.SyntaxStmt) = c.to_iterator(in)
  use stmt <- iterator.each(it)
  c.write(out, build_stmt(id_server, tlis, constant_server, stmt))
}

fn build_stmt(id_server: fresh.IdServer(a), tlis, cs, stmt) -> g.Stmt {
  case stmt {
    g.DeclSyntax(name, arg, argt, body) -> {
      let new_id: g.Id(g.ExprPh) =
        actor.call(tlis, NewId(_, name), 15 * 60 * 1000)
      let arg_id: g.Id(g.ExprPh) = fresh.get(id_server)
      let new_argt = build_type(id_server, tlis, argt, map.new())
      build_expr(
        id_server,
        tlis,
        cs,
        body,
        map.from_list([#(name, new_id), #(arg, arg_id)]),
      )
      |> g.Lambda(new_id, arg_id, Ok(new_argt), _)
      |> g.Def(new_id, _)
    }
    g.DeclTSyntax(name, targ, body) -> {
      let new_id: g.Id(g.ExprPh) =
        actor.call(tlis, NewId(_, name), 15 * 60 * 1000)
      let arg_id: g.Id(g.TypePh) = fresh.get(id_server)
      build_expr(
        id_server,
        tlis,
        cs,
        body,
        map.from_list([#(name, new_id), #(targ, arg_id)]),
      )
      |> g.TLambda(new_id, arg_id, _)
      |> g.Def(new_id, _)
    }
  }
}

fn build_expr(
  id_server: fresh.IdServer(a),
  tlis: TLIS,
  constant_server: constants.ConstantServer,
  e: g.SyntaxExpr,
  renames: ExprRenames,
) -> g.Expr {
  case e {
    g.IntSyntax(i) -> {
      let id = fresh.get(id_server)
      constants.int(constant_server, id, i)
      g.Int(id)
    }
    g.IdentSyntax(name) ->
      case map.get(renames, name) {
        Error(Nil) ->
          g.Ident(fresh.get(id_server), get_top_level_name(tlis, name))
        Ok(id) -> g.Ident(fresh.get(id_server), id)
      }
    g.LamSyntax(arg, mbargt, body) -> {
      let arg_id: g.Id(g.ExprPh) = fresh.get(id_server)
      g.Lambda(
        fresh.get(id_server),
        arg_id,
        result.map(mbargt, build_type(id_server, tlis, _, renames)),
        build_expr(
          id_server,
          tlis,
          constant_server,
          body,
          map.insert(renames, arg, arg_id),
        ),
      )
    }
    g.TLamSyntax(arg, body) -> {
      let arg_id: g.Id(g.TypePh) = fresh.get(id_server)
      g.TLambda(
        fresh.get(id_server),
        arg_id,
        build_expr(
          id_server,
          tlis,
          constant_server,
          body,
          map.insert(renames, arg, arg_id),
        ),
      )
    }
    g.AppSyntax(foo, bar) ->
      g.App(
        fresh.get(id_server),
        build_expr(id_server, tlis, constant_server, foo, renames),
        build_expr(id_server, tlis, constant_server, bar, renames),
      )
    g.TAppSyntax(foo, bar) ->
      g.TApp(
        fresh.get(id_server),
        build_expr(id_server, tlis, constant_server, foo, renames),
        build_type(id_server, tlis, bar, renames),
      )
  }
}

fn build_type(
  id_server: fresh.IdServer(a),
  tlis: TLIS,
  t: g.SyntaxType,
  renames: TypeRenames,
) -> g.Type {
  case t {
    g.TVarSyntax(name) ->
      case map.get(renames, name) {
        Ok(id) -> g.TVar(id)
        Error(Nil) -> g.TVar(get_top_level_name(tlis, name))
      }
    g.TConstrSyntax("Dyn", []) -> g.TDynamic
    g.TConstrSyntax(name, args) ->
      g.TConstr(name, list.map(args, build_type(id_server, tlis, _, renames)))
    g.FuncTypeSyntax(foo, bar) ->
      g.TFunc(
        build_type(id_server, tlis, foo, renames),
        build_type(id_server, tlis, bar, renames),
      )
    g.TForallSyntax(arg, body) -> {
      let arg_id: g.Id(g.TypePh) = fresh.get(id_server)
      g.TForall(
        arg_id,
        build_type(id_server, tlis, body, map.insert(renames, arg, arg_id)),
      )
    }
  }
}

pub opaque type TopLevelIdMessage {
  GetId(process.Subject(g.Id(g.ExprPh)), String)
  NewId(process.Subject(g.Id(g.ExprPh)), String)
  Finished
}

type TLIS =
  process.Subject(TopLevelIdMessage)

fn get_top_level_name(tlis: TLIS, str: String) -> g.Id(g.ExprPh) {
  actor.call(tlis, GetId(_, str), 15 * 60 * 1000)
}

fn end_tlis(tlis: TLIS) {
  actor.send(tlis, Finished)
}

fn top_level_id_server(id_server: fresh.IdServer(a)) -> g.MaybeActor(TLIS) {
  actor.start(#(id_server, map.new()), handle_message)
}

fn handle_message(
  msg: TopLevelIdMessage,
  state: #(fresh.IdServer(a), StmtRenames),
) -> actor.Next(#(fresh.IdServer(a), StmtRenames)) {
  let #(id_server, renames) = state
  case msg {
    GetId(client, name) -> {
      case map.get(renames, name) {
        Ok(id) -> {
          process.send(client, id)
          actor.Continue(state)
        }
        Error(Nil) -> {
          process.sleep(10)
          handle_message(msg, state)
        }
      }
    }
    NewId(client, id) -> {
      let new_id = fresh.get(id_server)
      process.send(client, new_id)
      actor.Continue(#(id_server, map.insert(renames, id, new_id)))
    }
    Finished -> actor.Stop(process.Normal)
  }
}
