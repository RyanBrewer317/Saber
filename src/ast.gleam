import core.{
  App1, App2, Builtin1, Builtin2, Def1, Def2, Expr1, Expr2, Func1, Func2, Id,
  Ident1, Ident2, Int1, Int2, Stmt1, Stmt2, TDynamic1, TDynamic2, TLabelType2,
  TPi1, TPi2, TType2, Undefined,
}
import monad.{Monad, do, fail, fresh, return}
import gleam/list

type Renames =
  List(#(String, Id))

fn get(r: Renames, name: String) -> Result(Id, Nil) {
  case r {
    [] -> Error(Nil)
    [#(name2, id), ..] if name == name2 -> Ok(id)
    [_, ..rest] -> get(rest, name)
  }
}

pub fn iteratee(
  s: Stmt1,
  so_far: #(List(Stmt2), Renames),
) -> Monad(#(List(Stmt2), Renames)) {
  let #(ast, renames) = so_far
  use #(s2, renames2) <- do(stmt(s, renames))
  return(#([s2, ..ast], renames2))
}

fn stmt(s: Stmt1, renames: Renames) -> Monad(#(Stmt2, Renames)) {
  case s {
    Def1(p, name, body) -> {
      use id <- fresh()
      let renames2 = [#(name, id), ..renames]
      use body2 <- do(expr(body, renames2))
      return(#(Def2(p, id, body2), renames2))
    }
  }
}

fn expr(e: Expr1, renames: Renames) -> Monad(Expr2) {
  case e {
    Int1(p, i) -> return(Int2(p, i))
    Ident1(p, "dyn") -> return(TDynamic2(p))
    Ident1(p, "type") -> return(TType2(p))
    Ident1(p, "labeltype") -> return(TLabelType2(p))
    Ident1(p, "int") -> return(Builtin2(p, "int"))
    Ident1(p, name) ->
      case get(renames, name) {
        Ok(id) -> return(Ident2(p, id))
        Error(Nil) -> fail(Undefined(p, name))
      }
    Builtin1(p, name) -> return(Builtin2(p, name))
    Func1(p, imp_args, args, body) -> {
      use imp_args2: List(#(String, Id)) <- do(monad.map(
        imp_args,
        fn(a) { fresh(fn(i) { return(#(a, i)) }) },
      ))
      let renames2 = list.append(imp_args2, renames)
      use args2: List(#(String, #(Id, Expr2))) <- do(monad.map(
        args,
        fn(a) {
          use arg_id <- fresh()
          use argt <- do(expr(a.1, renames2))
          return(#(a.0, #(arg_id, argt)))
        },
      ))
      let renames3 =
        list.append(list.map(args2, fn(a) { #(a.0, { a.1 }.0) }), renames2)
      use body2 <- do(expr(body, renames3))
      return(Func2(
        p,
        list.map(imp_args2, fn(a) { a.1 }),
        list.map(args2, fn(a) { a.1 }),
        body2,
      ))
    }
    App1(p, func, args) -> {
      use func2 <- do(expr(func, renames))
      use args2 <- do(monad.map(args, expr(_, renames)))
      return(App2(p, func2, args2))
    }
    TPi1(p, imp_args, args, body) -> {
      use imp_args2 <- do(monad.map(
        imp_args,
        fn(a) { fresh(fn(i) { return(#(a, i)) }) },
      ))
      let renames2 = list.append(imp_args2, renames)
      use args2 <- do(monad.map(
        args,
        fn(a) {
          use arg_id <- fresh()
          use argt <- do(expr(a.1, renames2))
          return(#(a.0, #(arg_id, argt)))
        },
      ))
      let renames3 =
        list.append(list.map(args2, fn(a) { #(a.0, { a.1 }.0) }), renames2)
      use body2 <- do(expr(body, renames3))
      return(TPi2(
        p,
        list.map(imp_args2, fn(a) { a.1 }),
        list.map(args2, fn(a) { a.1 }),
        body2,
      ))
    }
    TDynamic1(p) -> return(TDynamic2(p))
  }
}
