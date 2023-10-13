import core.{
  App1, App2, Builtin1, Builtin2, Def1, Def2, DotAccess1, DotAccess2, Expr1,
  Expr2, Func1, Func2, Id, Ident1, Ident2, Import1, Import2, Int1, Int2,
  Library1, Library2, Module1, Module2, Stmt1, Stmt2, TDynamic1, TDynamic2,
  TLabelType2, TPi1, TPi2, TType2, Undefined,
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

pub fn build_lib(lib1: Library1) -> Monad(Library2) {
  use entry <- do(build_mod(lib1.entry))
  return(Library2(lib1.path, entry))
}

fn build_mod(mod1: Module1) -> Monad(Module2) {
  use #(ast, _) <- do(monad.reduce(mod1.ast, #([], []), iteratee))
  use subs <- do(monad.map(mod1.subs, build_mod))
  return(Module2(mod1.path, subs, mod1.files, list.reverse(ast)))
}

fn iteratee(
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
    Import1(p, name) -> return(#(Import2(p, name), renames))
  }
}

fn expr(e: Expr1, renames: Renames) -> Monad(Expr2) {
  case e {
    Int1(p, i) -> return(Int2(p, i))
    Ident1(p, _, "dyn") -> return(TDynamic2(p))
    Ident1(p, _, "type") -> return(TType2(p))
    Ident1(p, _, "labeltype") -> return(TLabelType2(p))
    Ident1(p, _, "int") -> return(Builtin2(p, "int"))
    Ident1(p, path, name) ->
      case get(renames, name) {
        Ok(id) -> return(Ident2(p, id))
        Error(Nil) -> fail(Undefined(path, p, name))
      }
    Builtin1(p, name) -> return(Builtin2(p, name))
    DotAccess1(p, e2, field) ->
      monad.fmap(expr(e2, renames), DotAccess2(p, _, field))
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
