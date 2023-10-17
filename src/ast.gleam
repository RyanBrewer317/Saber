import core.{
  App1, App2, Builtin1, Builtin2, Def1, Def2, DotAccess1, Expr1, Expr2, Func1,
  Func2, Global, Id, Ident1, Ident2, Import1, Import2, Int1, Int2, Library1,
  Library2, Local, Module1, Module2, ModuleAccess2, Stmt1, Stmt2, TDynamic1,
  TDynamic2, TLabelType2, TPi1, TPi2, TType2, Undefined,
}
import monad.{Monad, do, fail, fresh, return}
import gleam/list
import gleam/map

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
  use #(ast, renames) <- do(monad.reduce(
    mod1.ast,
    #([], []),
    fn(s, so_far) {
      let #(ast, renames) = so_far
      use #(s2, renames2) <- do(stmt(s, renames, mod1))
      return(#([s2, ..ast], renames2))
    },
  ))
  use subs <- do(monad.map(mod1.subs, build_mod))
  let symbol_table =
    map.map_values(
      mod1.symbol_table,
      fn(_, v) { monad.unwrap(expr(v, renames, mod1)) },
    )
  return(Module2(mod1.path, subs, symbol_table, mod1.files, list.reverse(ast)))
}

fn stmt(s: Stmt1, renames: Renames, mod: Module1) -> Monad(#(Stmt2, Renames)) {
  case s {
    Def1(p, name, body) -> {
      use body2 <- do(expr(body, renames, mod))
      return(#(Def2(p, name, body2), renames))
    }
    Import1(p, name) -> return(#(Import2(p, name), renames))
  }
}

fn expr(e: Expr1, renames: Renames, mod: Module1) -> Monad(Expr2) {
  case e {
    Int1(p, i) -> return(Int2(p, i))
    Ident1(p, _, "dyn") -> return(TDynamic2(p))
    Ident1(p, _, "type") -> return(TType2(p))
    Ident1(p, _, "labeltype") -> return(TLabelType2(p))
    Ident1(p, _, "int") -> return(Builtin2(p, "int"))
    Ident1(p, _, "print") -> return(Builtin2(p, "print"))
    Ident1(p, path, name) ->
      case get(renames, name) {
        Ok(id) -> return(Ident2(p, Local(id)))
        Error(Nil) ->
          case map.get(mod.symbol_table, name) {
            Ok(_) -> return(Ident2(p, Global(name)))
            Error(Nil) -> fail(Undefined(path, p, name))
          }
      }
    Builtin1(p, name) -> return(Builtin2(p, name))
    DotAccess1(p, e2, field) -> {
      let not_module = fn() { todo }
      let is_mod = fn(name) {
        list.find(mod.subs, fn(sub) { sub.path == mod.path <> "/" <> name })
      }
      case e2 {
        Ident1(_, _, name) -> {
          case is_mod(name) {
            Ok(sub) ->
              case map.get(sub.symbol_table, field) {
                Ok(_) -> return(ModuleAccess2(p, sub.path, field))
                Error(Nil) -> fail(Undefined(mod.path, p, name <> "." <> field))
              }
            Error(Nil) -> not_module()
          }
        }
        _ -> not_module()
      }
    }
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
          use argt <- do(expr(a.1, renames2, mod))
          return(#(a.0, #(arg_id, argt)))
        },
      ))
      let renames3 =
        list.append(list.map(args2, fn(a) { #(a.0, { a.1 }.0) }), renames2)
      use body2 <- do(expr(body, renames3, mod))
      return(Func2(
        p,
        list.map(imp_args2, fn(a) { a.1 }),
        list.map(args2, fn(a) { a.1 }),
        body2,
      ))
    }
    App1(p, func, args) -> {
      use func2 <- do(expr(func, renames, mod))
      use args2 <- do(monad.map(args, expr(_, renames, mod)))
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
          use argt <- do(expr(a.1, renames2, mod))
          return(#(a.0, #(arg_id, argt)))
        },
      ))
      let renames3 =
        list.append(list.map(args2, fn(a) { #(a.0, { a.1 }.0) }), renames2)
      use body2 <- do(expr(body, renames3, mod))
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
