import core.{
  App1, App2, Builtin1, Builtin2, Def1, Def2, DotAccess1, Expr1, Expr2, Func1,
  Func2, Global, Id, Ident1, Ident2, Import1, Import2, Int1, Int2, Library1,
  Library2, Local, Module1, Module2, ModuleAccess2, Stmt1, Stmt2, Struct1,
  Struct2, TDynamic1, TDynamic2, TLabelType2, TPi1, TPi2, TStruct1, TStruct2,
  TType2, Undefined, StructAccess2
}
import monad.{Monad, State, do, fail, fresh, monadic_fold, monadic_map, return}
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

pub fn build_lib(lib1: Library1, state: State) -> Monad(Library2) {
  use entry, state2 <- do(build_mod(lib1.entry, state))
  return(Library2(lib1.path, entry), state2)
}

fn build_mod(mod1: Module1, state: State) -> Monad(Module2) {
  use #(ast, renames), state2 <- monadic_fold(
    mod1.ast,
    #([], []),
    state,
    fn(so_far, s, statex) {
      let #(ast, renames) = so_far
      use #(s2, renames2), statex2 <- do(stmt(s, renames, mod1, statex))
      return(#([s2, ..ast], renames2), statex2)
    },
  )
  use subs, state3 <- monadic_map(mod1.subs, state2, build_mod)
  let symbol_table =
    map.map_values(
      mod1.symbol_table,
      fn(_, v) { monad.unwrap(expr(v, renames, mod1, state3)) },
    )
  return(
    Module2(mod1.path, subs, symbol_table, mod1.files, list.reverse(ast)),
    state3,
  )
}

fn stmt(
  s: Stmt1,
  renames: Renames,
  mod: Module1,
  state: State,
) -> Monad(#(Stmt2, Renames)) {
  case s {
    Def1(p, name, body) -> {
      use body2, state2 <- do(expr(body, renames, mod, state))
      return(#(Def2(p, name, body2), renames), state2)
    }
    Import1(p, name) -> return(#(Import2(p, name), renames), state)
  }
}

fn expr(e: Expr1, renames: Renames, mod: Module1, state: State) -> Monad(Expr2) {
  case e {
    Int1(p, i) -> return(Int2(p, i), state)
    Ident1(p, _, "dyn") -> return(TDynamic2(p), state)
    Ident1(p, _, "type") -> return(TType2(p), state)
    Ident1(p, _, "labeltype") -> return(TLabelType2(p), state)
    Ident1(p, _, "int") -> return(Builtin2(p, "int"), state)
    Ident1(p, _, "print") -> return(Builtin2(p, "print"), state)
    Ident1(p, path, name) ->
      case get(renames, name) {
        Ok(id) -> return(Ident2(p, Local(id)), state)
        Error(Nil) ->
          case map.get(mod.symbol_table, name) {
            Ok(_) -> return(Ident2(p, Global(name)), state)
            Error(Nil) -> fail(Undefined(path, p, name))
          }
      }
    Builtin1(p, name) -> return(Builtin2(p, name), state)
    DotAccess1(p, e2, field) -> {
      let not_module = fn(statex) {
        use e22, statex2 <- do(expr(e2, renames, mod, statex))
        return(StructAccess2(p, e22, field), statex2)
      }
      let is_mod = fn(name) {
        list.find(mod.subs, fn(sub) { sub.path == mod.path <> "/" <> name })
      }
      case e2 {
        Ident1(_, _, name) -> {
          case is_mod(name) {
            Ok(sub) ->
              case map.get(sub.symbol_table, field) {
                Ok(_) -> return(ModuleAccess2(p, sub.path, field), state)
                Error(Nil) -> fail(Undefined(mod.path, p, name <> "." <> field))
              }
            Error(Nil) -> not_module(state)
          }
        }
        _ -> not_module(state)
      }
    }
    Func1(p, imp_args, args, body) -> {
      use imp_args2: List(#(String, Id)), state2 <- monadic_map(
        imp_args,
        state,
        fn(a, statex) {
          fresh(statex, fn(i, statex2) { return(#(a, i), statex2) })
        },
      )
      let renames2 = list.append(imp_args2, renames)
      use args2: List(#(String, #(Id, Expr2))), state3 <- monadic_map(
        args,
        state2,
        fn(a, statex) {
          use arg_id, statex2 <- fresh(statex)
          use argt, statex3 <- do(expr(a.1, renames2, mod, statex2))
          return(#(a.0, #(arg_id, argt)), statex3)
        },
      )
      let renames3 =
        list.append(list.map(args2, fn(a) { #(a.0, { a.1 }.0) }), renames2)
      use body2, state4 <- do(expr(body, renames3, mod, state3))
      return(
        Func2(
          p,
          list.map(imp_args2, fn(a) { a.1 }),
          list.map(args2, fn(a) { a.1 }),
          body2,
        ),
        state4,
      )
    }
    App1(p, func, args) -> {
      use func2, state2 <- do(expr(func, renames, mod, state))
      use args2, state3 <- monadic_map(
        args,
        state2,
        fn(a, statex) { expr(a, renames, mod, statex) },
      )
      return(App2(p, func2, args2), state3)
    }
    TPi1(p, imp_args, args, body) -> {
      use imp_args2, state2 <- monadic_map(
        imp_args,
        state,
        fn(a, statex) {
          use i, statex2 <- fresh(statex)
          return(#(a, i), statex2)
        },
      )
      let renames2 = list.append(imp_args2, renames)
      use args2, state3 <- monadic_map(
        args,
        state2,
        fn(a, statex) {
          use arg_id, statex2 <- fresh(statex)
          use argt, statex3 <- do(expr(a.1, renames2, mod, statex2))
          return(#(a.0, #(arg_id, argt)), statex3)
        },
      )
      let renames3 =
        list.append(list.map(args2, fn(a) { #(a.0, { a.1 }.0) }), renames2)
      use body2, state4 <- do(expr(body, renames3, mod, state3))
      return(
        TPi2(
          p,
          list.map(imp_args2, fn(a) { a.1 }),
          list.map(args2, fn(a) { a.1 }),
          body2,
        ),
        state4,
      )
    }
    TDynamic1(p) -> return(TDynamic2(p), state)
    Struct1(p, fields) -> {
      use fields2, state2 <- monadic_map(
        fields,
        state,
        fn(f, statex) {
          use val, statex2 <- do(expr(f.1, renames, mod, statex))
          return(#(f.0, val), statex2)
        },
      )
      return(Struct2(p, fields2), state2)
    }
    TStruct1(p, fields) -> {
      use fields2, state2 <- monadic_map(
        fields,
        state,
        fn(f, statex) {
          use t, statex2 <- do(expr(f.1, renames, mod, statex))
          return(#(f.0, t), statex2)
        },
      )
      return(TStruct2(p, fields2), state2)
    }
  }
}
