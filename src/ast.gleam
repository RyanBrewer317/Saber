import core.{
  App1, App2, Builtin1, Builtin2, Def1, Def2, DotAccess1, Expr1, Expr2, Func1,
  Func2, Global, Id, Ident1, Ident2, Import1, Import2, Int1, Int2, Library1,
  Library2, Local, Module1, Module2, ModuleAccess2, Stmt1, Stmt2, Struct1,
  Struct2, StructAccess2, TDynamic1, TDynamic2, TPi1, TPi2, TStruct1, TStruct2,
  TType2, Undefined,
}
import monad.{
  Monad, State, do, fail, fresh, label, monadic_fold, monadic_map, return,
}
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
  use entry, state <- do(build_mod(lib1.entry, state))
  return(Library2(lib1.path, entry), state)
}

fn build_mod(mod1: Module1, state: State) -> Monad(Module2) {
  use #(ast, renames), state <- monadic_fold(
    mod1.ast,
    #([], []),
    state,
    fn(so_far, s, state) {
      let #(ast, renames) = so_far
      use #(s2, renames), state <- do(stmt(s, renames, mod1, state))
      return(#([s2, ..ast], renames), state)
    },
  )
  use subs, state <- monadic_map(mod1.subs, state, build_mod)
  use state <- label("in symbol table", state)
  use symbol_table, state <- monadic_fold(
    map.to_list(mod1.symbol_table),
    map.new(),
    state,
    fn(st, entry, state) {
      let #(k, v) = entry
      monad.fmap(expr(v, renames, mod1, state), map.insert(st, k, _))
    },
  )
  return(
    Module2(mod1.path, subs, symbol_table, mod1.files, list.reverse(ast)),
    state,
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
      use body2, state <- do(expr(body, renames, mod, state))
      return(#(Def2(p, name, body2), renames), state)
    }
    Import1(p, name) -> return(#(Import2(p, name), renames), state)
  }
}

fn expr(e: Expr1, renames: Renames, mod: Module1, state: State) -> Monad(Expr2) {
  case e {
    Int1(p, i) -> return(Int2(p, i), state)
    Ident1(p, _, "dyn") -> return(TDynamic2(p), state)
    Ident1(p, _, "type") -> return(TType2(p), state)
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
      let not_module = fn(state) {
        use e22, state <- do(expr(e2, renames, mod, state))
        return(StructAccess2(p, e22, field), state)
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
      use state <- label("in func", state)
      use imp_args2, state <- monadic_map(
        imp_args,
        state,
        fn(a, state) { fresh(state, fn(i, state) { return(#(a, i), state) }) },
      )
      let renames = list.append(imp_args2, renames)
      use #(args2_rev, renames), state <- monadic_fold(
        args,
        #([], renames),
        state,
        fn(s, a, state) {
          let #(so_far, renames) = s
          let #(argname, argt) = a
          use arg_id, state <- fresh(state)
          use argt, state <- do(expr(argt, renames, mod, state))
          return(
            #(
              [#(argname, #(arg_id, argt)), ..so_far],
              [#(argname, arg_id), ..renames],
            ),
            state,
          )
        },
      )
      let args2 = list.reverse(args2_rev)
      let renames =
        list.append(list.map(args2, fn(a) { #(a.0, { a.1 }.0) }), renames)
      use body2, state <- do(expr(body, renames, mod, state))
      return(
        Func2(
          p,
          list.map(imp_args2, fn(a) { a.1 }),
          list.map(args2, fn(a) { a.1 }),
          body2,
        ),
        state,
      )
    }
    App1(p, func, args) -> {
      use func2, state <- do(expr(func, renames, mod, state))
      use args2, state <- monadic_map(
        args,
        state,
        fn(a, state) { expr(a, renames, mod, state) },
      )
      return(App2(p, func2, args2), state)
    }
    TPi1(p, imp_args, args, body) -> {
      use imp_args2, state <- monadic_map(
        imp_args,
        state,
        fn(a, state) {
          use i, state <- fresh(state)
          return(#(a, i), state)
        },
      )
      let renames = list.append(imp_args2, renames)
      use #(args2_rev, renames), state <- monadic_fold(
        args,
        #([], renames),
        state,
        fn(s, a, state) {
          let #(so_far, renames) = s
          let #(argname, argt) = a
          use arg_id, state <- fresh(state)
          use argt, state <- do(expr(argt, renames, mod, state))
          return(
            #(
              [#(argname, #(arg_id, argt)), ..so_far],
              [#(argname, arg_id), ..renames],
            ),
            state,
          )
        },
      )
      let args2 = list.reverse(args2_rev)
      let renames =
        list.append(list.map(args2, fn(a) { #(a.0, { a.1 }.0) }), renames)
      use body2, state <- do(expr(body, renames, mod, state))
      return(
        TPi2(
          p,
          list.map(imp_args2, fn(a) { a.1 }),
          list.map(args2, fn(a) { a.1 }),
          body2,
        ),
        state,
      )
    }
    TDynamic1(p) -> return(TDynamic2(p), state)
    Struct1(p, fields) -> {
      use fields2, state <- monadic_map(
        fields,
        state,
        fn(f, state) {
          use val, state <- do(expr(f.1, renames, mod, state))
          return(#(f.0, val), state)
        },
      )
      return(Struct2(p, fields2), state)
    }
    TStruct1(p, fields) -> {
      use fields2, state <- monadic_map(
        fields,
        state,
        fn(f, state) {
          use t, state <- do(expr(f.1, renames, mod, state))
          return(#(f.0, t), state)
        },
      )
      return(TStruct2(p, fields2), state)
    }
  }
}
