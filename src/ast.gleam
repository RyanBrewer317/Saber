import core.{
  type Expr1, type Expr2, type Id, type Library1, type Library2, type Module1,
  type Module2, type Stmt1, type Stmt2, App1, App2, Builtin1, Builtin2, Def1,
  Def2, Func1, Func2, Global, Ident1, Ident2, Int1,
  Int2, Library2, Local, Module2,
  TInter1, TInter2, TPi1, TPi2, TType2,
  Undefined, Arg2
}
import monad.{
  type Monad, type State, do, fail, fresh, label, monadic_fold, monadic_map,
  return,
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
    Def1(p, name, t, body) -> {
      use body, state <- do(expr(body, renames, mod, state))
      use t, state <- do(expr(t, renames, mod, state))
      return(#(Def2(p, name, t, body), renames), state)
    }
  }
}

fn expr(e: Expr1, renames: Renames, mod: Module1, state: State) -> Monad(Expr2) {
  case e {
    Int1(p, i) -> return(Int2(p, i), state)
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
    // DotAccess1(p, e, field) -> {
      // use e, state <- do(expr(e, renames, mod, state))
      // return(InterAccess2(p, e, field), state)
      // let not_module = fn(state) {
      //   use e, state <- do(expr(e, renames, mod, state))
      //   return(StructAccess2(p, e, field), state)
      // }
      // let is_mod = fn(name) {
      //   list.find(mod.subs, fn(sub) { sub.path == mod.path <> "/" <> name })
      // }
      // case e {
      //   Ident1(_, _, name) -> {
      //     case is_mod(name) {
      //       Ok(sub) ->
      //         case map.get(sub.symbol_table, field) {
      //           Ok(_) -> return(ModuleAccess2(p, sub.path, field), state)
      //           Error(Nil) -> fail(Undefined(mod.path, p, name <> "." <> field))
      //         }
      //       Error(Nil) -> not_module(state)
      //     }
      //   }
      //   _ -> not_module(state)
      // }
    // }
    Func1(p, args, body) -> {
      use state <- label("in func", state)
      use #(args_rev, renames), state <- monadic_fold(
        args,
        #([], renames),
        state,
        fn(s, a, state) {
          let #(so_far, renames) = s
          use arg_id, state <- fresh(state)
          use argt, state <- do(expr(a.t, renames, mod, state))
          return(
            #(
              [#(a.id, Arg2(mode: a.mode, id: arg_id, t: argt)), ..so_far],
              [#(a.id, arg_id), ..renames],
            ),
            state,
          )
        },
      )
      let args = list.reverse(args_rev)
      let renames =
        list.append(list.map(args, fn(a) { #(a.0, { a.1 }.id) }), renames)
      use body, state <- do(expr(body, renames, mod, state))
      return(
        Func2(
          p,
          list.map(args, fn(a) { a.1 }),
          body,
        ),
        state,
      )
    }
    App1(p, func, args) -> {
      use func, state <- do(expr(func, renames, mod, state))
      use args, state <- monadic_map(
        args,
        state,
        fn(a, state) { expr(a, renames, mod, state) },
      )
      return(App2(p, func, args), state)
    }
    TPi1(p, args, body) -> {
      use #(args_rev, renames), state <- monadic_fold(
        args,
        #([], renames),
        state,
        fn(s, a, state) {
          let #(so_far, renames) = s
          use arg_id, state <- fresh(state)
          use argt, state <- do(expr(a.t, renames, mod, state))
          return(
            #(
              [#(a.id, Arg2(mode: a.mode, id: arg_id, t: argt)), ..so_far],
              [#(a.id, arg_id), ..renames],
            ),
            state,
          )
        },
      )
      let args = list.reverse(args_rev)
      let renames =
        list.append(list.map(args, fn(a) { #(a.0, { a.1 }.id) }), renames)
      use body, state <- do(expr(body, renames, mod, state))
      return(
        TPi2(
          p,
          list.map(args, fn(a) { a.1 }),
          body,
        ),
        state,
      )
    }
    TInter1(p, ts) -> {
      use #(ts_rev, _), state <- monadic_fold(
        ts,
        #([], renames),
        state,
        fn(s, t, state) {
          let #(so_far, renames) = s
          let #(name, ty) = t
          use ty, state <- do(expr(ty, renames, mod, state))
          use new_name, state <- fresh(state)
          return(
            #([#(new_name, ty), ..so_far], [#(name, new_name), ..renames]),
            state,
          )
        },
      )
      let ts = list.reverse(ts_rev)
      return(TInter2(p, ts), state)
    }
  }
}
