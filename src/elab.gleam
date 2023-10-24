import core.{
  App3, App4, Builtin3, Builtin4, Def3, Def4, Downcast3, Downcast4, Expr3, Expr4,
  Func3, Func4, Ident3, Ident4, Import3, Import4, Int3, Int4, Library3, Library4,
  Module3, Module4, ModuleAccess3, ModuleAccess4, Stmt3, Stmt4, Struct3, Struct4,
  StructAccess3, StructAccess4, TDynamic3, TDynamic4, TKind3, TKind4, TPi3, TPi4,
  TStruct3, TStruct4, TType3, TType4, Upcast3, Upcast4,
}
import monad.{Monad, State, do, monadic_fold, monadic_map, return}
import gleam/list
import gleam/map

pub fn elaborate_lib(lib3: Library3, state: State) -> Monad(Library4) {
  use entry, state <- do(elaborate_mod(lib3.entry, state))
  return(Library4(lib3.path, entry), state)
}

fn elaborate_mod(mod3: Module3, state: State) -> Monad(Module4) {
  use #(ast), state <- monadic_fold(mod3.ast, #([]), state, iteratee)
  use subs, state <- monadic_map(mod3.subs, state, elaborate_mod)
  let symbol_table =
    map.map_values(mod3.symbol_table, fn(_, v) { monad.unwrap(expr(v, state)) })
  return(
    Module4(mod3.path, subs, symbol_table, mod3.files, list.reverse(ast)),
    state,
  )
}

fn iteratee(
  so_far: #(List(Stmt4)),
  s: Stmt3,
  state: State,
) -> Monad(#(List(Stmt4))) {
  use s2, state <- do(stmt(s, state))
  let #(so_far2) = so_far
  return(#([s2, ..so_far2]), state)
}

pub fn stmt(s: Stmt3, state: State) -> Monad(Stmt4) {
  case s {
    Def3(p, id, e) -> {
      use e2, state <- do(expr(e, state))
      return(Def4(p, id, e2), state)
    }
    Import3(p, name) -> return(Import4(p, name), state)
  }
}

fn expr(e: Expr3, state: State) -> Monad(Expr4) {
  case e {
    Int3(p, i) -> return(Int4(p, i), state)
    Ident3(p, t, id) -> {
      use t2, state <- do(expr(t, state))
      return(Ident4(p, t2, id), state)
    }
    Builtin3(p, t, name) -> {
      use t2, state <- do(expr(t, state))
      return(Builtin4(p, t2, name), state)
    }
    ModuleAccess3(p, t, module_name, field) -> {
      use t2, state <- do(expr(t, state))
      return(ModuleAccess4(p, t2, module_name, field), state)
    }
    StructAccess3(p, t, e, field) -> {
      use e2, state <- do(expr(e, state))
      use t2, state <- do(expr(t, state))
      return(StructAccess4(p, t2, e2, field), state)
    }
    Func3(p, t, imp_args, args, body) -> {
      use t2, state <- do(expr(t, state))
      let imp_args2 = list.map(imp_args, fn(a) { #(a, TType4(p)) })
      use args2, state <- monadic_map(
        args,
        state,
        fn(a, state) {
          use t, state <- do(expr(a.1, state))
          return(#(a.0, t), state)
        },
      )
      let args3 = list.append(imp_args2, args2)
      use body2, state <- do(expr(body, state))
      return(Func4(p, t2, args3, body2), state)
    }
    App3(p, t, func, args) -> {
      use func2, state <- do(expr(func, state))
      use t2, state <- do(expr(t, state))
      use args2, state <- monadic_map(args, state, expr)
      return(App4(p, t2, func2, args2), state)
    }
    TPi3(p, imp_args, args, body) -> {
      let imp_args2 = list.map(imp_args, fn(a) { #(a, TType4(p)) })
      use args2, state <- monadic_map(
        args,
        state,
        fn(a, state) {
          use t, state <- do(expr(a.1, state))
          return(#(a.0, t), state)
        },
      )
      let args3 = list.append(imp_args2, args2)
      use body2, state <- do(expr(body, state))
      return(TPi4(p, args3, body2), state)
    }
    Downcast3(p, e, to, from) -> {
      use e2, state <- do(expr(e, state))
      use to2, state <- do(expr(to, state))
      use from2, state <- do(expr(from, state))
      return(Downcast4(p, e2, to2, from2), state)
    }
    Upcast3(p, e, to, from) -> {
      use e2, state <- do(expr(e, state))
      use to2, state <- do(expr(to, state))
      use from2, state <- do(expr(from, state))
      return(Upcast4(p, e2, to2, from2), state)
    }
    TDynamic3(p) -> return(TDynamic4(p), state)
    TType3(p) -> return(TType4(p), state)
    TKind3(p) -> return(TKind4(p), state)
    Struct3(p, t, fields) -> {
      use fields2, state <- monadic_map(
        fields,
        state,
        fn(f, state) {
          use val, state <- do(expr(f.1, state))
          return(#(f.0, val), state)
        },
      )
      use t2, state <- do(expr(t, state))
      return(Struct4(p, t2, fields2), state)
    }
    TStruct3(p, fields) -> {
      use fields2, state <- monadic_map(
        fields,
        state,
        fn(f, state) {
          use t, state <- do(expr(f.1, state))
          return(#(f.0, t), state)
        },
      )
      return(TStruct4(p, fields2), state)
    }
  }
}
