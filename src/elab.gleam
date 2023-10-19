import core.{
  App3, App4, Builtin3, Builtin4, Def3, Def4, Downcast3, Downcast4, Expr3, Expr4,
  Func3, Func4, Ident3, Ident4, Import3, Import4, Int3, Int4, Library3, Library4,
  Module3, Module4, ModuleAccess3, ModuleAccess4, Stmt3, Stmt4, Struct3, Struct4,
  StructAccess3, StructAccess4, TDynamic3, TDynamic4, TKind3, TKind4,
  TLabelKind3, TLabelKind4, TLabelType3, TLabelType4, TPi3, TPi4, TStruct3,
  TStruct4, TType3, TType4, Upcast3, Upcast4,
}
import monad.{Monad, State, do, monadic_fold, monadic_map, return}
import gleam/list
import gleam/map

pub fn elaborate_lib(lib3: Library3, state: State) -> Monad(Library4) {
  use entry, state2 <- do(elaborate_mod(lib3.entry, state))
  return(Library4(lib3.path, entry), state2)
}

fn elaborate_mod(mod3: Module3, state: State) -> Monad(Module4) {
  use #(ast), state2 <- monadic_fold(mod3.ast, #([]), state, iteratee)
  use subs, state3 <- monadic_map(mod3.subs, state2, elaborate_mod)
  let symbol_table =
    map.map_values(
      mod3.symbol_table,
      fn(_, v) { monad.unwrap(expr(v, state3)) },
    )
  return(
    Module4(mod3.path, subs, symbol_table, mod3.files, list.reverse(ast)),
    state3,
  )
}

fn iteratee(
  so_far: #(List(Stmt4)),
  s: Stmt3,
  state: State,
) -> Monad(#(List(Stmt4))) {
  use s2, state2 <- do(stmt(s, state))
  let #(so_far2) = so_far
  return(#([s2, ..so_far2]), state2)
}

pub fn stmt(s: Stmt3, state: State) -> Monad(Stmt4) {
  case s {
    Def3(p, id, e) -> {
      use e2, state2 <- do(expr(e, state))
      return(Def4(p, id, e2), state2)
    }
    Import3(p, name) -> return(Import4(p, name), state)
  }
}

fn expr(e: Expr3, state: State) -> Monad(Expr4) {
  case e {
    Int3(p, i) -> return(Int4(p, i), state)
    Ident3(p, t, id) -> {
      use t2, state2 <- do(expr(t, state))
      return(Ident4(p, t2, id), state2)
    }
    Builtin3(p, t, name) -> {
      use t2, state2 <- do(expr(t, state))
      return(Builtin4(p, t2, name), state2)
    }
    ModuleAccess3(p, t, module_name, field) -> {
      use t2, state2 <- do(expr(t, state))
      return(ModuleAccess4(p, t2, module_name, field), state2)
    }
    StructAccess3(p, t, e, field) -> {
      use e2, state2 <- do(expr(e, state))
      use t2, state3 <- do(expr(t, state2))
      return(StructAccess4(p, t2, e2, field), state3)
    }
    Func3(p, t, imp_args, args, body) -> {
      use t2, state2 <- do(expr(t, state))
      let imp_args2 = list.map(imp_args, fn(a) { #(a, TType4(p)) })
      use args2, state3 <- monadic_map(
        args,
        state2,
        fn(a, statex) {
          use t, statex2 <- do(expr(a.1, statex))
          return(#(a.0, t), statex2)
        },
      )
      let args3 = list.append(imp_args2, args2)
      use body2, state4 <- do(expr(body, state3))
      return(Func4(p, t2, args3, body2), state4)
    }
    App3(p, t, func, args) -> {
      use func2, state2 <- do(expr(func, state))
      use t2, state3 <- do(expr(t, state2))
      use args2, state4 <- monadic_map(args, state3, expr)
      return(App4(p, t2, func2, args2), state4)
    }
    TPi3(p, imp_args, args, body) -> {
      let imp_args2 = list.map(imp_args, fn(a) { #(a, TType4(p)) })
      use args2, state2 <- monadic_map(
        args,
        state,
        fn(a, statex) {
          use t, statex2 <- do(expr(a.1, statex))
          return(#(a.0, t), statex2)
        },
      )
      let args3 = list.append(imp_args2, args2)
      use body2, state3 <- do(expr(body, state2))
      return(TPi4(p, args3, body2), state3)
    }
    Downcast3(p, e, to, from) -> {
      use e2, state2 <- do(expr(e, state))
      use to2, state3 <- do(expr(to, state2))
      use from2, state4 <- do(expr(from, state3))
      return(Downcast4(p, e2, to2, from2), state4)
    }
    Upcast3(p, e, to, from) -> {
      use e2, state2 <- do(expr(e, state))
      use to2, state3 <- do(expr(to, state2))
      use from2, state4 <- do(expr(from, state3))
      return(Upcast4(p, e2, to2, from2), state4)
    }
    TDynamic3(p) -> return(TDynamic4(p), state)
    TType3(p) -> return(TType4(p), state)
    TKind3(p) -> return(TKind4(p), state)
    TLabelType3(p) -> return(TLabelType4(p), state)
    TLabelKind3(p) -> return(TLabelKind4(p), state)
    Struct3(p, t, fields) -> {
      use fields2, state2 <- monadic_map(
        fields,
        state,
        fn(f, statex) {
          use val, statex2 <- do(expr(f.1, statex))
          return(#(f.0, val), statex2)
        },
      )
      use t2, state3 <- do(expr(t, state2))
      return(Struct4(p, t2, fields2), state3)
    }
    TStruct3(p, fields) -> {
      use fields2, state2 <- monadic_map(
        fields,
        state,
        fn(f, statex) {
          use t, statex2 <- do(expr(f.1, statex))
          return(#(f.0, t), statex2)
        },
      )
      return(TStruct4(p, fields2), state2)
    }
  }
}
