import monad.{Monad, State, do, fail, monadic_fold, monadic_map, return}
import core.{
  App2, App3, Builtin2, Builtin3, CallingNonFunction, CallingWrongArity, Def2,
  Def3, Downcast3, Expr2, Expr3, Func2, Func3, Global, Id, Ident, Ident2, Ident3,
  Import2, Import3, Int2, Int3, Library2, Library3, Local, Module2, Module3,
  ModuleAccess2, ModuleAccess3, Stmt2, Stmt3, Struct2, Struct3, StructAccess2,
  StructAccess3, TDynamic2, TDynamic3, TLabelType2, TLabelType3, TPi2, TPi3,
  TStruct2, TStruct3, TType2, TType3, TypeError, UnknownStructField, Upcast3,
  contains3, ident_to_str, substitute, type_eq, typeof,
}
import gleam/map.{Map, get, insert}
import gleam/result
import gleam/list

pub fn annotate_lib(lib2: Library2, state: State) -> Monad(Library3) {
  use entry, state2 <- do(annotate_mod(lib2.entry, state))
  return(Library3(lib2.path, entry), state2)
}

fn annotate_mod(mod2: Module2, state: State) -> Monad(Module3) {
  use #(ast, gamma), state2 <- monadic_fold(
    mod2.ast,
    #([], map.new()),
    state,
    fn(so_far, s, statex) {
      let #(ast, gamma) = so_far
      use #(s2, gamma2), statex2 <- do(stmt(s, gamma, mod2, statex))
      return(#([s2, ..ast], gamma2), statex2)
    },
  )
  use subs, state3 <- monadic_map(mod2.subs, state2, annotate_mod)
  let symbol_table =
    map.map_values(
      mod2.symbol_table,
      fn(_, v) { monad.unwrap(expr(gamma, v, mod2, state3)) },
    )
  return(
    Module3(mod2.path, subs, symbol_table, mod2.files, list.reverse(ast)),
    state3,
  )
}

fn stmt(
  s: Stmt2,
  gamma: Map(Ident, Expr3),
  mod: Module2,
  state: State,
) -> Monad(#(Stmt3, Map(Ident, Expr3))) {
  case s {
    Def2(p, name, val) -> {
      use val2, state2 <- do(
        gamma
        |> expr(val, mod, state),
      )
      return(
        #(Def3(p, name, val2), insert(gamma, Global(name), typeof(val2))),
        state2,
      )
    }
    Import2(p, name) -> return(#(Import3(p, name), gamma), state)
  }
}

fn expr(
  gamma: Map(Ident, Expr3),
  e: Expr2,
  mod: Module2,
  state: State,
) -> Monad(Expr3) {
  case e {
    TType2(p) -> return(TType3(p), state)
    Ident2(p, id) ->
      case get(gamma, id) {
        Ok(t) -> return(Ident3(p, t, id), state)
        Error(Nil) ->
          case id {
            Local(_) ->
              panic(
                "undefined local variable " <> ident_to_str(id) <> " during typechecking",
              )
            Global(name) ->
              case map.get(mod.symbol_table, name) {
                Ok(t) -> {
                  use t2, state2 <- do(expr(gamma, t, mod, state))
                  return(Ident3(p, t2, id), state2)
                }
                Error(Nil) ->
                  panic(
                    "undefined global variable " <> ident_to_str(id) <> " during typechecking",
                  )
              }
          }
      }

    App2(p, func, args) -> {
      // prove gamma |- func2: typeof(func2)
      use func2, state2 <- do(expr(gamma, func, mod, state))
      case typeof(func2) {
        TPi3(_, imp_args, formal_args, ret_t) -> {
          use state3 <- monad.when(
            list.length(formal_args) != list.length(args),
            monad.fail(CallingWrongArity(p, typeof(func2), list.length(args))),
            state2,
          )
          use args2, state4 <- monadic_map(
            args,
            state3,
            fn(a, statex) { expr(gamma, a, mod, statex) },
          )
          let solutions = solve(imp_args, formal_args, args2)
          use supplied_imp_args, state5 <- monadic_map(
            imp_args,
            state4,
            fn(a, statex) {
              return(
                result.lazy_unwrap(
                  map.get(solutions, a),
                  or: fn() { panic("No solution found for implicit argument") },
                ),
                statex,
              )
            },
          )
          let formal_args2 =
            list.map(formal_args, fn(a) { #(a.0, instantiate(solutions, a.1)) })
          use args3, state6 <- monadic_map(
            list.zip(
              list.append(
                list.map(imp_args, fn(i) { #(i, TType3(p)) }),
                formal_args2,
              ),
              list.append(supplied_imp_args, args2),
            ),
            state5,
            fn(a, statex) {
              let #(#(_, argt), actual_arg) = a
              simplify(
                Downcast3(
                  p,
                  Upcast3(p, actual_arg, typeof(actual_arg), TDynamic3(p)),
                  TDynamic3(p),
                  argt,
                ),
                statex,
              )
            },
          )
          return(
            App3(
              p,
              list.fold(
                list.append(
                  list.zip(formal_args2, args3),
                  list.zip(
                    list.map(imp_args, fn(a) { #(a, TType3(p)) }),
                    supplied_imp_args,
                  ),
                ),
                ret_t,
                fn(ret_t_so_far, a) {
                  let #(#(id, _), actual_arg) = a
                  substitute(id, actual_arg, ret_t_so_far)
                },
              ),
              func2,
              args3,
            ),
            state6,
          )
        }
        TDynamic3(p) -> {
          use args2, state3 <- monadic_map(
            args,
            state2,
            fn(a, statex) { expr(gamma, a, mod, statex) },
          )
          return(
            App3(
              p,
              TDynamic3(p),
              Downcast3(
                p,
                func2,
                TDynamic3(p),
                TPi3(
                  p,
                  [],
                  list.map(args2, fn(a) { #(-1, typeof(a)) }),
                  TDynamic3(p),
                ),
              ),
              args2,
            ),
            state3,
          )
        }
        t -> monad.fail(CallingNonFunction(t))
      }
    }
    Int2(p, i) -> return(Int3(p, i), state)
    Builtin2(p, "int") -> return(Builtin3(p, TType3(p), "int"), state)
    Builtin2(p, "print") ->
      return(
        Builtin3(
          p,
          TPi3(
            pos: p,
            implicit_args: [],
            args: [#(-1, Builtin3(p, TType3(p), "int"))],
            body: Builtin3(p, TType3(p), "int"),
          ),
          "print",
        ),
        state,
      )
    Func2(p, imp_args, args, body) -> {
      let gamma2 =
        list.fold(imp_args, gamma, fn(g, a) { insert(g, Local(a), TType3(p)) })
      use args2, state2 <- monadic_map(
        args,
        state,
        fn(a, statex) {
          use a2, statex2 <- do(expr(gamma2, a.1, mod, statex))
          return(#(a.0, a2), statex2)
        },
      )
      let gamma3 =
        list.fold(args2, gamma2, fn(g, a) { insert(g, Local(a.0), a.1) })
      use body2, state3 <- do(
        gamma3
        |> expr(body, mod, state2),
      )
      return(
        Func3(
          p,
          TPi3(p, imp_args, args2, typeof(body2)),
          imp_args,
          args2,
          body2,
        ),
        state3,
      )
    }
    TPi2(p, imp_args, args, body) -> {
      let gamma2 =
        list.fold(imp_args, gamma, fn(g, a) { insert(g, Local(a), TType3(p)) })
      use args2, state2 <- monadic_map(
        args,
        state,
        fn(a, statex) {
          use a2, statex2 <- do(expr(gamma2, a.1, mod, statex))
          return(#(a.0, a2), statex2)
        },
      )
      let gamma3 =
        list.fold(args2, gamma2, fn(g, a) { insert(g, Local(a.0), a.1) })
      use body2, state3 <- do(
        gamma3
        |> expr(body, mod, state2),
      )
      return(TPi3(p, imp_args, args2, body2), state3)
    }
    ModuleAccess2(p, path, field) -> {
      let sub =
        mod.subs
        |> list.find(fn(s) { s.path == path })
        |> result.lazy_unwrap(fn() { panic("Module not found: " <> path) })
      let thing =
        map.get(sub.symbol_table, field)
        |> result.lazy_unwrap(fn() { panic("undefined") })
      use t, state2 <- do(
        gamma
        |> expr(thing, mod, state),
      )
      return(ModuleAccess3(p, t, path, field), state2)
    }
    StructAccess2(p, e, field) -> {
      use e2, state2 <- do(expr(gamma, e, mod, state))
      case typeof(e2) {
        TStruct3(_, fields) ->
          case list.find(fields, fn(f) { f.0 == field }) {
            Ok(#(_, t)) -> return(StructAccess3(p, t, e2, field), state2)
            Error(Nil) -> fail(UnknownStructField(p, typeof(e2), field))
          }
        _ -> fail(UnknownStructField(p, typeof(e2), field))
      }
    }
    TDynamic2(p) -> return(TDynamic3(p), state)
    TLabelType2(p) -> return(TLabelType3(p), state)
    Struct2(p, fields) -> {
      use fields2, state2 <- monadic_map(
        fields,
        state,
        fn(f, statex) {
          use val, statex2 <- do(expr(gamma, f.1, mod, statex))
          return(#(f.0, val), statex2)
        },
      )
      let ts = list.map(fields2, fn(f) { #(f.0, typeof(f.1)) })
      return(Struct3(p, TStruct3(p, ts), fields2), state2)
    }
    TStruct2(p, fields) -> {
      use fields2, state2 <- monadic_map(
        fields,
        state,
        fn(f, statex) {
          use t, statex2 <- do(expr(gamma, f.1, mod, statex))
          return(#(f.0, t), statex2)
        },
      )
      return(TStruct3(p, fields2), state2)
    }
  }
}

fn type2(t: Expr3) -> Expr2 {
  case t {
    Ident3(pos, _, x) -> Ident2(pos, x)
    Builtin3(pos, _, name) -> Builtin2(pos, name)
    TDynamic3(pos) -> TDynamic2(pos)
    TPi3(pos, imp_args, args, body) ->
      TPi2(
        pos,
        imp_args,
        list.map(args, fn(a) { #(a.0, type2(a.1)) }),
        type2(body),
      )
    TType3(pos) -> TType2(pos)
    _ -> panic("")
  }
}

fn simplify(expr: Expr3, state: State) -> Monad(Expr3) {
  case expr {
    Downcast3(_, e, c, d) ->
      case type_eq(c, d) {
        // casting a type to itself is a no-op
        True -> simplify(e, state)
        False ->
          case e {
            Downcast3(p, e2, a, b) ->
              case type_eq(b, c) {
                // cast A->B then C->D (where B=C) = cast A->D
                True -> simplify(Downcast3(p, e2, a, d), state)
                // cast A->B then C->D (where B!=C) is a type error since they're both downcasts
                False -> monad.fail(TypeError(p, b, c))
              }

            Upcast3(p, e2, a, b) ->
              case type_eq(b, c) {
                // cast up A->B then down C->D (where B=C)
                True ->
                  case subtype(a, d) {
                    // A and D have a subtype s so casting up A->B then down C->D can be rewritten as casting down A->s then up s->D
                    Ok(t) ->
                      simplify(Upcast3(p, Downcast3(p, e2, a, t), t, d), state)
                    // if there is no such subtype then casting up and then down is just changing the type, which is a type error
                    Error(Nil) -> monad.fail(TypeError(p, a, d))
                  }
                False -> return(e, state)
              }
            _ -> return(expr, state)
          }
      }
    Upcast3(_, e, t1, t2) ->
      case type_eq(t1, t2) {
        True -> simplify(e, state)
        False ->
          case e {
            Upcast3(p, e2, t3, t4) ->
              case type_eq(t4, t1) {
                True -> simplify(Upcast3(p, e2, t3, t2), state)
                False -> return(e, state)
              }
            _ -> return(expr, state)
          }
      }

    e -> return(e, state)
  }
}

fn subtype_list(ts1: List(Expr3), ts2: List(Expr3)) -> Result(List(Expr3), Nil) {
  case ts1, ts2 {
    [], [] -> Ok([])
    [], _ -> Error(Nil)
    _, [] -> Error(Nil)
    [t1, ..rest1], [t2, ..rest2] -> {
      use s <- result.try(subtype(t1, t2))
      use ss <- result.try(subtype_list(rest1, rest2))
      Ok([s, ..ss])
    }
  }
}

fn subtype(t1: Expr3, t2: Expr3) -> Result(Expr3, Nil) {
  case type_eq(t1, t2) {
    True -> Ok(t1)
    False ->
      case t1, t2 {
        t1, t2 if t1 == t2 -> Ok(t1)
        TDynamic3(_), _ -> Ok(t2)
        _, TDynamic3(_) -> Ok(t1)
        TPi3(p, imp_args1, args1, body1), TPi3(_, imp_args2, args2, body2) -> {
          case
            list.fold(imp_args1, False, fn(b, a) { b || contains3(body1, a) }) || list.fold(
              args1,
              False,
              fn(b, a) { b || contains3(body1, a.0) },
            ) || list.fold(
              imp_args2,
              False,
              fn(b, a) { b || contains3(body2, a) },
            ) || list.fold(
              args2,
              False,
              fn(b, a) { b || contains3(body2, a.0) },
            )
          {
            True -> Error(Nil)
            False -> {
              let assert [] = imp_args1
              let assert [] = imp_args2
              use sup_args <- result.try(supertype_list(
                list.map(args1, fn(a) { a.1 }),
                list.map(args2, fn(a) { a.1 }),
              ))
              use sub <- result.try(subtype(body1, body2))
              Ok(TPi3(p, [], list.map(sup_args, fn(a) { #(-1, a) }), sub))
            }
          }
        }

        _, _ -> Error(Nil)
      }
  }
}

fn supertype_list(
  ts1: List(Expr3),
  ts2: List(Expr3),
) -> Result(List(Expr3), Nil) {
  case ts1, ts2 {
    [], [] -> Ok([])
    [], _ -> Error(Nil)
    _, [] -> Error(Nil)
    [t1, ..rest1], [t2, ..rest2] -> {
      use s <- result.try(supertype(t1, t2))
      use ss <- result.try(supertype_list(rest1, rest2))
      Ok([s, ..ss])
    }
  }
}

fn supertype(t1: Expr3, t2: Expr3) -> Result(Expr3, Nil) {
  case type_eq(t1, t2) {
    True -> Ok(t1)
    False ->
      case t1, t2 {
        TDynamic3(_), _ -> Ok(t1)
        _, TDynamic3(_) -> Ok(t2)
        TPi3(p, imp_args1, args1, body1), TPi3(_, imp_args2, args2, body2) -> {
          case
            list.fold(imp_args1, False, fn(b, a) { b || contains3(body1, a) }) || list.fold(
              args1,
              False,
              fn(b, a) { b || contains3(body1, a.0) },
            ) || list.fold(
              imp_args2,
              False,
              fn(b, a) { b || contains3(body2, a) },
            ) || list.fold(
              args2,
              False,
              fn(b, a) { b || contains3(body2, a.0) },
            )
          {
            True -> Error(Nil)
            False -> {
              let assert [] = imp_args1
              let assert [] = imp_args2
              use sub_args <- result.try(subtype_list(
                list.map(args1, fn(a) { a.1 }),
                list.map(args2, fn(a) { a.1 }),
              ))
              use sup <- result.try(supertype(body1, body2))
              Ok(TPi3(p, [], list.map(sub_args, fn(a) { #(-1, a) }), sup))
            }
          }
        }
        _, _ -> Error(Nil)
      }
  }
}

fn solve(
  imp_args: List(Id),
  formal_args: List(#(Id, Expr3)),
  actual_args: List(Expr3),
) -> Map(Id, Expr3) {
  case formal_args, actual_args {
    [], [] -> map.new()
    [#(_, t), ..rest], [a, ..rest2] ->
      case t {
        Ident3(_, _, Local(id)) ->
          case
            imp_args
            |> list.contains(id)
          {
            True ->
              solve(imp_args, rest, rest2)
              |> map.insert(id, typeof(a))
            False -> solve(imp_args, rest, rest2)
          }
        _ -> solve(imp_args, rest, rest2)
      }
    _, _ -> panic("arity mismatch during elaboration")
  }
}

fn instantiate(solutions: Map(Id, Expr3), t: Expr3) -> Expr3 {
  let i = instantiate(solutions, _)
  case t {
    Ident3(_, _, Local(id)) ->
      solutions
      |> map.get(id)
      |> result.unwrap(or: t)
    Func3(p, t, imp_args, args, body) ->
      Func3(
        p,
        i(t),
        imp_args,
        list.map(args, fn(a) { #(a.0, i(a.1)) }),
        i(body),
      )
    TPi3(p, imp_args, args, body) ->
      TPi3(p, imp_args, list.map(args, fn(a) { #(a.0, i(a.1)) }), i(body))
    App3(p, t, func, args) -> App3(p, i(t), i(func), list.map(args, i))
    Downcast3(p, e, t1, t2) -> Downcast3(p, i(e), i(t1), i(t2))
    Upcast3(p, e, t1, t2) -> Upcast3(p, i(e), i(t1), i(t2))
    _ -> t
  }
}
