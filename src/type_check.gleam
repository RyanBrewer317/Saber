import monad.{Monad, State, do, fail, monadic_fold, monadic_map, return}
import core.{
  App2, App3, Builtin2, Builtin3, CallingNonFunction, CallingWrongArity, Def2,
  Def3, Downcast3, Expr2, Expr3, Func2, Func3, Global, Id, Ident, Ident2, Ident3,
  Import2, Import3, Int2, Int3, Library2, Library3, Local, Module2, Module3,
  ModuleAccess2, ModuleAccess3, Stmt2, Stmt3, Struct2, Struct3, StructAccess2,
  StructAccess3, TDynamic2, TDynamic3, TKind3, TPi2, TPi3, TStruct2, TStruct3,
  TType2, TType3, TypeError, UnknownStructField, Upcast3, contains3,
  ident_to_str, substitute, type_eq, typeof,
}
import gleam/map.{Map, get, insert}
import gleam/result
import gleam/list

pub fn annotate_lib(lib2: Library2, state: State) -> Monad(Library3) {
  use entry, state <- do(annotate_mod(lib2.entry, state))
  return(Library3(lib2.path, entry), state)
}

fn annotate_mod(mod2: Module2, state: State) -> Monad(Module3) {
  use #(ast, gamma, defs), state <- monadic_fold(
    mod2.ast,
    #([], map.new(), map.new()),
    state,
    fn(so_far, s, state) {
      let #(ast, gamma, defs) = so_far
      use #(s2, gamma, defs), state <- do(stmt(s, gamma, mod2, defs, state))
      return(#([s2, ..ast], gamma, defs), state)
    },
  )
  use subs, state <- monadic_map(mod2.subs, state, annotate_mod)
  let symbol_table =
    map.map_values(
      mod2.symbol_table,
      fn(_, v) { monad.unwrap(expr(gamma, v, mod2, defs, state)) },
    )
  return(
    Module3(mod2.path, subs, symbol_table, mod2.files, list.reverse(ast)),
    state,
  )
}

fn stmt(
  s: Stmt2,
  gamma: Map(Ident, Expr3),
  mod: Module2,
  defs: Map(Ident, Expr3),
  state: State,
) -> Monad(#(Stmt3, Map(Ident, Expr3), Map(Ident, Expr3))) {
  case s {
    Def2(p, name, val) -> {
      use val2, state <- do(
        gamma
        |> expr(val, mod, defs, state),
      )
      return(
        #(
          Def3(p, name, val2),
          insert(gamma, Global(name), typeof(val2)),
          insert(defs, Global(name), val2),
        ),
        state,
      )
    }
    Import2(p, name) -> return(#(Import3(p, name), gamma, defs), state)
  }
}

fn expr(
  gamma: Map(Ident, Expr3),
  e: Expr2,
  mod: Module2,
  defs: Map(Ident, Expr3),
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
                  use t2, state <- do(expr(gamma, t, mod, defs, state))
                  return(Ident3(p, t2, id), state)
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
      use func2, state <- do(expr(gamma, func, mod, defs, state))
      case typeof(func2) {
        TPi3(_, imp_args, formal_args, ret_t) -> {
          use state <- monad.when(
            list.length(formal_args) != list.length(args),
            monad.fail(CallingWrongArity(p, typeof(func2), list.length(args))),
            state,
          )
          use args, state <- monadic_map(
            args,
            state,
            fn(a, state) {
              use a2, state <- do(expr(gamma, a, mod, defs, state))
              return(a2, state)
            },
          )
          let solutions = solve(imp_args, formal_args, args)
          use supplied_imp_args, state <- monadic_map(
            imp_args,
            state,
            fn(a, state) {
              return(
                result.lazy_unwrap(
                  map.get(solutions, a),
                  or: fn() { panic("No solution found for implicit argument") },
                ),
                state,
              )
            },
          )
          let solutions =
            list.fold(
              list.zip(formal_args, args),
              solutions,
              fn(sols, a) {
                let #(#(formal, _), actual) = a
                insert(sols, formal, actual)
              },
            )
          let formal_args =
            list.map(formal_args, fn(a) { #(a.0, instantiate(solutions, a.1)) })
          use args, state <- monadic_map(
            list.zip(
              list.append(
                list.map(imp_args, fn(i) { #(i, TType3(p)) }),
                formal_args,
              ),
              list.append(supplied_imp_args, args),
            ),
            state,
            fn(a, state) {
              let #(#(_, argt), actual_arg) = a
              use argt, state <- do(eval(argt, defs, state))
              use actual_arg_t, state <- do(eval(
                typeof(actual_arg),
                defs,
                state,
              ))
              simplify(
                Downcast3(
                  p,
                  Upcast3(p, actual_arg, actual_arg_t, TDynamic3(p)),
                  TDynamic3(p),
                  argt,
                ),
                state,
              )
            },
          )
          return(
            App3(
              p,
              list.fold(
                list.append(
                  list.zip(formal_args, args),
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
              args,
            ),
            state,
          )
        }
        TDynamic3(p) -> {
          use args, state <- monadic_map(
            args,
            state,
            fn(a, state) { expr(gamma, a, mod, defs, state) },
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
                  list.map(args, fn(a) { #(-1, typeof(a)) }),
                  TDynamic3(p),
                ),
              ),
              args,
            ),
            state,
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
      let gamma =
        list.fold(imp_args, gamma, fn(g, a) { insert(g, Local(a), TType3(p)) })
      use #(args_rev, gamma), state <- monadic_fold(
        args,
        #([], gamma),
        state,
        fn(s, a, state) {
          let #(so_far, gamma) = s
          let #(argid, argt) = a
          use argt2, state <- do(expr(gamma, argt, mod, defs, state))
          return(
            #([#(argid, argt2), ..so_far], insert(gamma, Local(argid), argt2)),
            state,
          )
        },
      )
      let args = list.reverse(args_rev)
      let gamma =
        list.fold(args, gamma, fn(g, a) { insert(g, Local(a.0), a.1) })
      use body2, state <- do(
        gamma
        |> expr(body, mod, defs, state),
      )
      return(
        Func3(p, TPi3(p, imp_args, args, typeof(body2)), imp_args, args, body2),
        state,
      )
    }
    TPi2(p, imp_args, args, body) -> {
      let gamma =
        list.fold(imp_args, gamma, fn(g, a) { insert(g, Local(a), TType3(p)) })
      use #(args_rev, gamma), state <- monadic_fold(
        args,
        #([], gamma),
        state,
        fn(s, a, state) {
          let #(so_far, gamma) = s
          let #(argid, argt) = a
          use argt2, state <- do(expr(gamma, argt, mod, defs, state))
          return(
            #([#(argid, argt2), ..so_far], insert(gamma, Local(argid), argt2)),
            state,
          )
        },
      )
      let args = list.reverse(args_rev)
      let gamma =
        list.fold(args, gamma, fn(g, a) { insert(g, Local(a.0), a.1) })
      use body2, state <- do(
        gamma
        |> expr(body, mod, defs, state),
      )
      return(TPi3(p, imp_args, args, body2), state)
    }
    ModuleAccess2(p, path, field) -> {
      let sub =
        mod.subs
        |> list.find(fn(s) { s.path == path })
        |> result.lazy_unwrap(fn() { panic("Module not found: " <> path) })
      let thing =
        map.get(sub.symbol_table, field)
        |> result.lazy_unwrap(fn() { panic("undefined") })
      use t, state <- do(
        gamma
        |> expr(thing, mod, defs, state),
      )
      return(ModuleAccess3(p, t, path, field), state)
    }
    StructAccess2(p, e, field) -> {
      use e2, state <- do(expr(gamma, e, mod, defs, state))
      case typeof(e2) {
        TStruct3(_, fields) ->
          case list.find(fields, fn(f) { f.0 == field }) {
            Ok(#(_, t)) -> return(StructAccess3(p, t, e2, field), state)
            Error(Nil) -> fail(UnknownStructField(p, typeof(e2), field))
          }
        _ -> fail(UnknownStructField(p, typeof(e2), field))
      }
    }
    TDynamic2(p) -> return(TDynamic3(p), state)
    Struct2(p, fields) -> {
      use fields2, state <- monadic_map(
        fields,
        state,
        fn(f, state) {
          use val, state <- do(expr(gamma, f.1, mod, defs, state))
          return(#(f.0, val), state)
        },
      )
      let ts = list.map(fields2, fn(f) { #(f.0, typeof(f.1)) })
      return(Struct3(p, TStruct3(p, ts), fields2), state)
    }
    TStruct2(p, fields) -> {
      use fields2, state <- monadic_map(
        fields,
        state,
        fn(f, state) {
          use t, state <- do(expr(gamma, f.1, mod, defs, state))
          return(#(f.0, t), state)
        },
      )
      return(TStruct3(p, fields2), state)
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

fn eval(e: Expr3, env: Map(Ident, Expr3), state: State) -> Monad(Expr3) {
  case e {
    Ident3(p, t, i) -> {
      use t, state <- do(eval(t, env, state))
      case get(env, i) {
        Ok(e) -> return(e, state)
        Error(Nil) -> return(Ident3(p, t, i), state)
      }
    }
    Int3(_, _) -> return(e, state)
    Builtin3(_, t, _) ->
      do(eval(t, env, state), fn(_, state) { return(e, state) })
    Func3(p, t, imp_args, args, body) -> {
      use t, state <- do(eval(t, env, state))
      use args, state <- monadic_map(
        args,
        state,
        fn(a, state) {
          let #(argid, argt) = a
          use t, state <- do(eval(argt, env, state))
          return(#(argid, t), state)
        },
      )
      return(Func3(p, t, imp_args, args, body), state)
    }
    App3(_, t, func, args) -> {
      use _, state <- do(eval(t, env, state))
      use func, state <- do(eval(func, env, state))
      use args, state <- monadic_map(
        args,
        state,
        fn(a, state) { eval(a, env, state) },
      )
      case func {
        Func3(_, _, _, formal_args, body) -> {
          let substitutions =
            list.fold(
              list.zip(formal_args, args),
              map.new(),
              fn(subs, a) {
                let #(#(argid, _), actual) = a
                insert(subs, argid, actual)
              },
            )
          return(instantiate(substitutions, body), state)
        }
        Builtin3(_, _, "print") -> {
          let assert [Int3(p, arg)] = args
          monad.log(arg)
          return(Int3(p, arg), state)
        }
        _ -> panic("application of non-function")
      }
    }
    TPi3(p, imp_args, args, body) -> {
      use args, state <- monadic_map(
        args,
        state,
        fn(a, state) {
          let #(argid, argt) = a
          use t, state <- do(eval(argt, env, state))
          return(#(argid, t), state)
        },
      )
      return(TPi3(p, imp_args, args, body), state)
    }
    Downcast3(p, e, from, to) -> {
      use e, state <- do(eval(e, env, state))
      use from, state <- do(eval(from, env, state))
      use to, state <- do(eval(to, env, state))
      return(Downcast3(p, e, from, to), state)
    }
    Upcast3(p, e, from, to) -> {
      use e, state <- do(eval(e, env, state))
      use from, state <- do(eval(from, env, state))
      use to, state <- do(eval(to, env, state))
      return(Upcast3(p, e, from, to), state)
    }
    TType3(p) -> return(TType3(p), state)
    TKind3(p) -> return(TKind3(p), state)
    TDynamic3(p) -> return(TDynamic3(p), state)
    TStruct3(p, fields) -> {
      use fields, state <- monadic_map(
        fields,
        state,
        fn(field, state) {
          let #(name, t) = field
          use t, state <- do(eval(t, env, state))
          return(#(name, t), state)
        },
      )
      return(TStruct3(p, fields), state)
    }
    _ -> panic("todo")
  }
}
