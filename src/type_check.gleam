import core.{
  type Arg3, type Expr2, type Expr3, type Id, type Ident, type Library2,
  type Library3, type Module2, type Module3, type Monad, type Position,
  type State, type Stmt2, type Stmt3, AccessingNonInter, App2, App3, Arg2, Arg3,
  ArgMode, Builtin2, Builtin3, CallingNonFunction, CallingWrongArity, Def2, Def3,
  Func2, Func3, Global, Ident2, Ident3, Int2, Int3, Inter2, Inter3, Library3,
  Local, Module3, Projection2, Projection3, ProjectionOutOfBounds, TEq2, TEq3,
  TInter2, TInter3, TKind3, TPi2, TPi3, TType2, TType3, TypeError,
  UnequalIntersectionComponents, alpha_eq, do, erase, fail, ident_to_str,
  monadic_fold, monadic_map, return, substitute, typeof,
}
import gleam/map.{type Map, get, insert}
import gleam/result
import gleam/list
import gleam/bool

type Context {
  Context(
    gamma: Map(Ident, Expr3),
    mod: Module2,
    defs: Map(Ident, Expr3),
    state: State,
  )
}

fn with_state(c: Context, state: State) -> Context {
  Context(c.gamma, c.mod, c.defs, state)
}

fn with_gamma(c: Context, gamma: Map(Ident, Expr3)) -> Context {
  Context(gamma, c.mod, c.defs, c.state)
}

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
      use #(s2, gamma, defs), state <- do(stmt(
        s,
        Context(gamma, mod2, defs, state),
      ))
      return(#([s2, ..ast], gamma, defs), state)
    },
  )
  use subs, state <- monadic_map(mod2.subs, state, annotate_mod)
  let c = Context(gamma, mod2, defs, state)
  let symbol_table =
    map.map_values(mod2.symbol_table, fn(_, v) { core.unwrap(expr(v, c)) })
  return(
    Module3(mod2.path, subs, symbol_table, mod2.files, list.reverse(ast)),
    state,
  )
}

fn stmt(
  s: Stmt2,
  c: Context,
) -> Monad(#(Stmt3, Map(Ident, Expr3), Map(Ident, Expr3))) {
  case s {
    Def2(p, name, t, val) -> {
      use t, state <- do(expr(t, c))
      let c =
        c
        |> with_state(state)
        |> with_gamma(insert(c.gamma, Global(name), t))
      use e, state <- do(expr(val, c))
      case alpha_eq(typeof(e), t) {
        True ->
          return(
            #(
              Def3(p, name, t, e),
              map.from_list([#(Global(name), t)]),
              map.from_list([#(Global(name), e)]),
            ),
            state,
          )
        False -> fail(TypeError(p, typeof(e), t))
      }
    }
  }
}

fn expr(e: Expr2, c: Context) -> Monad(Expr3) {
  case e {
    TType2(p) -> return(TType3(p), c.state)
    Ident2(p, id) -> ident(p, id, c)
    App2(p, func, args) -> application(p, func, args, c)
    Int2(p, i) -> return(Int3(p, i), c.state)
    Builtin2(p, "int") -> return(Builtin3(p, TType3(p), "int"), c.state)
    Builtin2(p, "print") -> builtin_print(p, c.state)
    Func2(p, args, body) -> func(p, args, body, c)
    TPi2(p, args, body) -> pi(p, args, body, c)
    TInter2(p, ts) -> intersection_type(p, ts, c)
    Projection2(p, e, i) -> projection(p, e, i, c)
    Inter2(p, es) -> intersection(p, es, c)
    TEq2(p, l, r) -> identity_type(p, l, r, c)
  }
}

fn ident(p, id, c: Context) -> Monad(Expr3) {
  case get(c.gamma, id) {
    Ok(t) -> return(Ident3(p, t, id), c.state)
    Error(Nil) ->
      case id {
        Local(_) ->
          panic(
            "undefined local variable " <> ident_to_str(id) <> " during typechecking",
          )
        Global(name) ->
          case map.get(c.mod.symbol_table, name) {
            Ok(t) -> {
              use t2, state <- do(expr(t, c))
              return(Ident3(p, t2, id), state)
            }
            Error(Nil) ->
              panic(
                "undefined global variable " <> ident_to_str(id) <> " during typechecking",
              )
          }
      }
  }
}

fn application(p, func, actual_args, c: Context) -> Monad(Expr3) {
  use func, state <- do(expr(func, c))
  let c = with_state(c, state)
  use #(formal_args, bodyt), state <- do(extract_pi_type(func, state))
  use <- bool.guard(
    when: list.length(actual_args) != list.length(formal_args),
    return: fail(CallingWrongArity(p, func, list.length(actual_args))),
  )
  use #(args_rev, ret, _), state <- monadic_fold(
    list.zip(formal_args, actual_args),
    #([], bodyt, []),
    state,
    fn(so_far, pair, state) {
      let #(args_rev, ret, subs) = so_far
      let c =
        c
        |> with_state(state)
      let #(formal, actual) = pair
      use actual, state <- do(expr(actual, c))
      let formal_t =
        list.fold(
          subs,
          formal.t,
          fn(t, s) {
            let #(from, to) = s
            substitute(from: from, to: to, in: t)
          },
        )
      case alpha_eq(typeof(actual), formal_t) {
        True ->
          return(
            #(
              [#(formal.mode, actual), ..args_rev],
              substitute(from: formal.id, to: actual, in: ret),
              [#(formal.id, actual), ..subs],
            ),
            state,
          )
        False -> fail(TypeError(p, formal.t, typeof(actual)))
      }
    },
  )
  let args = list.reverse(args_rev)
  return(App3(p, ret, func, args), state)
}

fn extract_pi_type(func: Expr3, state: State) -> Monad(#(List(Arg3), Expr3)) {
  case typeof(func) {
    TPi3(_, formal_args, body) -> return(#(formal_args, body), state)
    _ -> fail(CallingNonFunction(func))
  }
}

fn builtin_print(p, state) -> Monad(Expr3) {
  return(
    Builtin3(
      p,
      TPi3(
        pos: p,
        args: [Arg3(ArgMode(False), -1, Builtin3(p, TType3(p), "int"))],
        body: Builtin3(p, TType3(p), "int"),
      ),
      "print",
    ),
    state,
  )
}

fn func(p, args, body, c: Context) -> Monad(Expr3) {
  use #(args_rev, gamma), state <- monadic_fold(
    args,
    #([], c.gamma),
    c.state,
    fn(s, a, state) {
      let #(so_far, gamma) = s
      let c =
        c
        |> with_gamma(gamma)
        |> with_state(state)
      use argt2, state <- do(expr(a.t, c))
      return(
        #(
          [Arg3(mode: a.mode, id: a.id, t: argt2), ..so_far],
          insert(gamma, Local(a.id), argt2),
        ),
        state,
      )
    },
  )
  let args = list.reverse(args_rev)
  let c =
    c
    |> with_gamma(gamma)
    |> with_state(state)
  use body2, state <- do(expr(body, c))
  return(Func3(p, TPi3(p, args, typeof(body2)), args, body2), state)
}

fn pi(p, args, body, c: Context) -> Monad(Expr3) {
  use #(args_rev, gamma), state <- monadic_fold(
    args,
    #([], c.gamma),
    c.state,
    fn(s, a, state) {
      let #(so_far, gamma) = s
      let c =
        c
        |> with_gamma(gamma)
        |> with_state(state)
      use argt2, state <- do(expr(a.t, c))
      return(
        #(
          [Arg3(mode: a.mode, id: a.id, t: argt2), ..so_far],
          insert(gamma, Local(a.id), argt2),
        ),
        state,
      )
    },
  )
  let args = list.reverse(args_rev)
  let c =
    c
    |> with_gamma(gamma)
    |> with_state(state)
  use body2, state <- do(expr(body, c))
  return(TPi3(p, args, body2), state)
}

fn intersection_type(p, ts, c: Context) -> Monad(Expr3) {
  use #(ts_rev, _), state <- monadic_fold(
    ts,
    #([], c.gamma),
    c.state,
    fn(s, t, state) {
      let #(so_far, gamma) = s
      let #(name, ty) = t
      let c =
        c
        |> with_gamma(gamma)
        |> with_state(state)
      use ty, state <- do(expr(ty, c))
      return(#([#(name, ty), ..so_far], insert(gamma, Local(name), ty)), state)
    },
  )
  let ts = list.reverse(ts_rev)
  return(TInter3(p, ts), state)
}

fn intersection(p, es: List(#(Id, Expr2, Expr2)), c: Context) -> Monad(Expr3) {
  let assert [e, ..rest] = es
  let #(first_id, first, first_t) = e
  use first, state <- do(expr(first, c))
  use erased_first, state <- do(core.normalize(
    p,
    erase(first, c.defs),
    state,
    map.new(),
  ))
  use first_t, state <- do(expr(first_t, with_state(c, state)))
  use <- bool.guard(
    when: !alpha_eq(first_t, typeof(first)),
    return: fail(TypeError(p, first_t, typeof(first))),
  )
  let c =
    c
    |> with_state(state)
    |> with_gamma(insert(c.gamma, Local(first_id), first_t))
  use #(rest_rev, _), state <- monadic_fold(
    rest,
    #([], c.gamma),
    state,
    fn(so_far, component, state) {
      let #(rest_rev, gamma) = so_far
      let c =
        c
        |> with_state(state)
        |> with_gamma(gamma)
      let #(id, e, t) = component
      use e, state <- do(expr(e, c))
      use erased_e, state <- do(core.normalize(
        p,
        erase(e, c.defs),
        state,
        map.new(),
      ))
      use <- bool.guard(
        when: !core.erased_alpha_eq(erased_e, erased_first),
        return: fail(UnequalIntersectionComponents(p, first, e)),
      )
      use t, state <- do(expr(t, with_state(c, state)))
      return(#([#(id, e, t), ..rest_rev], insert(gamma, Local(id), e)), state)
    },
  )
  let components = [#(first_id, first, first_t), ..list.reverse(rest_rev)]
  let ts =
    list.map(
      components,
      fn(component) {
        let #(id, _, t) = component
        #(id, t)
      },
    )
  return(Inter3(p, TInter3(p, ts), components), state)
}

fn projection(p, e, i, c: Context) -> Monad(Expr3) {
  use e, state <- do(expr(e, c))
  case typeof(e) {
    TInter3(_, fields) -> {
      case list.at(fields, i) {
        Ok(f) -> return(Projection3(p, f.1, e, i), state)
        Error(Nil) -> fail(ProjectionOutOfBounds(p, typeof(e), i))
      }
    }
    _ -> fail(AccessingNonInter(p, e, i))
  }
}

fn identity_type(p: Position, l: Expr2, r: Expr2, c: Context) -> Monad(Expr3) {
  use l, state <- do(expr(l, c))
  use r, state <- do(expr(r, with_state(c, state)))
  return(TEq3(p, l, r), state)
}

fn type2(t: Expr3) -> Expr2 {
  case t {
    Ident3(pos, _, x) -> Ident2(pos, x)
    Builtin3(pos, _, name) -> Builtin2(pos, name)
    TPi3(pos, args, body) ->
      TPi2(
        pos,
        list.map(args, fn(a) { Arg2(mode: a.mode, id: a.id, t: type2(a.t)) }),
        type2(body),
      )
    TType3(pos) -> TType2(pos)
    _ -> panic("")
  }
}

fn instantiate(solutions: Map(Id, Expr3), t: Expr3) -> Expr3 {
  let i = instantiate(solutions, _)
  case t {
    Ident3(_, _, Local(id)) ->
      solutions
      |> map.get(id)
      |> result.unwrap(or: t)
    Func3(p, t, args, body) ->
      Func3(
        p,
        i(t),
        list.map(args, fn(a) { Arg3(mode: a.mode, id: a.id, t: i(a.t)) }),
        i(body),
      )
    TPi3(p, args, body) ->
      TPi3(
        p,
        list.map(args, fn(a) { Arg3(mode: a.mode, id: a.id, t: i(a.t)) }),
        i(body),
      )
    App3(p, t, func, args) ->
      App3(p, i(t), i(func), list.map(args, fn(a) { #(a.0, i(a.1)) }))
    _ -> t
  }
}

fn eval(e: Expr3, env: Map(Ident, Expr3), state: State) -> Monad(Expr3) {
  case e {
    Ident3(p, t, i) -> eval_ident(p, t, i, env, state)
    Int3(_, _) -> return(e, state)
    Builtin3(_, t, _) -> eval_builtin(e, t, env, state)
    Func3(p, t, args, body) -> eval_func(p, t, args, body, env, state)
    App3(_, t, func, args) -> eval_application(t, func, args, env, state)
    TPi3(p, args, body) -> eval_pi(p, args, body, env, state)
    TType3(p) -> return(TType3(p), state)
    TKind3(p) -> return(TKind3(p), state)
    _ -> panic("todo")
  }
}

fn eval_ident(p, t, i, env, state) -> Monad(Expr3) {
  use t, state <- do(eval(t, env, state))
  case get(env, i) {
    Ok(e) -> return(e, state)
    Error(Nil) -> return(Ident3(p, t, i), state)
  }
}

fn eval_builtin(e, t, env, state) -> Monad(Expr3) {
  do(eval(t, env, state), fn(_, state) { return(e, state) })
}

fn eval_func(p, t, args, body, env, state) -> Monad(Expr3) {
  use t, state <- do(eval(t, env, state))
  use args, state <- monadic_map(
    args,
    state,
    fn(a, state) {
      use t, state <- do(eval(a.t, env, state))
      return(Arg3(mode: a.mode, id: a.id, t: t), state)
    },
  )
  return(Func3(p, t, args, body), state)
}

fn eval_application(t, func, args, env, state) -> Monad(Expr3) {
  use _, state <- do(eval(t, env, state))
  use func, state <- do(eval(func, env, state))
  use args, state <- monadic_map(
    args,
    state,
    fn(a, state) { eval(a.1, env, state) },
  )
  case func {
    Func3(_, _, formal_args, body) -> {
      let substitutions =
        list.fold(
          list.zip(formal_args, args),
          map.new(),
          fn(subs, a) {
            let #(arg, actual) = a
            insert(subs, arg.id, actual)
          },
        )
      return(instantiate(substitutions, body), state)
    }
    Builtin3(_, _, "print") -> {
      let assert [Int3(p, arg)] = args
      core.log(arg)
      return(Int3(p, arg), state)
    }
    _ -> panic("application of non-function")
  }
}

fn eval_pi(p, args, body, env, state) -> Monad(Expr3) {
  use args, state <- monadic_map(
    args,
    state,
    fn(a, state) {
      use t, state <- do(eval(a.t, env, state))
      return(Arg3(mode: a.mode, id: a.id, t: t), state)
    },
  )
  return(TPi3(p, args, body), state)
}
