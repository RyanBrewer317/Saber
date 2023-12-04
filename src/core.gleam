import gleam/int.{to_string}
import gleam/string
import gleam/list
import gleam/bool.{guard}
import gleam/map.{type Map}
import party.{type ParseError}
import gleam/io
import gleam/string_builder.{type StringBuilder}

pub type Position =
  party.Position

pub opaque type State {
  State(id: Id, log: StringBuilder)
}

pub opaque type Monad(a) {
  Cont(State, a)
  Fail(Error)
}

pub fn do(ma: Monad(a), f: fn(a, State) -> Monad(b)) -> Monad(b) {
  case ma {
    Cont(state, val) -> f(val, state)
    Fail(e) -> Fail(e)
  }
}

pub fn fresh(state: State, k: fn(Id, State) -> Monad(b)) -> Monad(b) {
  k(state.id, State(state.id + 1, state.log))
}

pub fn return(x: a, state: State) -> Monad(a) {
  Cont(state, x)
}

pub fn try(
  x: Result(a, Error),
  state: State,
  k: fn(a, State) -> Monad(b),
) -> Monad(b) {
  case x {
    Ok(val) -> k(val, state)
    Error(e) -> Fail(e)
  }
}

pub fn fail(e: Error) -> Monad(a) {
  Fail(e)
}

pub fn eval(ma: Monad(a)) -> Result(a, Error) {
  case ma {
    Cont(state, val) -> {
      io.println(string_builder.to_string(state.log))
      Ok(val)
    }
    Fail(e) -> Error(e)
  }
}

pub fn start(k: fn(State) -> Monad(a)) -> Monad(a) {
  k(State(0, string_builder.new()))
}

pub fn monadic_map(
  l: List(a),
  state: State,
  f: fn(a, State) -> Monad(b),
  k: fn(List(b), State) -> Monad(c),
) -> Monad(c) {
  case l {
    [] -> k([], state)
    [x, ..xs] -> {
      case f(x, state) {
        Cont(state, x2) -> {
          use xs2, state <- monadic_map(xs, state, f)
          k([x2, ..xs2], state)
        }
        Fail(e) -> Fail(e)
      }
    }
  }
}

pub fn fmap(ma: Monad(a), f: fn(a) -> b) -> Monad(b) {
  case ma {
    Cont(state, x) -> Cont(state, f(x))
    Fail(e) -> Fail(e)
  }
}

pub fn monadic_fold(
  over l: List(a),
  from base: b,
  using state: State,
  with f: fn(b, a, State) -> Monad(b),
  then k: fn(b, State) -> Monad(c),
) -> Monad(c) {
  case l {
    [] -> k(base, state)
    [x, ..xs] -> {
      case f(base, x, state) {
        Cont(state, b2) -> {
          monadic_fold(xs, b2, state, f, k)
        }
        Fail(e) -> Fail(e)
      }
    }
  }
}

pub fn when(
  cond: Bool,
  ma: Monad(Nil),
  state: State,
  k: fn(State) -> Monad(a),
) -> Monad(a) {
  case cond {
    True ->
      case ma {
        Cont(state, Nil) -> k(state)
        Fail(e) -> Fail(e)
      }
    False -> k(state)
  }
}

pub fn unwrap(ma: Monad(a)) -> a {
  case ma {
    Cont(_, x) -> x
    Fail(_) -> panic("")
  }
}

pub fn log(msg: a) -> a {
  io.debug(msg)
  msg
}

pub fn label(msg: String, state: State, k: fn(State) -> Monad(a)) -> Monad(a) {
  // io.println(msg)
  let x = k(state)
  case x {
    Cont(_, _) -> x
    Fail(e) -> Fail(ExtendedError(e, msg))
  }
}

pub type Library0 {
  Library0(path: String, entry: Module0)
}

pub type Module0 {
  Module0(path: String, subs: List(Module0), files: List(String))
}

pub type Library1 {
  Library1(path: String, entry: Module1)
}

pub type Module1 {
  Module1(
    path: String,
    subs: List(Module1),
    symbol_table: Map(String, Expr1),
    files: List(String),
    ast: List(Stmt1),
  )
}

pub type Library2 {
  Library2(path: String, entry: Module2)
}

pub type Module2 {
  Module2(
    path: String,
    subs: List(Module2),
    symbol_table: Map(String, Expr2),
    files: List(String),
    ast: List(Stmt2),
  )
}

pub type Library3 {
  Library3(path: String, entry: Module3)
}

pub type Module3 {
  Module3(
    path: String,
    subs: List(Module3),
    symbol_table: Map(String, Expr3),
    files: List(String),
    ast: List(Stmt3),
  )
}

pub type Error {
  CouldntOpenFile(String)
  ParseError(String, ParseError(Nil))
  Undefined(String, Position, String)
  TypeError(Position, Expr3, Expr3)
  ProjectionOutOfBounds(Position, Expr3, Int)
  AccessingNonInter(Position, Expr3, Int)
  UnequalIntersectionComponents(Position, Expr3, Expr3)
  CallingNonFunction(Expr3)
  CallingNonForall(Expr3)
  SortMismatch(Position, Expr3, Expr3)
  CallingWrongArity(Position, Expr3, Int)
  TypeAtRuntime(Expr3)
  ExtendedError(Error, String)
  PureCallingWrongArity(Position, Int, Int)
  PureCallingNonFunction(Position)
}

pub fn pretty_err(e: Error) -> String {
  case e {
    CouldntOpenFile(r) ->
      "Error! Couldn't open the Saber file! Reason: " <> string.inspect(r)
    ParseError(path, p) ->
      "Parse error! " <> string.inspect(p) <> " in " <> path
    Undefined(path, pos, s) ->
      "Error! Undefined variable " <> s <> " at " <> string.inspect(pos) <> " in " <> path
    TypeError(pos, t1, t2) ->
      "Type error! Couldn't unify " <> pretty_expr3(t1) <> " and " <> pretty_expr3(
        t2,
      ) <> " at " <> string.inspect(pos)
    ProjectionOutOfBounds(pos, t, i) ->
      "Type Error! Projection " <> to_string(i) <> " out of bounds on a value of type " <> pretty_expr3(
        t,
      ) <> " at " <> string.inspect(pos)
    AccessingNonInter(pos, e, i) ->
      "Type Error! Accessing component " <> to_string(i) <> " of non-intersection expression " <> pretty_expr3(
        e,
      ) <> " at " <> string.inspect(pos)
    UnequalIntersectionComponents(pos, e1, e2) ->
      "Type Error! An intersection was found to have non-equal components " <> pretty_expr3(
        e1,
      ) <> " and " <> pretty_expr3(e2) <> " at " <> string.inspect(pos)
    CallingNonFunction(t) ->
      "Type error! Calling non-function of type " <> pretty_expr3(t) <> " as if it were a function"
    CallingNonForall(t) ->
      "Type error! Calling non-forall of type " <> pretty_expr3(t) <> " as if it were a type-abstraction"
    SortMismatch(pos, s1, s2) ->
      "Sort error! Values of sort " <> pretty_expr3(s2) <> " cannot depend on values of sort " <> pretty_expr3(
        s1,
      ) <> " but are being made to at " <> string.inspect(pos)
    CallingWrongArity(pos, t, args_len) ->
      "Type error! Calling expression of type " <> pretty_expr3(t) <> " with " <> to_string(
        args_len,
      ) <> " args at " <> string.inspect(pos)
    TypeAtRuntime(t) ->
      "Runtime error! Found a type at runtime, " <> pretty_expr3(t)
    ExtendedError(e, s) -> "When " <> s <> ", " <> pretty_err(e)
    PureCallingWrongArity(p, i, j) ->
      "Type Error! In pure evaluation, using " <> to_string(j) <> " arguments to call a function that expected " <> to_string(
        i,
      ) <> " at " <> string.inspect(p)
    PureCallingNonFunction(p) ->
      "Type Error! In pure evaluation, calling non function at " <> string.inspect(
        p,
      )
  }
}

pub type Ident {
  Local(name: Id)
  Global(name: String)
}

pub fn ident_to_str(i: Ident) -> String {
  case i {
    Local(id) -> "x" <> int.to_string(id)
    Global(name) -> name
  }
}

pub type ArgMode {
  ArgMode(implicit: Bool)
}

pub type Arg1 {
  Arg1(mode: ArgMode, id: String, t: Expr1)
}

pub type Expr1 {
  Int1(pos: Position, val: Int)
  Ident1(pos: Position, path: String, name: String)
  Builtin1(pos: Position, name: String)
  Projection1(pos: Position, expr: Expr1, idx: Int)
  Func1(pos: Position, args: List(Arg1), body: Expr1)
  App1(pos: Position, func: Expr1, args: List(Expr1))
  TPi1(pos: Position, args: List(Arg1), body: Expr1)
  TInter1(pos: Position, args: List(#(String, Expr1)))
  Inter1(pos: Position, args: List(#(String, Expr1, Expr1)))
  TEq1(pos: Position, left: Expr1, right: Expr1)
}

pub type Stmt1 {
  Def1(pos: Position, name: String, t: Expr1, val: Expr1)
}

pub type Id =
  Int

pub type Arg2 {
  Arg2(mode: ArgMode, id: Id, t: Expr2)
}

pub type Expr2 {
  Int2(pos: Position, val: Int)
  Ident2(pos: Position, id: Ident)
  Builtin2(pos: Position, name: String)
  Projection2(pos: Position, expr: Expr2, idx: Int)
  // InterAccess2(pos: Position, expr: Expr2, name: String)
  Func2(pos: Position, args: List(Arg2), body: Expr2)
  App2(pos: Position, func: Expr2, args: List(Expr2))
  TPi2(pos: Position, args: List(Arg2), body: Expr2)
  TType2(pos: Position)
  TInter2(pos: Position, args: List(#(Id, Expr2)))
  Inter2(pos: Position, args: List(#(Id, Expr2, Expr2)))
  TEq2(pos: Position, left: Expr2, right: Expr2)
}

pub type Stmt2 {
  Def2(pos: Position, name: String, t: Expr2, val: Expr2)
}

pub type Arg3 {
  Arg3(mode: ArgMode, id: Id, t: Expr3)
}

pub type Expr3 {
  Int3(pos: Position, val: Int)
  Ident3(pos: Position, t: Expr3, id: Ident)
  Builtin3(pos: Position, t: Expr3, name: String)
  Projection3(pos: Position, t: Expr3, expr: Expr3, idx: Int)
  Func3(pos: Position, t: Expr3, args: List(Arg3), body: Expr3)
  App3(pos: Position, t: Expr3, func: Expr3, args: List(#(ArgMode, Expr3)))
  TPi3(pos: Position, args: List(Arg3), body: Expr3)
  TType3(pos: Position)
  TKind3(pos: Position)
  TInter3(pos: Position, args: List(#(Id, Expr3)))
  Inter3(pos: Position, t: Expr3, args: List(#(Id, Expr3, Expr3)))
  TEq3(pos: Position, left: Expr3, right: Expr3)
}

pub type PureExpr {
  Int(val: Int)
  Ident(id: Id)
  Builtin(name: String)
  Func(args: List(Id), body: PureExpr)
  App(func: PureExpr, args: List(PureExpr))
}

pub fn erase(e: Expr3, defs: Map(Ident, Expr3)) -> PureExpr {
  let erase = erase(_, defs)
  case e {
    Int3(_, i) -> Int(i)
    Ident3(_, _, id) ->
      case id {
        Local(id) -> Ident(id)
        Global(_) ->
          case map.get(defs, id) {
            Ok(val) -> erase(val)
            Error(Nil) -> panic("undefined global during typechecking")
          }
      }
    Builtin3(_, _, name) -> Builtin(name)
    Projection3(_, _, e, _) -> erase(e)
    Func3(_, _, args, body) ->
      Func(
        list.map(list.filter(args, fn(a) { !a.mode.implicit }), fn(a) { a.id }),
        erase(body),
      )
    App3(_, _, func, args) -> {
      let args = list.filter(args, fn(a) { !{ a.0 }.implicit })
      App(erase(func), list.map(args, fn(a) { erase(a.1) }))
    }
    Inter3(_, _, args) -> {
      let assert Ok(first) = list.first(args)
      erase(first.1)
    }
    _ -> panic("")
  }
}

fn erased_substitute(from old: Id, to new: PureExpr, in e: PureExpr) -> PureExpr {
  let swap = erased_substitute(old, new, _)
  case e {
    Ident(id) if id == old -> new
    Func(args, body) ->
      case list.contains(args, old) {
        True -> e
        False -> Func(args, swap(body))
      }
    App(func, args) -> App(swap(func), list.map(args, swap))
    Int(_) | Ident(_) | Builtin(_) -> e
  }
}

fn erased_func_eq(
  args1: List(Id),
  body1: PureExpr,
  args2: List(Id),
  body2: PureExpr,
) -> Bool {
  case args1, args2 {
    [], [] -> body1 == body2
    [x, ..rest1], [y, ..rest2] ->
      erased_func_eq(
        rest1,
        body1,
        rest2,
        erased_substitute(from: y, to: Ident(x), in: body2),
      )
    _, _ ->
      // different number of args
      False
  }
}

pub fn erased_alpha_eq(e1: PureExpr, e2: PureExpr) -> Bool {
  case e1, e2 {
    Int(i), Int(j) -> i == j
    Ident(x), Ident(y) -> x == y
    Builtin(foo), Builtin(bar) -> foo == bar
    Func(args1, body1), Func(args2, body2) ->
      erased_func_eq(args1, body1, args2, body2)
  }
}

pub fn normalize(
  p: Position,
  e: PureExpr,
  state: State,
  heap: Map(Id, PureExpr),
) -> Monad(PureExpr) {
  case e {
    Ident(id) ->
      case map.get(heap, id) {
        Ok(val) -> return(val, state)
        Error(Nil) ->
          panic(
            "undefined variable in pure expr, which should be during typechecking",
          )
      }
    App(func, args) -> {
      use func, state <- do(normalize(p, func, state, heap))
      case func {
        Func(formals, body) -> {
          use <- bool.guard(
            when: list.length(formals) != list.length(args),
            return: fail(PureCallingWrongArity(
              p,
              list.length(formals),
              list.length(args),
            )),
          )
          let heap = map.merge(map.from_list(list.zip(formals, args)), heap)
          normalize(p, body, state, heap)
        }
        _ -> fail(PureCallingNonFunction(p))
      }
    }
    Int(_) | Builtin(_) | Func(_, _) -> return(e, state)
  }
}

pub fn mod_name_from_path(path: String) -> String {
  let assert Ok(name) = list.last(string.split(path, "/"))
  name
}

pub fn pretty_arg_mode(am: ArgMode) -> String {
  let ArgMode(imp) = am
  case imp {
    True -> "@"
    False -> ""
  }
}

pub fn pretty_expr3(e: Expr3) -> String {
  case e {
    Int3(_, i) -> to_string(i)
    Ident3(_, _, Local(id)) -> "x" <> to_string(id)
    Ident3(_, _, Global(name)) -> name
    Builtin3(_, _, name) -> name
    Projection3(_, _, e, idx) -> pretty_expr3(e) <> "." <> to_string(idx)
    Inter3(_, _, es) ->
      "[" <> string.join(
        list.map(
          es,
          fn(e) {
            "x" <> to_string(e.0) <> ": " <> pretty_expr3(e.1) <> ": " <> pretty_expr3(
              e.2,
            )
          },
        ),
        ", ",
      ) <> "]"
    Func3(_, _, args, body) ->
      "fn(" <> {
        args
        |> list.map(fn(a) {
          pretty_arg_mode(a.mode) <> "x" <> to_string(a.id) <> ": " <> pretty_expr3(
            a.t,
          )
        })
        |> string.join(", ")
      } <> ") " <> pretty_expr3(body)
    App3(_, _, func, args) ->
      pretty_expr3(func) <> "(" <> {
        args
        |> list.map(fn(a) { pretty_expr3(a.1) })
        |> string.join(", ")
      } <> ")"
    TPi3(_, args, body) ->
      case list.any(args, fn(a) { contains3(body, a.id) }) {
        True ->
          "Pi(" <> {
            args
            |> list.map(fn(a) {
              pretty_arg_mode(a.mode) <> "x" <> to_string(a.id) <> ": " <> pretty_expr3(
                a.t,
              )
            })
            |> string.join(", ")
          } <> ") " <> pretty_expr3(body)
        False ->
          "(" <> {
            args
            |> list.map(fn(a) { pretty_arg_mode(a.mode) <> pretty_expr3(a.t) })
            |> string.join(", ")
          } <> ")->" <> pretty_expr3(body)
      }
    TType3(_) -> "type"
    TKind3(_) -> "kind"
    TInter3(_, ts) ->
      "inter{" <> {
        ts
        |> list.map(fn(t) { "x" <> to_string(t.0) <> ": " <> pretty_expr3(t.1) })
        |> string.join(", ")
      } <> "}"
    TEq3(_, l, r) -> pretty_expr3(l) <> " = " <> pretty_expr3(r)
  }
}

pub fn contains3(e: Expr3, id: Id) -> Bool {
  let c = contains3(_, id)
  case e {
    Ident3(_, _, Local(x)) if x == id -> True
    Ident3(_, t, _) -> c(t)
    Builtin3(_, t, _) -> c(t)
    Projection3(_, t, e, _) -> c(t) || c(e)
    Func3(_, t, args, body) ->
      c(t) || list.any(args, fn(a) { c(a.t) }) || c(body)
    App3(_, t, func, args) ->
      c(t) || c(func) || list.any(args, fn(a) { c(a.1) })
    TPi3(_, args, body) -> list.any(args, fn(a) { c(a.t) }) || c(body)
    TInter3(_, ts) -> list.any(ts, fn(t) { c(t.1) })
    Inter3(_, t, es) -> c(t) || list.any(es, fn(e) { c(e.1) })
    Int3(_, _) | TType3(_) | TKind3(_) -> False
    TEq3(_, l, r) -> c(l) || c(r)
  }
}

pub fn substitute(from id: Id, to new: Expr3, in e: Expr3) -> Expr3 {
  let sub = substitute(id, new, _)
  case e {
    Int3(_, _) -> e
    Ident3(_, _, Local(x)) if id == x -> new
    Ident3(_, _, _) -> e
    Builtin3(_, _, _) -> e
    Projection3(pos, t, e, i) -> Projection3(pos, sub(t), sub(e), i)
    Inter3(pos, t, es) ->
      Inter3(pos, sub(t), list.map(es, fn(e) { #(e.0, sub(e.1), sub(e.2)) }))
    Func3(pos, t, args, body) ->
      case list.any(args, fn(a) { a.id == id }) {
        True -> t
        False ->
          Func3(
            pos,
            sub(t),
            list.map(args, fn(a) { Arg3(mode: a.mode, id: a.id, t: sub(a.t)) }),
            sub(body),
          )
      }
    App3(pos, t, func, args) ->
      App3(pos, sub(t), sub(func), list.map(args, fn(a) { #(a.0, sub(a.1)) }))
    TPi3(pos, args, body) ->
      case list.any(args, fn(a) { a.id == id }) {
        True -> e
        False ->
          TPi3(
            pos,
            list.map(
              args,
              fn(a) {
                Arg3(mode: a.mode, id: a.id, t: substitute(id, new, a.t))
              },
            ),
            substitute(id, new, body),
          )
      }
    TType3(_) -> e
    TKind3(_) -> e
    TInter3(pos, ts) -> TInter3(pos, list.map(ts, fn(t) { #(t.0, sub(t.1)) }))
    TEq3(pos, l, r) -> TEq3(pos, sub(l), sub(r))
  }
  // InterAccess3(pos, t, e, field) -> InterAccess3(pos, sub(t), sub(e), field)
}

fn swap_ident(from id: Id, to new: Id, in t: Expr3) -> Expr3 {
  let sub = swap_ident(id, new, _)
  case t {
    Int3(_, _) -> t
    Ident3(pos, t, Local(x)) if id == x -> Ident3(pos, t, Local(new))
    Ident3(_, _, _) -> t
    Ident3(_, _, _) -> t
    Builtin3(_, _, _) -> t
    Projection3(pos, t, e, i) -> Projection3(pos, sub(t), sub(e), i)
    Inter3(pos, t, es) ->
      Inter3(pos, sub(t), list.map(es, fn(e) { #(e.0, sub(e.1), sub(e.2)) }))
    Func3(pos, t, args, body) ->
      case list.any(args, fn(a) { a.id == new }) {
        True -> t
        False ->
          Func3(
            pos,
            sub(t),
            list.map(args, fn(a) { Arg3(mode: a.mode, id: a.id, t: sub(a.t)) }),
            sub(body),
          )
      }
    App3(pos, t, func, args) ->
      App3(pos, sub(t), sub(func), list.map(args, fn(a) { #(a.0, sub(a.1)) }))
    TPi3(pos, args, body) ->
      case list.any(args, fn(a) { a.id == id }) {
        True -> t
        False ->
          TPi3(
            pos,
            list.map(args, fn(a) { Arg3(mode: a.mode, id: a.id, t: sub(a.t)) }),
            sub(body),
          )
      }
    TType3(_) -> t
    TKind3(_) -> t
    TInter3(pos, ts) -> TInter3(pos, list.map(ts, fn(t) { #(t.0, sub(t.1)) }))
    TEq3(pos, l, r) -> TEq3(pos, sub(l), sub(r))
  }
  // InterAccess3(pos, t, e, field) -> InterAccess3(pos, sub(t), sub(e), field)
}

pub fn binder_eq(args1, body1, args2, body2) -> Bool {
  case args1, args2 {
    [], [] -> alpha_eq(body1, body2)
    [arg1, ..rest1], [arg2, ..rest2] ->
      alpha_eq(arg1.t, arg2.t) && binder_eq(
        rest1,
        body1,
        list.map(
          rest2,
          fn(a) {
            Arg3(
              mode: a.mode,
              id: a.id,
              t: swap_ident(from: arg2.id, to: arg1.id, in: a.t),
            )
          },
        ),
        swap_ident(from: arg2.id, to: arg1.id, in: body2),
      )
    _, _ -> False
  }
  // unequal number of arguments
}

pub fn alpha_eq(e1: Expr3, e2: Expr3) -> Bool {
  case e1, e2 {
    Ident3(_, _, id1), Ident3(_, _, id2) if id1 == id2 -> True
    Builtin3(_, _, name1), Builtin3(_, _, name2) if name1 == name2 -> True
    TPi3(_, args1, body1), TPi3(_, args2, body2) -> {
      use <- guard(
        when: list.length(args1) != list.length(args2),
        return: False,
      )
      binder_eq(args1, body1, args2, body2)
    }
    App3(_, _, func1, args1), App3(_, _, func2, args2) ->
      alpha_eq(func1, func2) && list.all(
        list.zip(args1, args2),
        fn(p) { alpha_eq({ p.0 }.1, { p.1 }.1) },
      )
    TType3(_), TType3(_) -> True
    TInter3(_, ts1), TInter3(_, ts2) ->
      list.all(
        list.zip(ts1, ts2),
        fn(pair) {
          let #(#(_, t1), #(_, t2)) = pair
          alpha_eq(t1, t2)
        },
      )
    _, _ -> False
  }
}

pub type Stmt3 {
  Def3(pos: Position, id: String, t: Expr3, val: Expr3)
}

pub fn pretty_stmt3(s: Stmt3) -> String {
  case s {
    Def3(_, name, _, Func3(_, TPi3(_, _, rett), args, body)) ->
      "fn " <> name <> "(" <> string.join(
        list.map(
          args,
          fn(a) {
            pretty_arg_mode(a.mode) <> "x" <> to_string(a.id) <> ": " <> pretty_expr3(
              a.t,
            )
          },
        ),
        ", ",
      ) <> ") -> " <> pretty_expr3(rett) <> " {\n  " <> pretty_expr3(body) <> "\n}"
  }
}

pub fn typeof(e: Expr3) -> Expr3 {
  case e {
    Int3(pos, _) -> Builtin3(pos, TType3(pos), "int")
    Ident3(_, t, _) -> t
    Builtin3(_, t, _) -> t
    Projection3(_, t, _, _) -> t
    Inter3(_, t, _) -> t
    Func3(_, t, _, _) -> t
    App3(_, t, _, _) -> t
    TPi3(_, _, b) -> typeof(b)
    TType3(pos) -> TKind3(pos)
    TKind3(_) -> panic("kind is untypeable")
    TInter3(pos, _) -> TType3(pos)
    TEq3(pos, _, _) -> TType3(pos)
  }
}
