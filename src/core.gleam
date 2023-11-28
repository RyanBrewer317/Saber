import gleam/int.{to_string}
import gleam/string
import gleam/list
import gleam/bool.{guard}
import gleam/map.{type Map}
import party.{type ParseError, type Position}

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
  UnknownInterField(Position, Expr3, String)
  AccessingNonInter(Position, Expr3, String)
  CallingNonFunction(Expr3)
  CallingNonForall(Expr3)
  SortMismatch(Position, Expr3, Expr3)
  CallingWrongArity(Position, Expr3, Int)
  TypeAtRuntime(Expr3)
  ExtendedError(Error, String)
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
    UnknownInterField(pos, t, field) ->
      "Type Error! Trying to access field " <> field <> " on a value of type " <> pretty_expr3(
        t,
      ) <> " at " <> string.inspect(pos)
    AccessingNonInter(pos, e, field) -> "Type Error! Accessing field " <> field <> " of non-intersection expression " <> pretty_expr3(e) <> " at " <> string.inspect(pos)
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
  // DotAccess1(pos: Position, expr: Expr1, name: String)
  Func1(pos: Position, args: List(Arg1), body: Expr1)
  App1(pos: Position, func: Expr1, args: List(Expr1))
  TPi1(pos: Position, args: List(Arg1), body: Expr1)
  TInter1(pos: Position, args: List(#(String, Expr1)))
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
  // InterAccess2(pos: Position, expr: Expr2, name: String)
  Func2(pos: Position, args: List(Arg2), body: Expr2)
  App2(pos: Position, func: Expr2, args: List(Expr2))
  TPi2(pos: Position, args: List(Arg2), body: Expr2)
  TType2(pos: Position)
  TInter2(pos: Position, args: List(#(Id, Expr2)))
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
  // InterAccess3(pos: Position, t: Expr3, expr: Expr3, name: String)
  Func3(pos: Position, t: Expr3, args: List(Arg3), body: Expr3)
  App3(pos: Position, t: Expr3, func: Expr3, args: List(Expr3))
  TPi3(pos: Position, args: List(Arg3), body: Expr3)
  TType3(pos: Position)
  TKind3(pos: Position)
  TInter3(pos: Position, args: List(#(Id, Expr3)))
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
    // InterAccess3(_, _, e, field) -> pretty_expr3(e) <> "." <> field
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
        |> list.map(pretty_expr3)
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
  }
}

pub fn contains3(e: Expr3, id: Id) -> Bool {
  let c = contains3(_, id)
  case e {
    Ident3(_, _, Local(x)) if x == id -> True
    Ident3(_, t, _) -> c(t)
    Builtin3(_, t, _) -> c(t)
    Func3(_, t, args, body) ->
      c(t) || list.any(args, fn(a) { c(a.t) }) || c(body)
    App3(_, t, func, args) -> c(t) || c(func) || list.any(args, c)
    TPi3(_, args, body) -> list.any(args, fn(a) { c(a.t) }) || c(body)
    TInter3(_, ts) -> list.any(ts, fn(t) { c(t.1) })
    // InterAccess3(_, t, e, _) -> c(t) || c(e)
    Int3(_, _) | TType3(_) | TKind3(_) -> False
  }
}

pub fn substitute(id: Id, new: Expr3, t: Expr3) -> Expr3 {
  let sub = substitute(id, new, _)
  case t {
    Int3(_, _) -> t
    Ident3(_, _, Local(x)) if id == x -> new
    Ident3(_, _, _) -> t
    Builtin3(_, _, _) -> t
    Ident3(_, _, _) -> t
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
      App3(pos, sub(t), sub(func), list.map(args, sub))
    TPi3(pos, args, body) ->
      case list.any(args, fn(a) { a.id == id }) {
        True -> t
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
    TType3(_) -> t
    TKind3(_) -> t
    TInter3(pos, ts) -> TInter3(pos, list.map(ts, fn(t) { #(t.0, sub(t.1)) }))
    // InterAccess3(pos, t, e, field) -> InterAccess3(pos, sub(t), sub(e), field)
  }
}

fn swap_ident(from id: Id, to new: Id, in t: Expr3) -> Expr3 {
  let sub = swap_ident(id, new, _)
  case t {
    Int3(_, _) -> t
    Ident3(pos, t, Local(x)) if id == x -> Ident3(pos, t, Local(new))
    Ident3(_, _, _) -> t
    Ident3(_, _, _) -> t
    Builtin3(_, _, _) -> t
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
      App3(pos, sub(t), sub(func), list.map(args, sub))
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
    // InterAccess3(pos, t, e, field) -> InterAccess3(pos, sub(t), sub(e), field)
  }
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
        fn(p) { alpha_eq(p.0, p.1) },
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
    // InterAccess3(_, t1, e1, field1), InterAccess3(_, t2, e2, field2) -> alpha_eq(t1, t2) && alpha_eq(e1, e2) && field1 == field2
    // _, _ -> False
  }
}

pub type Stmt3 {
  Def3(pos: Position, id: String, t: Expr3, val: Expr3)
  Import3(pos: Position, name: String)
}

pub fn pretty_stmt3(s: Stmt3) -> String {
  case s {
    Def3(_, name, _, Func3(_, TPi3(_, _, rett), args, body)) ->
      "fn " <> name <> "(" <> string.join(
        list.map(
          args,
          fn(a) { pretty_arg_mode(a.mode) <> "x" <> to_string(a.id) <> ": " <> pretty_expr3(a.t) },
        ),
        ", ",
      ) <> ") -> " <> pretty_expr3(rett) <> " {\n  " <> pretty_expr3(body) <> "\n}"
    Import3(_, name) -> "import " <> name
  }
}

pub fn typeof(e: Expr3) -> Expr3 {
  case e {
    Int3(pos, _) -> Builtin3(pos, TType3(pos), "int")
    Ident3(_, t, _) -> t
    Builtin3(_, t, _) -> t
    // InterAccess3(_, t, _, _) -> t
    Func3(_, t, _, _) -> t
    App3(_, t, _, _) -> t
    TPi3(_, _, b) -> typeof(b)
    TType3(pos) -> TKind3(pos)
    TKind3(_) -> panic("kind is untypeable")
    TInter3(pos, _) -> TType3(pos)
  }
}