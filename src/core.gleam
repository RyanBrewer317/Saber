import gleam/int.{to_string}
import gleam/string
import gleam/list
import party.{ParseError, Position}

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
    files: List(String),
    ast: List(Stmt3),
  )
}

pub type Library4 {
  Library4(path: String, entry: Module4)
}

pub type Module4 {
  Module4(
    path: String,
    subs: List(Module4),
    files: List(String),
    ast: List(Stmt4),
  )
}

pub type Error {
  CouldntOpenFile(String)
  ParseError(String, ParseError(Nil))
  Undefined(String, Position, String)
  TypeError(Position, Expr3, Expr3)
  CallingNonFunction(Expr3)
  CallingNonForall(Expr3)
  SortMismatch(Position, Expr3, Expr3)
  CallingWrongArity(Position, Expr3, Int)
  TypeAtRuntime(Expr3)
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
  }
}

pub type Expr1 {
  Int1(pos: Position, val: Int)
  Ident1(pos: Position, path: String, name: String)
  Builtin1(pos: Position, name: String)
  DotAccess1(pos: Position, expr: Expr1, name: String)
  Func1(
    pos: Position,
    implicit_args: List(String),
    args: List(#(String, Expr1)),
    body: Expr1,
  )
  App1(pos: Position, func: Expr1, args: List(Expr1))
  TPi1(
    pos: Position,
    imp_args: List(String),
    args: List(#(String, Expr1)),
    body: Expr1,
  )
  TDynamic1(pos: Position)
}

pub type Stmt1 {
  Def1(pos: Position, name: String, val: Expr1)
  Import1(pos: Position, name: String)
}

pub type Id =
  Int

pub type Expr2 {
  Int2(pos: Position, val: Int)
  Ident2(pos: Position, id: Id)
  Builtin2(pos: Position, name: String)
  DotAccess2(pos: Position, expr: Expr2, name: String)
  Func2(
    pos: Position,
    implicit_args: List(Id),
    args: List(#(Id, Expr2)),
    body: Expr2,
  )
  App2(pos: Position, func: Expr2, args: List(Expr2))
  TPi2(
    pos: Position,
    implicit_args: List(Id),
    args: List(#(Id, Expr2)),
    body: Expr2,
  )
  TType2(pos: Position)
  TLabelType2(pos: Position)
  TDynamic2(pos: Position)
}

pub type Stmt2 {
  Def2(pos: Position, name: Id, val: Expr2)
  Import2(pos: Position, name: String)
}

pub type Expr3 {
  Int3(pos: Position, val: Int)
  Ident3(pos: Position, t: Expr3, id: Id)
  Builtin3(pos: Position, t: Expr3, name: String)
  DotAccess3(pos: Position, t: Expr3, expr: Expr3, name: String)
  Func3(
    pos: Position,
    t: Expr3,
    implicit_args: List(Id),
    args: List(#(Id, Expr3)),
    body: Expr3,
  )
  App3(pos: Position, t: Expr3, func: Expr3, args: List(Expr3))
  Upcast3(pos: Position, e: Expr3, from: Expr3, to: Expr3)
  Downcast3(pos: Position, e: Expr3, from: Expr3, to: Expr3)
  TPi3(
    pos: Position,
    implicit_args: List(Id),
    args: List(#(Id, Expr3)),
    body: Expr3,
  )
  TType3(pos: Position)
  TKind3(pos: Position)
  TLabelType3(pos: Position)
  TLabelKind3(pos: Position)
  TDynamic3(pos: Position)
}

pub fn pretty_expr3(e: Expr3) -> String {
  case e {
    Int3(_, i) -> to_string(i)
    Ident3(_, _, id) -> "x" <> to_string(id)
    Builtin3(_, _, name) -> name
    DotAccess3(_, _, e, field) -> pretty_expr3(e) <> "." <> field
    Func3(_, _, [], args, body) ->
      "fn(" <> {
        args
        |> list.map(fn(a) { "x" <> to_string(a.0) <> ": " <> pretty_expr3(a.1) })
        |> string.join(", ")
      } <> ") " <> pretty_expr3(body)
    Func3(_, _, imp_args, args, body) ->
      "fn<" <> {
        imp_args
        |> list.map(fn(a) { "x" <> to_string(a) })
        |> string.join(", ")
      } <> ">(" <> {
        args
        |> list.map(fn(a) { "x" <> to_string(a.0) <> ": " <> pretty_expr3(a.1) })
        |> string.join(", ")
      } <> ") " <> pretty_expr3(body)
    App3(_, _, func, args) ->
      pretty_expr3(func) <> "(" <> {
        args
        |> list.map(pretty_expr3)
        |> string.join(", ")
      } <> ")"
    Upcast3(_, body, a, b) ->
      "(U: " <> pretty_expr3(a) <> "~>" <> pretty_expr3(b) <> ")" <> pretty_expr3(
        body,
      )
    Downcast3(_, body, a, b) ->
      "(D: " <> pretty_expr3(a) <> "~>" <> pretty_expr3(b) <> ")" <> pretty_expr3(
        body,
      )
    TPi3(_, [], args, body) ->
      case list.any(args, fn(a) { contains3(body, a.0) }) {
        True ->
          "Pi(" <> {
            args
            |> list.map(fn(a) {
              "x" <> to_string(a.0) <> ": " <> pretty_expr3(a.1)
            })
            |> string.join(", ")
          } <> ") " <> pretty_expr3(body)
        False ->
          "(" <> {
            args
            |> list.map(fn(a) { pretty_expr3(a.1) })
            |> string.join(", ")
          } <> ")->" <> pretty_expr3(body)
      }
    TPi3(_, imp_args, args, body) ->
      case
        list.any(args, fn(a) { contains3(body, a.0) }) || list.any(
          imp_args,
          contains3(body, _),
        )
      {
        True ->
          "Pi<" <> {
            imp_args
            |> list.map(fn(a) { "x" <> to_string(a) })
            |> string.join(", ")
          } <> ">(" <> {
            args
            |> list.map(fn(a) {
              "x" <> to_string(a.0) <> ": " <> pretty_expr3(a.1)
            })
            |> string.join(", ")
          } <> ") " <> pretty_expr3(body)
        False ->
          "<" <> {
            imp_args
            |> list.map(fn(a) { "x" <> to_string(a) })
            |> string.join(", ")
          } <> ">(" <> {
            args
            |> list.map(fn(a) {
              "x" <> to_string(a.0) <> ": " <> pretty_expr3(a.1)
            })
            |> string.join(", ")
          } <> ")->" <> pretty_expr3(body)
      }
    TType3(_) -> "type"
    TLabelType3(_) -> "labeltype"
    TLabelKind3(_) -> "labelkind"
    TKind3(_) -> "kind"
    TDynamic3(_) -> "dyn"
  }
}

pub fn contains3(e: Expr3, id: Id) -> Bool {
  let c = contains3(_, id)
  case e {
    Ident3(_, _, x) if x == id -> True
    Ident3(_, t, _) -> c(t)
    Builtin3(_, t, _) -> c(t)
    DotAccess3(_, t, e2, _) -> c(t) || c(e2)
    Func3(_, t, _, args, body) ->
      c(t) || list.any(args, fn(a) { c(a.1) }) || c(body)
    App3(_, t, func, args) -> c(t) || c(func) || list.any(args, c)
    Downcast3(_, e2, t1, t2) | Upcast3(_, e2, t1, t2) -> c(e2) || c(t1) || c(t2)
    TPi3(_, _, args, body) -> list.any(args, fn(a) { c(a.1) }) || c(body)
    _ -> False
  }
}

pub fn substitute(id: Id, new: Expr3, t: Expr3) -> Expr3 {
  let sub = substitute(id, new, _)
  case t {
    Int3(_, _) -> t
    Ident3(_, _, x) if id == x -> new
    Builtin3(_, _, _) -> t
    Ident3(_, _, _) -> t
    DotAccess3(pos, t, e, field) -> DotAccess3(pos, sub(t), sub(e), field)
    Func3(pos, t, imp_args, args, body) ->
      case list.contains(imp_args, id) || list.any(args, fn(a) { a.0 == id }) {
        True -> t
        False ->
          Func3(
            pos,
            sub(t),
            imp_args,
            list.map(args, fn(a) { #(a.0, sub(a.1)) }),
            sub(body),
          )
      }
    App3(pos, t, func, args) ->
      App3(pos, sub(t), sub(func), list.map(args, sub))
    Downcast3(pos, e, from, to) -> Downcast3(pos, sub(e), sub(from), sub(to))
    Upcast3(pos, e, from, to) -> Upcast3(pos, sub(e), sub(from), sub(to))
    TPi3(pos, imp_args, args, body) ->
      case list.contains(imp_args, id) || list.any(args, fn(a) { a.0 == id }) {
        True -> t
        False ->
          TPi3(
            pos,
            imp_args,
            list.map(args, fn(a) { #(a.0, substitute(id, new, a.1)) }),
            substitute(id, new, body),
          )
      }
    TDynamic3(_) -> t
    TType3(_) -> t
    TLabelType3(_) -> t
    TLabelKind3(_) -> t
    TKind3(_) -> t
  }
}

fn swap_ident(id: Id, new: Id, t: Expr3) -> Expr3 {
  let sub = swap_ident(id, new, _)
  case t {
    Int3(_, _) -> t
    Ident3(pos, t, x) if id == x -> Ident3(pos, t, new)
    Ident3(_, _, _) -> t
    Builtin3(_, _, _) -> t
    DotAccess3(pos, t, e, field) -> DotAccess3(pos, sub(t), sub(e), field)
    Func3(pos, t, imp_args, args, body) ->
      case
        list.contains(imp_args, new) || list.any(args, fn(a) { a.0 == new })
      {
        True -> t
        False ->
          Func3(
            pos,
            sub(t),
            imp_args,
            list.map(args, fn(a) { #(a.0, sub(a.1)) }),
            sub(body),
          )
      }
    App3(pos, t, func, args) ->
      App3(pos, sub(t), sub(func), list.map(args, sub))
    Downcast3(pos, e, from, to) -> Downcast3(pos, sub(e), sub(from), sub(to))
    Upcast3(pos, e, from, to) -> Upcast3(pos, sub(e), sub(from), sub(to))
    TPi3(pos, imp_args, args, body) ->
      case list.contains(imp_args, id) || list.any(args, fn(a) { a.0 == id }) {
        True -> t
        False ->
          TPi3(
            pos,
            imp_args,
            list.map(args, fn(a) { #(a.0, sub(a.1)) }),
            sub(body),
          )
      }
    TDynamic3(_) -> t
    TType3(_) -> t
    TLabelType3(_) -> t
    TLabelKind3(_) -> t
    TKind3(_) -> t
  }
}

pub fn type_eq(t1: Expr3, t2: Expr3) -> Bool {
  case t1, t2 {
    Ident3(_, _, id1), Ident3(_, _, id2) if id1 == id2 -> True
    Builtin3(_, _, name1), Builtin3(_, _, name2) if name1 == name2 -> True
    DotAccess3(_, _, e1, field1), DotAccess3(_, _, e2, field2) ->
      type_eq(e1, e2) && field1 == field2
    TPi3(_, imp_args1, args1, body1), TPi3(_, imp_args2, args2, body2) ->
      list.length(imp_args1) == list.length(imp_args2) && list.length(args1) == list.length(
        args2,
      ) && list.all(
        list.zip(args1, args2),
        fn(p) { type_eq({ p.0 }.1, { p.1 }.1) },
      ) && type_eq(
        list.fold(
          list.zip(args2, args1),
          list.fold(
            list.zip(imp_args2, imp_args1),
            body2,
            fn(a, b) { swap_ident(b.0, b.1, a) },
          ),
          fn(a, b) { swap_ident({ b.0 }.0, { b.1 }.0, a) },
        ),
        body1,
      )
    App3(_, _, _, _), App3(_, _, _, _) -> todo
    TDynamic3(_), TDynamic3(_) -> True
    TType3(_), TType3(_) -> True
    TLabelType3(_), TLabelType3(_) -> True
    TLabelKind3(_), TLabelKind3(_) -> True
    _, _ -> False
  }
}

pub type Stmt3 {
  Def3(pos: Position, id: Id, val: Expr3)
  Import3(pos: Position, name: String)
}

pub fn pretty_stmt3(s: Stmt3) -> String {
  case s {
    Def3(_, id, val) ->
      "def x" <> to_string(id) <> ": " <> pretty_expr3(typeof(val)) <> " = " <> pretty_expr3(
        val,
      )
    Import3(_, name) -> "import " <> name
  }
}

pub fn typeof(e: Expr3) -> Expr3 {
  case e {
    Int3(pos, _) -> Builtin3(pos, TType3(pos), "int")
    Ident3(_, t, _) -> t
    Builtin3(_, t, _) -> t
    DotAccess3(_, t, _, _) -> t
    Func3(_, t, _, _, _) -> t
    App3(_, t, _, _) -> t
    Downcast3(_, _, _, t) -> t
    Upcast3(_, _, _, t) -> t
    TPi3(_, _, _, b) -> typeof(b)
    TDynamic3(pos) -> TType3(pos)
    TType3(pos) -> TKind3(pos)
    TKind3(_) -> panic("kind is untypeable")
    TLabelType3(pos) -> TLabelKind3(pos)
    TLabelKind3(_) -> panic("labelkind is untypeable")
  }
}

pub type Expr4 {
  Int4(pos: Position, val: Int)
  Ident4(pos: Position, t: Expr4, id: Id)
  Builtin4(pos: Position, t: Expr4, name: String)
  DotAccess4(pos: Position, t: Expr4, expr: Expr4, field: String)
  Func4(pos: Position, t: Expr4, args: List(#(Id, Expr4)), body: Expr4)
  App4(pos: Position, t: Expr4, func: Expr4, args: List(Expr4))
  Upcast4(pos: Position, e: Expr4, from: Expr4, to: Expr4)
  Downcast4(pos: Position, e: Expr4, from: Expr4, to: Expr4)
  TPi4(pos: Position, args: List(#(Id, Expr4)), body: Expr4)
  TType4(pos: Position)
  TKind4(pos: Position)
  TLabelType4(pos: Position)
  TLabelKind4(pos: Position)
  TDynamic4(pos: Position)
}

pub type Stmt4 {
  Def4(pos: Position, id: Id, val: Expr4)
  Import4(pos: Position, name: String)
}

pub fn pretty_expr(e: Expr4) -> String {
  case e {
    Int4(_, i) -> to_string(i)
    Ident4(_, _, id) -> "x" <> to_string(id)
    Builtin4(_, _, name) -> name
    DotAccess4(_, _, e2, field) -> pretty_expr(e2) <> "." <> field
    Func4(_, _, args, body) ->
      "fn(" <> {
        args
        |> list.map(fn(a) { "x" <> to_string(a.0) <> ": " <> pretty_expr(a.1) })
        |> string.join(", ")
      } <> ") " <> pretty_expr(body)
    App4(_, _, func, args) ->
      pretty_expr(func) <> "(" <> {
        args
        |> list.map(pretty_expr)
        |> string.join(", ")
      } <> ")"
    Upcast4(_, body, a, b) ->
      "(U: " <> pretty_expr(a) <> "~>" <> pretty_expr(b) <> ")" <> pretty_expr(
        body,
      )
    Downcast4(_, body, a, b) ->
      "(D: " <> pretty_expr(a) <> "~>" <> pretty_expr(b) <> ")" <> pretty_expr(
        body,
      )
    TPi4(_, args, body) ->
      case list.any(args, fn(a) { contains(body, a.0) }) {
        True ->
          "Pi(" <> {
            args
            |> list.map(fn(a) {
              "x" <> to_string(a.0) <> ": " <> pretty_expr(a.1)
            })
            |> string.join(", ")
          } <> ") " <> pretty_expr(body)
        False ->
          "(" <> {
            args
            |> list.map(fn(a) { pretty_expr(a.1) })
            |> string.join(", ")
          } <> ")->" <> pretty_expr(body)
      }
    TType4(_) -> "type"
    TLabelType4(_) -> "labeltype"
    TLabelKind4(_) -> "labelkind"
    TKind4(_) -> "kind"
    TDynamic4(_) -> "dyn"
  }
}

pub fn contains(e: Expr4, id: Id) -> Bool {
  let c = contains(_, id)
  case e {
    Ident4(_, _, x) if x == id -> True
    Func4(_, t, args, body) ->
      c(t) || list.any(args, fn(a) { c(a.1) }) || c(body)
    App4(_, t, func, args) -> c(t) || c(func) || list.any(args, c)
    Downcast4(_, e2, t1, t2) | Upcast4(_, e2, t1, t2) -> c(e2) || c(t1) || c(t2)
    TPi4(_, args, body) -> list.any(args, fn(a) { c(a.1) }) || c(body)
    _ -> False
  }
}

pub fn pretty_stmt(s: Stmt4) -> String {
  case s {
    Def4(_, id, val) -> "def x" <> to_string(id) <> " = " <> pretty_expr(val)
    Import4(_, name) -> "import " <> name
  }
}
