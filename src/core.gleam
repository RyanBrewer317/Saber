import gleam/int.{to_string}
import gleam/string
import gleam/list
import gleam/bool.{guard}
import gleam/map.{Map}
import gleam/io
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

pub type Library4 {
  Library4(path: String, entry: Module4)
}

pub type Module4 {
  Module4(
    path: String,
    subs: List(Module4),
    symbol_table: Map(String, Expr4),
    files: List(String),
    ast: List(Stmt4),
  )
}

pub type Error {
  CouldntOpenFile(String)
  ParseError(String, ParseError(Nil))
  Undefined(String, Position, String)
  TypeError(Position, Expr3, Expr3)
  UnknownStructField(Position, Expr3, String)
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
    UnknownStructField(pos, t, field) ->
      "Type Error! Trying to access field " <> field <> " on a value of type " <> pretty_expr3(
        t,
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
  Struct1(pos: Position, fields: List(#(String, Expr1)))
  TStruct1(pos: Position, fields: List(#(String, Expr1)))
  TInter1(pos: Position, args: List(#(String, Expr1)))
}

pub type Stmt1 {
  Def1(pos: Position, name: String, t: Expr1, val: Expr1)
  Import1(pos: Position, name: String)
}

pub type Id =
  Int

pub type Expr2 {
  Int2(pos: Position, val: Int)
  Ident2(pos: Position, id: Ident)
  Builtin2(pos: Position, name: String)
  ModuleAccess2(pos: Position, module: String, name: String)
  StructAccess2(pos: Position, expr: Expr2, name: String)
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
  TDynamic2(pos: Position)
  Struct2(pos: Position, fields: List(#(String, Expr2)))
  TStruct2(pos: Position, fields: List(#(String, Expr2)))
  TInter2(pos: Position, args: List(#(Id, Expr2)))
}

pub type Stmt2 {
  Def2(pos: Position, name: String, t: Expr2, val: Expr2)
  Import2(pos: Position, name: String)
}

pub type Expr3 {
  Int3(pos: Position, val: Int)
  Ident3(pos: Position, t: Expr3, id: Ident)
  Builtin3(pos: Position, t: Expr3, name: String)
  ModuleAccess3(pos: Position, t: Expr3, module: String, name: String)
  StructAccess3(pos: Position, t: Expr3, expr: Expr3, name: String)
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
  TDynamic3(pos: Position)
  Struct3(pos: Position, t: Expr3, fields: List(#(String, Expr3)))
  TStruct3(pos: Position, fields: List(#(String, Expr3)))
  TInter3(pos: Position, args: List(#(Id, Expr3)))
}

pub fn mod_name_from_path(path: String) -> String {
  let assert Ok(name) = list.last(string.split(path, "/"))
  name
}

pub fn pretty_expr3(e: Expr3) -> String {
  case e {
    Int3(_, i) -> to_string(i)
    Ident3(_, _, Local(id)) -> "x" <> to_string(id)
    Ident3(_, _, Global(name)) -> name
    Builtin3(_, _, name) -> name
    ModuleAccess3(_, _, path, field) -> mod_name_from_path(path) <> "." <> field
    StructAccess3(_, _, e, field) -> pretty_expr3(e) <> "." <> field
    Func3(_, _, [], args, body) ->
      "fn(" <> {
        args
        |> list.map(fn(a) { "x" <> to_string(a.0) <> ": " <> pretty_expr3(a.1) })
        |> string.join(", ")
      } <> ") " <> pretty_expr3(body)
    Func3(_, _, imp_args, args, body) ->
      "fn[" <> {
        imp_args
        |> list.map(fn(a) { "x" <> to_string(a) })
        |> string.join(", ")
      } <> "](" <> {
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
          "Pi[" <> {
            imp_args
            |> list.map(fn(a) { "x" <> to_string(a) })
            |> string.join(", ")
          } <> "](" <> {
            args
            |> list.map(fn(a) {
              "x" <> to_string(a.0) <> ": " <> pretty_expr3(a.1)
            })
            |> string.join(", ")
          } <> ") " <> pretty_expr3(body)
        False ->
          "[" <> {
            imp_args
            |> list.map(fn(a) { "x" <> to_string(a) })
            |> string.join(", ")
          } <> "](" <> {
            args
            |> list.map(fn(a) {
              "x" <> to_string(a.0) <> ": " <> pretty_expr3(a.1)
            })
            |> string.join(", ")
          } <> ")->" <> pretty_expr3(body)
      }
    TType3(_) -> "type"
    TKind3(_) -> "kind"
    TDynamic3(_) -> "dyn"
    Struct3(_, _, fields) ->
      "{" <> {
        fields
        |> list.map(fn(f) { f.0 <> ": " <> pretty_expr3(f.1) })
        |> string.join(", ")
      } <> "}"
    TStruct3(_, fields) ->
      "struct{" <> {
        fields
        |> list.map(fn(f) { f.0 <> ": " <> pretty_expr3(f.1) })
        |> string.join(", ")
      } <> "}"
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
    ModuleAccess3(_, t, _, _) -> c(t)
    Func3(_, t, _, args, body) ->
      c(t) || list.any(args, fn(a) { c(a.1) }) || c(body)
    App3(_, t, func, args) -> c(t) || c(func) || list.any(args, c)
    Downcast3(_, e2, t1, t2) | Upcast3(_, e2, t1, t2) -> c(e2) || c(t1) || c(t2)
    TPi3(_, _, args, body) -> list.any(args, fn(a) { c(a.1) }) || c(body)
    Struct3(_, t, fields) -> c(t) || list.any(fields, fn(f) { c(f.1) })
    StructAccess3(_, t, e, _) -> c(t) || c(e)
    TStruct3(_, fields) -> list.any(fields, fn(f) { c(f.1) })
    TInter3(_, ts) -> list.any(ts, fn(t) { c(t.1) })
    _ -> False
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
    ModuleAccess3(pos, t, name, field) ->
      ModuleAccess3(pos, sub(t), name, field)
    StructAccess3(pos, t, e, field) -> StructAccess3(pos, sub(t), sub(e), field)
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
    TKind3(_) -> t
    Struct3(pos, t, fields) ->
      Struct3(pos, sub(t), list.map(fields, fn(f) { #(f.0, sub(f.1)) }))
    TStruct3(pos, fields) ->
      TStruct3(pos, list.map(fields, fn(f) { #(f.0, sub(f.1)) }))
    TInter3(pos, ts) -> TInter3(pos, list.map(ts, fn(t) { #(t.0, sub(t.1)) }))
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
    ModuleAccess3(pos, t, name, field) ->
      ModuleAccess3(pos, sub(t), name, field)
    StructAccess3(pos, t, e, field) -> StructAccess3(pos, sub(t), sub(e), field)
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
    TKind3(_) -> t
    Struct3(pos, t, fields) ->
      Struct3(pos, sub(t), list.map(fields, fn(f) { #(f.0, sub(f.1)) }))
    TStruct3(pos, fields) ->
      TStruct3(pos, list.map(fields, fn(f) { #(f.0, sub(f.1)) }))
    TInter3(pos, ts) -> TInter3(pos, list.map(ts, fn(t) { #(t.0, sub(t.1)) }))
  }
}

pub fn type_eq(t1: Expr3, t2: Expr3) -> Bool {
  case t1, t2 {
    Ident3(_, _, id1), Ident3(_, _, id2) if id1 == id2 -> True
    Builtin3(_, _, name1), Builtin3(_, _, name2) if name1 == name2 -> True
    ModuleAccess3(_, _, name1, field1), ModuleAccess3(_, _, name2, field2) ->
      name1 == name2 && field1 == field2
    TPi3(_, imp_args1, args1, body1), TPi3(_, imp_args2, args2, body2) -> {
      use <- guard(
        when: list.length(imp_args1) != list.length(imp_args2),
        return: False,
      )
      use <- guard(
        when: list.length(args1) != list.length(args2),
        return: False,
      )
      let imp_args1_in_args2 = 
        list.fold(
          list.zip(imp_args1, imp_args2),
          args2,
          fn(args, a) {
            let #(name1, swapee) = a
            list.map(
              args,
              fn(a2) {
                let #(name2, ty2) = a2
                let name = case name2 == swapee {
                  True -> name1
                  False -> name2
                }
                #(name, swap_ident(from: swapee, to: name1, in: ty2))
              },
            )
          },
        )
      let swapped =
        list.fold(
          list.zip(args1, args2),
          imp_args1_in_args2,
          fn(args, a) {
            let #(#(name1, _), #(swapee, _)) = a
            list.map(
              args,
              fn(a2) {
                let #(name2, ty2) = a2
                let name = case name2 == swapee {
                  True -> name1
                  False -> name2
                }
                #(name, swap_ident(from: swapee, to: name1, in: ty2))
              },
            )
          },
        )
      swapped == args1 && type_eq(
        io.debug(list.fold(
          list.zip(args2, args1),
          list.fold(
            list.zip(imp_args2, imp_args1),
            body2,
            fn(bdy2, a) { 
              let #(iarg2, iarg1) = a
              swap_ident(from: iarg2, to: iarg1, in: bdy2) },
          ),
          fn(bdy2, a) { 
            let #(arg2, arg1) = a
            swap_ident(from: arg2.0, to: arg1.0, in: bdy2) },
        )),
        io.debug(body1),
      )
    }
    App3(_, _, func1, args1), App3(_, _, func2, args2) ->
      type_eq(func1, func2) && list.all(
        list.zip(args1, args2),
        fn(p) { type_eq(p.0, p.1) },
      )
    TDynamic3(_), TDynamic3(_) -> True
    TType3(_), TType3(_) -> True
    TStruct3(_, fields1), TStruct3(_, fields2) ->
      list.all(
        list.zip(fields1, fields2),
        fn(pair) {
          let #(#(n1, t1), #(n2, t2)) = pair
          n1 == n2 && type_eq(t1, t2)
        },
      )
    TInter3(_, ts1), TInter3(_, ts2) ->
      list.all(
        list.zip(ts1, ts2),
        fn(pair) {
          let #(#(_, t1), #(_, t2)) = pair
          type_eq(t1, t2)
        },
      )
    _, _ -> False
  }
}

pub type Stmt3 {
  Def3(pos: Position, id: String, t: Expr3, val: Expr3)
  Import3(pos: Position, name: String)
}

pub fn pretty_stmt3(s: Stmt3) -> String {
  case s {
    Def3(_, name, _, Func3(_, TPi3(_, _, _, rett), [], args, body)) ->
      "fn " <> name <> "(" <> string.join(
        list.map(
          args,
          fn(a) { "x" <> to_string(a.0) <> ": " <> pretty_expr3(a.1) },
        ),
        ", ",
      ) <> ") -> " <> pretty_expr3(rett) <> " {\n  " <> pretty_expr3(body) <> "\n}"
    Def3(_, name, _, Func3(_, TPi3(_, _, _, rett), imp_args, args, body)) ->
      "fn " <> name <> "[" <> string.join(
        list.map(imp_args, fn(a) { "x" <> to_string(a) }),
        ",",
      ) <> "](" <> string.join(
        list.map(
          args,
          fn(a) { "x" <> to_string(a.0) <> ": " <> pretty_expr3(a.1) },
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
    ModuleAccess3(_, t, _, _) -> t
    StructAccess3(_, t, _, _) -> t
    Func3(_, t, _, _, _) -> t
    App3(_, t, _, _) -> t
    Downcast3(_, _, _, t) -> t
    Upcast3(_, _, _, t) -> t
    TPi3(_, _, _, b) -> typeof(b)
    TDynamic3(pos) -> TType3(pos)
    TType3(pos) -> TKind3(pos)
    TKind3(_) -> panic("kind is untypeable")
    Struct3(_, t, _) -> t
    TStruct3(pos, _) -> TType3(pos)
    TInter3(pos, _) -> TType3(pos)
  }
}

pub type Expr4 {
  Int4(pos: Position, val: Int)
  Ident4(pos: Position, t: Expr4, id: Ident)
  Builtin4(pos: Position, t: Expr4, name: String)
  ModuleAccess4(pos: Position, t: Expr4, module: String, field: String)
  StructAccess4(pos: Position, t: Expr4, e: Expr4, field: String)
  Func4(pos: Position, t: Expr4, args: List(#(Id, Expr4)), body: Expr4)
  App4(pos: Position, t: Expr4, func: Expr4, args: List(Expr4))
  Upcast4(pos: Position, e: Expr4, from: Expr4, to: Expr4)
  Downcast4(pos: Position, e: Expr4, from: Expr4, to: Expr4)
  TPi4(pos: Position, args: List(#(Id, Expr4)), body: Expr4)
  TType4(pos: Position)
  TKind4(pos: Position)
  TDynamic4(pos: Position)
  Struct4(pos: Position, t: Expr4, fields: List(#(String, Expr4)))
  TStruct4(pos: Position, fields: List(#(String, Expr4)))
  TInter4(pos: Position, ts: List(#(Id, Expr4)))
}

pub type Stmt4 {
  Def4(pos: Position, id: String, t: Expr4, val: Expr4)
  Import4(pos: Position, name: String)
}

pub fn pretty_expr(e: Expr4) -> String {
  case e {
    Int4(_, i) -> to_string(i)
    Ident4(_, _, Local(id)) -> "x" <> to_string(id)
    Ident4(_, _, Global(name)) -> name
    Builtin4(_, _, name) -> name
    ModuleAccess4(_, _, path, field) -> mod_name_from_path(path) <> "." <> field
    StructAccess4(_, _, e, field) -> pretty_expr(e) <> "." <> field
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
    TKind4(_) -> "kind"
    TDynamic4(_) -> "dyn"
    Struct4(_, _, fields) ->
      "{" <> string.join(
        list.map(fields, fn(f) { f.0 <> ": " <> pretty_expr(f.1) }),
        ", ",
      ) <> "}"
    TStruct4(_, fields) ->
      "struct{ " <> string.join(
        list.map(fields, fn(f) { f.0 <> ": " <> pretty_expr(f.1) }),
        ", ",
      ) <> "}"
    TInter4(_, ts) ->
      "Inter{" <> string.join(
        list.map(
          ts,
          fn(t) { "x" <> to_string(t.0) <> ": " <> pretty_expr(t.1) },
        ),
        ", ",
      ) <> "}"
  }
}

pub fn contains(e: Expr4, id: Id) -> Bool {
  let c = contains(_, id)
  case e {
    Ident4(_, _, Local(x)) if x == id -> True
    Ident4(_, t, _) -> c(t)
    Func4(_, t, args, body) ->
      c(t) || list.any(args, fn(a) { c(a.1) }) || c(body)
    App4(_, t, func, args) -> c(t) || c(func) || list.any(args, c)
    Downcast4(_, e2, t1, t2) | Upcast4(_, e2, t1, t2) -> c(e2) || c(t1) || c(t2)
    TPi4(_, args, body) -> list.any(args, fn(a) { c(a.1) }) || c(body)
    Builtin4(_, t, _) -> c(t)
    ModuleAccess4(_, t, _, _) -> c(t)
    Struct4(_, t, fields) -> c(t) || list.any(fields, fn(f) { c(f.1) })
    StructAccess4(_, t, e, _) -> c(t) || c(e)
    TStruct4(_, fields) -> list.any(fields, fn(f) { c(f.1) })
    TInter4(_, ts) -> list.any(ts, fn(t) { c(t.1) })
    _ -> False
  }
}

pub fn pretty_stmt(s: Stmt4) -> String {
  case s {
    Def4(_, name, _, Func4(_, TPi4(_, _, rett), args, body)) ->
      "fn " <> name <> "(" <> string.join(
        list.map(
          args,
          fn(a) { "x" <> to_string(a.0) <> ": " <> pretty_expr(a.1) },
        ),
        ", ",
      ) <> ") -> " <> pretty_expr(rett) <> " {\n  " <> pretty_expr(body) <> "\n}"
    Import4(_, name) -> "import " <> name
  }
}
