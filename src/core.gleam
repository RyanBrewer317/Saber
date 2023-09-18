import gleam/int.{to_string}
import gleam/list.{map}
import gleam/string.{join}
import party.{Position, ParseError}
import gleam/erlang/file.{Reason}

pub type Error {
  CouldntOpenFile(Reason)
  ParseError(ParseError(Nil))
  Undefined(Position, String)
  TypeError(Type3, Type3)
  CallingNonFunction(Type3)
  CallingNonForall(Type3)
  TypeAtRuntime(Type3)
}

pub fn pretty_err(e: Error) -> String {
  case e {
    CouldntOpenFile(r) -> "Error! Couldn't open the Saber file! Reason: " <> string.inspect(r)
    ParseError(p) -> "Parse error! " <> string.inspect(p)
    Undefined(pos, s) -> "Error! Undefined variable " <> s <> " at position " <> string.inspect(pos)
    TypeError(t1, t2) -> "Type error! Couldn't unify " <> pretty_type(t1) <> " and " <> pretty_type(t2)
    CallingNonFunction(t) -> "Type error! Calling non-function of type " <> pretty_type(t) <> " as if it were a function"
    CallingNonForall(t) -> "Type error! Calling non-forall of type " <> pretty_type(t) <> " as if it were a type-abstraction"
    TypeAtRuntime(t) -> "Runtime error! Found a type at runtime, " <> pretty_type(t)
  }
}

pub type Expr1 {
  Int1(pos: Position, val: Int)
  Ident1(pos: Position, name: String)
  Lam1(pos: Position, arg: String, argt: Result(Type1, Nil), body: Expr1)
  TLam1(pos: Position, arg: String, body: Expr1)
  App1(pos: Position, func: Expr1, arg: Expr1)
  TApp1(pos: Position, func: Expr1, arg: Type1)
}

pub type Type1 {
  TVar1(pos: Position, name: String)
  TConstr1(name: String, args: List(Type1))
  TForall1(arg: String, body: Type1)
  TFuncType1(foo: Type1, bar: Type1)
}

pub type Stmt1 {
  Def1(name: String, val: Expr1)
}

pub type Id =
  Int

pub type Expr2 {
  Int2(pos: Position, val: Int)
  Ident2(pos: Position, id: Id)
  Lam2(pos: Position, arg: Id, argt: Type2, body: Expr2)
  TLam2(pos: Position, arg: Id, body: Expr2)
  App2(pos: Position, func: Expr2, arg: Expr2)
  TApp2(pos: Position, func: Expr2, arg: Type2)
}

pub type Type2 {
  TVar2(name: Id)
  TConstr2(name: String, args: List(Type2))
  TForall2(arg: Id, body: Type2)
  TFuncType2(foo: Type2, bar: Type2)
  TDynamic2
}

pub type Stmt2 {
  Def2(name: Id, val: Expr2)
}

pub type Expr3 {
  Int3(pos: Position, val: Int)
  Ident3(pos: Position, t: Type3, id: Id)
  Lam3(pos: Position, t: Type3, arg: Id, argt: Type3, body: Expr3)
  TLam3(pos: Position, t: Type3, arg: Id, body: Expr3)
  App3(pos: Position, t: Type3, func: Expr3, arg: Expr3)
  TApp3(pos: Position, t: Type3, func: Expr3, arg: Type3)
  Upcast3(pos: Position, e: Expr3, from: Type3, to: Type3)
  Downcast3(pos: Position, e: Expr3, from: Type3, to: Type3)
}

pub fn pretty_expr(e: Expr3) -> String {
  case e {
    Int3(_, i) -> to_string(i)
    Ident3(_, _, id) -> "x" <> to_string(id)
    Lam3(_, _, arg_id, argt, body) -> "fn(x" <> to_string(arg_id) <> ": " <> pretty_type(argt) <> ") " <> pretty_expr(body)
    TLam3(_, _, arg_id, body) -> "fn<a" <> to_string(arg_id) <> "> " <> pretty_expr(body)
    App3(_, _, foo, bar) -> pretty_expr(foo) <> "(" <> pretty_expr(bar) <> ")"
    TApp3(_, _, foo, bar) -> pretty_expr(foo) <> "<" <> pretty_type(bar) <> ">"
    Upcast3(_, body, _, t) -> "(BOX: " <> pretty_type(t) <> ")" <> pretty_expr(body)
    Downcast3(_, body, _, t) -> "(UNBOX: " <> pretty_type(t) <> ")" <> pretty_expr(body)
  }
}

pub type Type3 {
  TVar3(name: Id)
  TConstr3(name: String, args: List(Type3))
  TForall3(arg: Id, body: Type3)
  TFuncType3(foo: Type3, bar: Type3)
  TDynamic3
}

pub fn pretty_type(t: Type3) -> String {
  case t {
    TVar3(id) -> "a" <> to_string(id)
    TConstr3(s, []) -> s
    TConstr3(s, ts) -> s <> "(" <> join(map(ts, pretty_type), ", ") <> ")"
    TForall3(arg, body) -> "forall a" <> to_string(arg) <> ". " <> pretty_type(body)
    TFuncType3(foo, bar) -> "(" <> pretty_type(foo) <> ") -> " <> pretty_type(bar)
    TDynamic3 -> "Dyn"
  }
}

pub type Stmt3 {
  Def3(id: Id, val: Expr3)
}

pub fn pretty_stmt(s: Stmt3) -> String {
  case s {
    Def3(id, val) -> "def x" <> to_string(id) <> " = " <> pretty_expr(val)
  }
}