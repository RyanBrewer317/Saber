import gleam/int
import gleam/string
import gleam/list
import gleam/otp/actor

pub type MaybeActor(a) =
  Result(a, actor.StartError)

pub type SyntaxExpr {
  IntSyntax(val: Int)
  IdentSyntax(name: String)
  LamSyntax(arg: String, argt: Result(SyntaxType, Nil), body: SyntaxExpr)
  TLamSyntax(arg: String, body: SyntaxExpr)
  AppSyntax(func: SyntaxExpr, arg: SyntaxExpr)
  TAppSyntax(func: SyntaxExpr, arg: SyntaxType)
}

pub type SyntaxStmt {
  DeclSyntax(name: String, arg: String, argt: SyntaxType, val: SyntaxExpr)
  DeclTSyntax(name: String, arg: String, val: SyntaxExpr)
}

pub type SyntaxType {
  TVarSyntax(name: String)
  TConstrSyntax(name: String, args: List(SyntaxType))
  TForallSyntax(arg: String, body: SyntaxType)
  FuncTypeSyntax(foo: SyntaxType, bar: SyntaxType)
}

pub type Expr {
  Int(id: Id(ExprPh))
  Ident(id: Id(ExprPh), referent: Id(ExprPh))
  Lambda(
    id: Id(ExprPh),
    arg_id: Id(ExprPh),
    argt: Result(Type, Nil),
    body: Expr,
  )
  TLambda(id: Id(ExprPh), arg_id: Id(TypePh), body: Expr)
  App(id: Id(ExprPh), foo: Expr, bar: Expr)
  TApp(id: Id(ExprPh), foo: Expr, bar: Type)
  Upcast(id: Id(ExprPh), e: Expr, from: Type, to: Type)
  Downcast(id: Id(ExprPh), e: Expr, from: Type, to: Type)
  TypeExpr(id: Id(ExprPh), val: Type)
}

pub fn pretty_expr(e: Expr) -> String {
  case e {
    Int(id) -> "intlit" <> int.to_string(id)
    Ident(_id, ref) -> "x" <> int.to_string(ref)
    Lambda(_id, arg_id, mbargt, body) -> {
      let argtstr = case mbargt {
        Ok(t) -> ": " <> pretty_type(t)
        Error(Nil) -> ""
      }
      "fn(x" <> int.to_string(arg_id) <> argtstr <> ") " <> pretty_expr(body)
    }
    TLambda(_id, arg_id, body) ->
      "fn<t" <> int.to_string(arg_id) <> "> " <> pretty_expr(body)
    App(_id, foo, bar) ->
      "(" <> pretty_expr(foo) <> ")(" <> pretty_expr(bar) <> ")"
    TApp(_id, foo, bar) ->
      "(" <> pretty_expr(foo) <> ")<" <> pretty_type(bar) <> ">"
    Upcast(_id, e, from, to) ->
      "#UPCAST " <> pretty_type(from) <> " TO " <> pretty_type(to) <> "# " <> pretty_expr(
        e,
      )
    Downcast(_id, e, from, to) ->
      "%DOWNCAST " <> pretty_type(from) <> " TO " <> pretty_type(to) <> "% " <> pretty_expr(
        e,
      )
    TypeExpr(_id, t) -> pretty_type(t)
  }
}

pub type Stmt {
  Def(id: Id(ExprPh), body: Expr)
}

pub fn pretty_stmt(s: Stmt) -> String {
  case s {
    Def(id, body) ->
      "def x" <> int.to_string(id) <> " = " <> pretty_expr(body)
  }
}

pub type Type {
  TForall(Id(TypePh), Type)
  TVar(Id(TypePh))
  TConstr(String, List(Type))
  TFunc(Type, Type)
  TDynamic
}

pub fn pretty_type(t) {
  case t {
    TForall(arg, t2) -> "fn<t" <> int.to_string(arg) <> "> " <> pretty_type(t2)
    TVar(id) -> "t" <> int.to_string(id)
    TConstr(name, []) -> name
    TConstr(name, args) ->
      name <> "(" <> string.join(list.map(args, pretty_type), ", ") <> ")"
    TFunc(arg, res) -> pretty_type(arg) <> " -> " <> pretty_type(res)
    TDynamic -> "Dyn"
  }
}

pub type PipeMessage(a) {
  Done
  Another(a)
}

pub type Id(a) =
  Int

pub type ExprPh

pub type TypePh

pub type ANFVal {
  ANFInt(Id(ExprPh))
  ANFLocal(Id(ExprPh))
  ANFGlobal(Id(ExprPh))
  ANFTypeVal(Type)
}

pub fn pretty_anf_val(val: ANFVal) -> String {
  case val {
    ANFInt(id) -> "intlit" <> int.to_string(id)
    ANFLocal(id) | ANFGlobal(id) -> "x" <> int.to_string(id)
    ANFTypeVal(t) -> pretty_type(t)
  }
}

pub type ANFExpr {
  // no more distinction between fn() and fn<>
  ANFHalt(id: Id(ExprPh), val: ANFVal)
  ANFFunc(id: Id(ExprPh), args: List(Id(ExprPh)), body: ANFExpr, cont: ANFExpr)
  ANFApp(
    id: Id(ExprPh),
    var: Id(ExprPh),
    foo: Id(ExprPh),
    args: List(ANFVal),
    cont: ANFExpr,
  )
  ANFTuple(id: Id(ExprPh), var: Id(ExprPh), args: List(ANFVal), cont: ANFExpr)
  ANFProj(
    id: Id(ExprPh),
    var: Id(ExprPh),
    tpl: Id(ExprPh),
    idx: Int,
    cont: ANFExpr,
  )
  ANFBox(id: Id(ExprPh), var: Id(ExprPh), val: ANFVal, cont: ANFExpr)
  ANFUnbox(id: Id(ExprPh), var: Id(ExprPh), val: ANFVal, cont: ANFExpr)
}

pub fn pretty_anf_expr(e) {
  case e {
    ANFHalt(_id, val) -> "return " <> pretty_anf_val(val)
    ANFFunc(id, args, body, cont) ->
      "def x" <> int.to_string(id) <> "(" <> {
        args
        |> list.map(fn(arg) { "x" <> int.to_string(arg) })
        |> string.join(", ")
      } <> ") {\n" <> pretty_anf_expr(body) <> "\n}\n" <> pretty_anf_expr(cont)
    ANFTuple(_id, var, args, cont) ->
      "let x" <> int.to_string(var) <> " = (" <> {
        args
        |> list.map(pretty_anf_val)
        |> string.join(", ")
      } <> ")\n" <> pretty_anf_expr(cont)
    ANFApp(_id, var, foo, bars, cont) ->
      "let x" <> int.to_string(var) <> " = x" <> int.to_string(foo) <> "(" <> {
        bars
        |> list.map(pretty_anf_val)
        |> string.join(", ")
      } <> ")\n" <> pretty_anf_expr(cont)
    ANFProj(_id, var, tpl, idx, cont) ->
      "let x" <> int.to_string(var) <> " = x" <> int.to_string(tpl) <> "[" <> int.to_string(
        idx,
      ) <> "]\n" <> pretty_anf_expr(cont)
    ANFBox(_id, var, val, cont) ->
      "let x" <> int.to_string(var) <> " = BOX(" <> pretty_anf_val(val) <> ")\n" <> pretty_anf_expr(
        cont,
      )
    ANFUnbox(_id, var, val, cont) ->
      "let x" <> int.to_string(var) <> " = UNBOX(" <> pretty_anf_val(val) <> ")\n" <> pretty_anf_expr(
        cont,
      )
  }
}

pub type ANFStmt {
  // no more distinction between () and <>
  ANFDef(id: Id(ExprPh), body: ANFExpr)
}

pub fn pretty_anf_stmt(stmt) {
  case stmt {
    ANFDef(id, body) ->
      "def x" <> int.to_string(id) <> " = " <> pretty_anf_expr(body) 
  }
}

pub type CPSVal {
  CPSInt(Id(ExprPh), Type)
  CPSLocal(Id(ExprPh), Type)
  CPSGlobal(Id(ExprPh), Type)
  CPSTuple(List(CPSVal), Type)
  CPSFunc(id: Id(ExprPh), tvars: List(Id(TypePh)), args: List(#(Id(ExprPh), Type)), body: CPSExpr)
}

pub type CPSExpr {
  CPSHalt(t: Type, v: CPSVal)
  CPSCall(foo: Id(ExprPh), targs: List(Type), args: List(CPSVal))
  CPSLet(var: Id(ExprPh), val: CPSVal, cont: CPSExpr)
  CPSProj(var: Id(ExprPh), tpl: Id(ExprPh), idx: CPSVal, cont: CPSExpr)
}

pub type CPSStmt {
  CPSDef(id: Id(ExprPh), body: CPSExpr)
}