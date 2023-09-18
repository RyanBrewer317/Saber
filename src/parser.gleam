import core.{
  App1, Def1, Expr1, TFuncType1, Ident1, Int1, Lam1, Stmt1, TApp1, TConstr1,
  TForall1, TLam1, TVar1, Type1, ParseError
}
import gleam/string
import gleam/int
import gleam/result
import party as p
import monad

pub fn intlit() -> p.Parser(Expr1, Nil) {
  use pos <- p.do(p.pos())
  use i <- p.do(p.try(p.map(p.many1(p.digit()), string.concat), int.parse))
  p.return(Int1(pos, i))
}

pub fn identstring() -> p.Parser(String, e) {
  use first <- p.do(p.lowercase_letter())
  use rest <- p.do(p.map(
    p.many(p.alt(p.alphanum(), p.char("_"))),
    string.concat,
  ))
  p.return(first <> rest)
}

pub fn identlit() -> p.Parser(Expr1, e) {
  use pos <- p.do(p.pos())
  use x <- p.do(identstring())
  p.return(Ident1(pos, x))
}

fn lamlit() -> p.Parser(Expr1, Nil) {
  use pos <- p.do(p.pos())
  use _ <- p.do(p.string("fn"))
  use _ <- p.do(ws())
  use res <- p.do(p.perhaps(p.char("(")))
  case res {
    Ok(_) -> {
      use _ <- p.do(ws())
      use x <- p.do(identstring())
      use _ <- p.do(ws())
      use res <- p.do(p.perhaps(p.char(":")))
      use mbargt <- p.do(case res {
        Ok(_) -> p.map(typ(), Ok)
        Error(e) -> p.return(Error(e))
      })
      use _ <- p.do(p.char(")"))
      use e <- p.do(p.lazy(expr))
      p.return(Lam1(pos, x, mbargt, e))
    }
    Error(Nil) -> {
      use _ <- p.do(p.char("<"))
      use _ <- p.do(ws())
      use x <- p.do(identstring())
      use _ <- p.do(ws())
      use _ <- p.do(p.char(">"))
      use e <- p.do(p.lazy(expr))
      p.return(TLam1(pos, x, e))
    }
  }
}

fn paren(p: p.Parser(a, e)) -> p.Parser(a, e) {
  use _ <- p.do(p.char("("))
  use x <- p.do(p)
  use _ <- p.do(p.char(")"))
  p.return(x)
}

fn tapp() {
  use _ <- p.do(p.char("<"))
  use t <- p.do(typ())
  use _ <- p.do(p.char(">"))
  p.return(t)
}

fn paren_expr() {
  paren(p.lazy(expr))
}

fn paren_type() {
  paren(p.lazy(typ))
}

fn tvar() -> p.Parser(Type1, a) {
  use pos <- p.do(p.pos())
  use name <- p.do(identstring())
  p.return(TVar1(pos, name))
}

fn tforall() -> p.Parser(Type1, a) {
  use _ <- p.do(p.string("fn"))
  use _ <- p.do(ws())
  use _ <- p.do(p.char("<"))
  use _ <- p.do(ws())
  use arg <- p.do(identstring())
  use _ <- p.do(ws())
  use _ <- p.do(p.char(">"))
  use body <- p.do(p.lazy(typ))
  p.return(TForall1(arg, body))
}

fn tconstr() -> p.Parser(Type1, a) {
  use first <- p.do(p.uppercase_letter())
  use rest <- p.do(
    p.many(p.alt(p.alphanum(), p.char("_")))
    |> p.map(string.concat),
  )
  use _ <- p.do(ws())
  use mbarg <- p.do(p.perhaps(paren_type()))
  case mbarg {
    Ok(arg) -> p.return(TConstr1(first <> rest, [arg]))
    Error(Nil) -> p.return(TConstr1(first <> rest, []))
  }
}

fn ws() -> p.Parser(Nil, a) {
  let comment = fn() {
    use _ <- p.do(p.string("//"))
    use _ <- p.do(p.many(p.satisfy(fn(c){c != "\n"})))
    p.return("")
  }
  use _ <- p.do(p.many(p.choice([p.char(" "), p.char("\t"), p.char("\n"), comment()])))
  p.return(Nil)
}

pub fn expr() -> p.Parser(Expr1, Nil) {
  use _ <- p.do(ws())
  use lit <- p.do(p.choice([intlit(), lamlit(), identlit(), paren_expr()]))
  use _ <- p.do(ws())
  use pos <- p.do(p.pos())
  use res <- p.do(p.perhaps(tapp()))
  use _ <- p.do(ws())
  case res {
    Ok(arg) -> p.return(TApp1(pos, lit, arg))
    Error(Nil) -> {
      use res <- p.do(p.perhaps(paren_expr()))
      use _ <- p.do(ws())
      case res {
        Ok(arg) -> p.return(App1(pos, lit, arg))
        Error(Nil) -> p.return(lit)
      }
    }
  }
}

pub fn stmt() -> p.Parser(Stmt1, Nil) {
  use _ <- p.do(ws())
  use _ <- p.do(p.seq(p.string("def"), p.not(p.alt(p.alphanum(), p.char("_")))))
  use _ <- p.do(ws())
  use name <- p.do(identstring())
  use _ <- p.do(ws())
  use _ <- p.do(p.char("="))
  use body <- p.do(expr())
  p.return(Def1(name, body))
}

fn typ() -> p.Parser(Type1, a) {
  use _ <- p.do(ws())
  use lit <- p.do(p.choice([paren_type(), tforall(), tvar(), tconstr()]))
  use _ <- p.do(ws())
  use res <- p.do(p.perhaps(p.seq(p.string("->"), p.lazy(typ))))
  case res {
    Ok(ret) -> p.return(TFuncType1(lit, ret))
    Error(Nil) -> p.return(lit)
  }
}

pub fn parse(src: String) -> monad.Monad(List(Stmt1)) {
  p.go(
    {
      use e <- p.do(p.many1(stmt()))
      use _ <- p.do(p.end())
      p.return(e)
    },
    src,
  )
  |> result.map_error(ParseError)
  |> monad.lift
}
