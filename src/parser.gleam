import core.{
  App1, Builtin1, Def1, Expr1, Func1, Ident1, Int1, ParseError, Stmt1, TDynamic1,
  TPi1,
}
import gleam/string
import gleam/int
import gleam/result
import gleam/list
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

pub fn builtin() -> p.Parser(Expr1, e) {
  use pos <- p.do(p.pos())
  use _ <- p.do(p.char("#"))
  use s <- p.do(identstring())
  p.return(Builtin1(pos, s))
}

fn lamlit() -> p.Parser(Expr1, Nil) {
  use pos <- p.do(p.pos())
  use _ <- p.do(p.string("fn"))
  use _ <- p.do(ws())
  use res <- p.do(p.perhaps(p.char("<")))
  use imp_args <- p.do(case res {
    Ok(_) -> {
      use out <- p.do(comma_sep({
        use _ <- p.do(ws())
        use x <- p.do(identstring())
        use _ <- p.do(ws())
        p.return(x)
      }))
      use _ <- p.do(p.char(">"))
      p.return(out)
    }
    Error(Nil) -> p.return([])
  })
  use _ <- p.do(ws())
  use _ <- p.do(p.char("("))
  use args <- p.do(comma_sep({
    use _ <- p.do(ws())
    use arg <- p.do(identstring())
    use _ <- p.do(ws())
    use res <- p.do(p.perhaps(p.char(":")))
    use argt <- p.do(case res {
      Ok(_) -> p.lazy(expr)
      Error(_) -> p.return(TDynamic1(pos))
    })
    p.return(#(arg, argt))
  }))
  use _ <- p.do(p.char(")"))
  use e <- p.do(p.lazy(expr))
  p.return(Func1(pos, imp_args, args, e))
}

fn paren(p: p.Parser(a, e)) -> p.Parser(a, e) {
  use _ <- p.do(p.char("("))
  use x <- p.do(p)
  use _ <- p.do(p.char(")"))
  p.return(x)
}

fn paren_expr() {
  paren(p.lazy(expr))
}

fn sep(parser: p.Parser(a, e), by s: p.Parser(b, e)) -> p.Parser(List(a), e) {
  use mb_a <- p.do(p.perhaps(parser))
  case mb_a {
    Ok(a) -> {
      use res <- p.do(p.perhaps(s))
      case res {
        Ok(_) -> {
          use rest <- p.do(sep(parser, by: s))
          p.return([a, ..rest])
        }
        Error(Nil) -> p.return([a])
      }
    }
    Error(Nil) -> p.return([])
  }
}

fn comma_sep(parser) {
  sep(parser, by: p.char(","))
}

fn tforall() -> p.Parser(Expr1, Nil) {
  use pos <- p.do(p.pos())
  use _ <- p.do(p.string("Pi"))
  use _ <- p.do(ws())
  use res <- p.do(p.perhaps(p.char("<")))
  use implicit_args <- p.do(case res {
    Ok(_) -> {
      use out <- p.do(comma_sep({
        use _ <- p.do(ws())
        use arg <- p.do(identstring())
        use _ <- p.do(ws())
        p.return(arg)
      }))
      use _ <- p.do(p.char(">"))
      p.return(out)
    }
    Error(Nil) -> p.return([])
  })
  use _ <- p.do(ws())
  use _ <- p.do(p.char("("))
  use args <- p.do(comma_sep({
    use _ <- p.do(ws())
    use arg <- p.do(identstring())
    use _ <- p.do(ws())
    use _ <- p.do(p.char(":"))
    use t <- p.do(p.lazy(expr))
    p.return(#(arg, t))
  }))
  use _ <- p.do(p.char(")"))
  use body <- p.do(p.lazy(expr))
  p.return(TPi1(pos, implicit_args, args, body))
}

fn ws() -> p.Parser(Nil, a) {
  let comment = fn() {
    use _ <- p.do(p.string("//"))
    use _ <- p.do(p.many(p.satisfy(fn(c) { c != "\n" })))
    p.return("")
  }
  use _ <- p.do(p.many(p.choice([
    p.char(" "),
    p.char("\t"),
    p.char("\n"),
    comment(),
  ])))
  p.return(Nil)
}

pub fn expr() -> p.Parser(Expr1, Nil) {
  use _ <- p.do(ws())
  use lit <- p.do(p.choice([
    tforall(),
    intlit(),
    paren_expr(),
    lamlit(),
    builtin(),
    identlit(),
  ]))
  use _ <- p.do(ws())
  use pos <- p.do(p.pos())
  use res <- p.do(p.perhaps(p.many1({
    use pos2 <- p.do(p.pos())
    use args <- p.do(paren(comma_sep(p.lazy(expr))))
    p.return(#(pos2, args))
  })))
  use _ <- p.do(ws())
  use res2 <- p.do(p.perhaps(p.string("->")))
  case res, res2 {
    Ok(arg), Error(Nil) ->
      p.return(list.fold(arg, lit, fn(a, b) { App1(b.0, a, b.1) }))
    Ok(arg), Ok(_) -> {
      use _ <- p.do(ws())
      use lit2 <- p.do(p.lazy(expr))
      p.return(TPi1(
        pos,
        [],
        [#("_", list.fold(arg, lit, fn(a, b) { App1(b.0, a, b.1) }))],
        lit2,
      ))
    }
    Error(Nil), Ok(_) -> {
      use _ <- p.do(ws())
      use lit2 <- p.do(p.lazy(expr))
      p.return(TPi1(pos, [], [#("_", lit)], lit2))
    }
    Error(Nil), Error(Nil) -> p.return(lit)
  }
}

pub fn def() -> p.Parser(Stmt1, Nil) {
  use _ <- p.do(ws())
  use pos <- p.do(p.pos())
  use _ <- p.do(p.string("def"))
  use _ <- p.do(p.not(p.alt(p.alphanum(), p.char("_"))))
  use _ <- p.do(ws())
  use name <- p.do(identstring())
  use _ <- p.do(ws())
  use _ <- p.do(p.char("="))
  use body <- p.do(expr())
  p.return(Def1(pos, name, body))
}

pub fn stmt() -> p.Parser(Stmt1, Nil) {
  def()
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
