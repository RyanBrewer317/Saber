import grammar as g
import gleam/string
import gleam/int
import party as p
import concurrent as c

// import gleam/io

pub fn intlit() -> p.Parser(g.SyntaxExpr, Nil) {
  p.map(p.try(p.map(p.many1(p.digit()), string.concat), int.parse), g.IntSyntax)
}

pub fn identstring() -> p.Parser(String, e) {
  use first <- p.do(p.lowercase_letter())
  use rest <- p.do(p.map(
    p.many(p.alt(p.alphanum(), p.char("_"))),
    string.concat,
  ))
  p.return(first <> rest)
}

pub fn identlit() -> p.Parser(g.SyntaxExpr, e) {
  p.map(identstring(), g.IdentSyntax)
}

fn lamlit() -> p.Parser(g.SyntaxExpr, Nil) {
  use _ <- p.do(p.string("fn"))
  use _ <- p.do(p.whitespace())
  use res <- p.do(p.perhaps(p.char("(")))
  case res {
    Ok(_) -> {
      use _ <- p.do(p.whitespace())
      use x <- p.do(identstring())
      use _ <- p.do(p.whitespace())
      use res <- p.do(p.perhaps(p.char(":")))
      use mbargt <- p.do(case res {
        Ok(_) -> p.map(typ(), Ok)
        Error(e) -> p.return(Error(e))
      })
      use _ <- p.do(p.char(")"))
      use e <- p.do(p.lazy(expr))
      p.return(g.LamSyntax(x, mbargt, e))
    }
    Error(Nil) -> {
      use _ <- p.do(p.char("<"))
      use _ <- p.do(p.whitespace())
      use x <- p.do(identstring())
      use _ <- p.do(p.whitespace())
      use _ <- p.do(p.char(">"))
      use e <- p.do(p.lazy(expr))
      p.return(g.TLamSyntax(x, e))
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

fn tvar() -> p.Parser(g.SyntaxType, a) {
  use name <- p.do(identstring())
  p.return(g.TVarSyntax(name))
}

fn tforall() -> p.Parser(g.SyntaxType, a) {
  use _ <- p.do(p.string("fn"))
  use _ <- p.do(p.whitespace())
  use _ <- p.do(p.char("<"))
  use _ <- p.do(p.whitespace())
  use arg <- p.do(identstring())
  use _ <- p.do(p.whitespace())
  use _ <- p.do(p.char(">"))
  use body <- p.do(p.lazy(typ))
  p.return(g.TForallSyntax(arg, body))
}

fn tconstr() -> p.Parser(g.SyntaxType, a) {
  use first <- p.do(p.uppercase_letter())
  use rest <- p.do(
    p.many(p.alt(p.alphanum(), p.char("_")))
    |> p.map(string.concat),
  )
  use _ <- p.do(p.whitespace())
  use mbarg <- p.do(p.perhaps(paren_type()))
  case mbarg {
    Ok(arg) -> p.return(g.TConstrSyntax(first <> rest, [arg]))
    Error(Nil) -> p.return(g.TConstrSyntax(first <> rest, []))
  }
}

pub fn expr() -> p.Parser(g.SyntaxExpr, Nil) {
  use _ <- p.do(p.whitespace())
  use lit <- p.do(p.choice([intlit(), lamlit(), identlit(), paren_expr()]))
  use _ <- p.do(p.whitespace())
  use res <- p.do(p.perhaps(tapp()))
  use _ <- p.do(p.whitespace())
  case res {
    Ok(arg) -> p.return(g.TAppSyntax(lit, arg))
    Error(Nil) -> {
      use res <- p.do(p.perhaps(paren_expr()))
      use _ <- p.do(p.whitespace())
      case res {
        Ok(arg) -> p.return(g.AppSyntax(lit, arg))
        Error(Nil) -> p.return(lit)
      }
    }
  }
}

pub fn stmt() -> p.Parser(g.SyntaxStmt, Nil) {
  use _ <- p.do(p.whitespace())
  use _ <- p.do(p.seq(p.string("def"), p.not(p.alt(p.alphanum(), p.char("_")))))
  use _ <- p.do(p.whitespace())
  use name <- p.do(identstring())
  use _ <- p.do(p.whitespace())
  use res <- p.do(p.perhaps(p.char("(")))
  case res {
    Ok(_) -> {
      use _ <- p.do(p.whitespace())
      use arg <- p.do(identstring())
      use _ <- p.do(p.whitespace())
      use _ <- p.do(p.char(":"))
      use _ <- p.do(p.whitespace())
      use t <- p.do(typ())
      use _ <- p.do(p.whitespace())
      use _ <- p.do(p.char(")"))
      use _ <- p.do(p.whitespace())
      use _ <- p.do(p.char(":"))
      use body <- p.do(expr())
      p.return(g.DeclSyntax(name, arg, t, body))
    }
    Error(Nil) -> {
      use _ <- p.do(p.char("<"))
      use _ <- p.do(p.whitespace())
      use arg <- p.do(identstring())
      use _ <- p.do(p.whitespace())
      use _ <- p.do(p.char(">"))
      use _ <- p.do(p.whitespace())
      use _ <- p.do(p.char(":"))
      use body <- p.do(expr())
      p.return(g.DeclTSyntax(name, arg, body))
    }
  }
}

fn typ() -> p.Parser(g.SyntaxType, a) {
  use _ <- p.do(p.whitespace())
  use lit <- p.do(p.choice([paren_type(), tforall(), tvar(), tconstr()]))
  use _ <- p.do(p.whitespace())
  use res <- p.do(p.perhaps(p.seq(p.string("->"), p.lazy(typ))))
  case res {
    Ok(ret) -> p.return(g.FuncTypeSyntax(lit, ret))
    Error(Nil) -> p.return(lit)
  }
}

pub fn parse(
  src: String,
  to stream: c.Stream(g.SyntaxStmt),
) -> Result(List(g.SyntaxStmt), p.ParseError(Nil)) {
  p.go(
    {
      use e <- p.do(p.many1(p.map(
        stmt(),
        fn(s) {
          // io.debug(s)
          c.write(stream, s)
          s
        },
      )))
      use _ <- p.do(p.end())
      // io.println("finished parsing!\n")
      c.finish(stream)
      p.return(e)
    },
    src,
  )
}
