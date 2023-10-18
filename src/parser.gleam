import core.{
  App1, Builtin1, CouldntOpenFile, Def1, DotAccess1, Error, Expr1, Func1, Ident1,
  Import1, Int1, Library0, Library1, Module0, Module1, ParseError, Stmt1,
  TDynamic1, TPi1,
}
import gleam/string
import gleam/int
import gleam/result
import gleam/list
import gleam/map.{Map}
import party as p
import monad.{Monad, State, do, monadic_map, return, try}
import simplifile

pub fn parse_lib(lib: Library0, state: State) -> Monad(Library1) {
  use entry, state2 <- do(parse_module(lib.entry, state))
  return(Library1(lib.path, entry), state2)
}

fn parse_module(mod: Module0, state: State) -> Monad(Module1) {
  use parses, state2 <- monadic_map(
    mod.files,
    state,
    fn(filename, state) {
      use code, state2 <- try(
        simplifile.read(mod.path <> "/" <> filename)
        |> result.replace_error(CouldntOpenFile(filename)),
        state,
      )
      parse(mod.path <> "/" <> filename, code, state2)
    },
  )

  use subs, state3 <- monadic_map(mod.subs, state2, parse_module)
  let #(ast, symbol_table) =
    list.fold(
      parses,
      #([], map.new()),
      fn(curr, new) { #(list.append(new.0, curr.0), map.merge(curr.1, new.1)) },
    )
  return(
    Module1(mod.path, subs, symbol_table, mod.files, list.reverse(ast)),
    state3,
  )
}

fn intlit() -> p.Parser(Expr1, Nil) {
  use pos <- p.do(p.pos())
  use i <- p.do(p.try(p.map(p.many1(p.digit()), string.concat), int.parse))
  p.return(Int1(pos, i))
}

fn identstring() -> p.Parser(String, e) {
  use first <- p.do(p.lowercase_letter())
  use rest <- p.do(p.map(
    p.many(p.alt(p.alphanum(), p.char("_"))),
    string.concat,
  ))
  p.return(first <> rest)
}

fn identlit(path: String) -> p.Parser(Expr1, e) {
  use pos <- p.do(p.pos())
  use x <- p.do(identstring())
  p.return(Ident1(pos, path, x))
}

fn builtin() -> p.Parser(Expr1, e) {
  use pos <- p.do(p.pos())
  use _ <- p.do(p.char("#"))
  use s <- p.do(identstring())
  p.return(Builtin1(pos, s))
}

fn lamlit(path: String) -> p.Parser(Expr1, Nil) {
  use pos <- p.do(p.pos())
  use _ <- p.do(p.string("fn"))
  use _ <- p.do(ws())
  use res <- p.do(p.perhaps(p.char("[")))
  use imp_args <- p.do(case res {
    Ok(_) -> {
      use out <- p.do(comma_sep({
        use _ <- p.do(ws())
        use x <- p.do(identstring())
        use _ <- p.do(ws())
        p.return(x)
      }))
      use _ <- p.do(p.char("]"))
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
      Ok(_) -> p.lazy(expr(path))
      Error(_) -> p.return(TDynamic1(pos))
    })
    p.return(#(arg, argt))
  }))
  use _ <- p.do(p.char(")"))
  use e <- p.do(p.lazy(expr(path)))
  p.return(Func1(pos, imp_args, args, e))
}

fn paren(p: p.Parser(a, e)) -> p.Parser(a, e) {
  use _ <- p.do(p.char("("))
  use x <- p.do(p)
  use _ <- p.do(p.char(")"))
  p.return(x)
}

fn paren_expr(path) {
  paren(p.lazy(expr(path)))
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

fn tforall(path: String) -> p.Parser(Expr1, Nil) {
  use pos <- p.do(p.pos())
  use _ <- p.do(p.string("Pi"))
  use _ <- p.do(ws())
  use res <- p.do(p.perhaps(p.char("[")))
  use implicit_args <- p.do(case res {
    Ok(_) -> {
      use out <- p.do(comma_sep({
        use _ <- p.do(ws())
        use arg <- p.do(identstring())
        use _ <- p.do(ws())
        p.return(arg)
      }))
      use _ <- p.do(p.char("]"))
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
    use t <- p.do(p.lazy(expr(path)))
    p.return(#(arg, t))
  }))
  use _ <- p.do(p.char(")"))
  use body <- p.do(p.lazy(expr(path)))
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

fn postfix(e: Expr1, path: String) -> p.Parser(Expr1, Nil) {
  use pos <- p.do(p.pos())
  use mb_call <- p.do(p.perhaps(paren(comma_sep(p.lazy(expr(path))))))
  case mb_call {
    Ok(args) -> {
      let e = App1(pos, e, args)
      use _ <- p.do(ws())
      postfix(e, path)
    }
    Error(Nil) -> {
      use mb_arr <- p.do(p.perhaps(p.string("->")))
      case mb_arr {
        Ok(_) -> {
          use rhs <- p.do(p.lazy(expr(path)))
          let e = TPi1(pos, [], [#("_", e)], rhs)
          use _ <- p.do(ws())
          postfix(e, path)
        }
        Error(Nil) -> {
          use mb_dot <- p.do(p.perhaps(p.char(".")))
          use _ <- p.do(ws())
          case mb_dot {
            Ok(_) -> {
              use _ <- p.do(ws())
              use fieldname <- p.do(identstring())
              let e = DotAccess1(pos, e, fieldname)
              use _ <- p.do(ws())
              postfix(e, path)
            }
            Error(Nil) -> p.return(e)
          }
        }
      }
    }
  }
}

fn expr(path: String) -> fn() -> p.Parser(Expr1, Nil) {
  fn() {
    use _ <- p.do(ws())
    use lit <- p.do(p.choice([
      tforall(path),
      intlit(),
      paren_expr(path),
      lamlit(path),
      builtin(),
      identlit(path),
    ]))
    use _ <- p.do(ws())
    use res <- p.do(postfix(lit, path))
    use _ <- p.do(ws())
    p.return(res)
  }
}

fn import_stmt(path) -> p.Parser(#(Stmt1, String, Expr1), e) {
  use pos <- p.do(p.pos())
  use _ <- p.do(p.string("import"))
  use _ <- p.do(p.not(p.alt(p.alphanum(), p.char("_"))))
  use _ <- p.do(ws())
  use name <- p.do(identstring())
  p.return(#(Import1(pos, name), name, Ident1(pos, path, "dyn")))
}

fn fn_def(path: String) -> p.Parser(#(Stmt1, String, Expr1), Nil) {
  use _ <- p.do(ws())
  use pos <- p.do(p.pos())
  use _ <- p.do(p.string("fn"))
  use _ <- p.do(p.not(p.alt(p.alphanum(), p.char("_"))))
  use _ <- p.do(ws())
  use name <- p.do(identstring())
  use _ <- p.do(ws())
  use res <- p.do(p.perhaps(p.char("[")))
  use implicit_args <- p.do(case res {
    Ok(_) -> {
      use out <- p.do(comma_sep({
        use _ <- p.do(ws())
        use arg <- p.do(identstring())
        use _ <- p.do(ws())
        p.return(arg)
      }))
      use _ <- p.do(p.char("]"))
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
    use t <- p.do(p.lazy(expr(path)))
    p.return(#(arg, t))
  }))
  use _ <- p.do(p.char(")"))
  use _ <- p.do(ws())
  use mb_ret_t <- p.do(p.perhaps(p.string("->")))
  use ret_t <- p.do(case mb_ret_t {
    Ok(_) -> expr(path)()
    Error(Nil) -> p.return(Ident1(pos, path, "dyn"))
  })
  let t = TPi1(pos, implicit_args, args, ret_t)
  use _ <- p.do(p.char("{"))
  use body <- p.do(expr(path)())
  use _ <- p.do(p.char("}"))
  use _ <- p.do(p.lazy(ws))
  p.return(#(Def1(pos, name, Func1(pos, implicit_args, args, body)), name, t))
}

fn stmt(path: String) -> p.Parser(#(Stmt1, String, Expr1), Nil) {
  p.choice([fn_def(path), import_stmt(path)])
}

fn parse(
  path: String,
  src: String,
  state: State,
) -> Monad(#(List(Stmt1), Map(String, Expr1))) {
  let res =
    p.go(
      {
        use res <- p.do(p.many1(stmt(path)))
        use _ <- p.do(p.end())
        let #(stmts, symbol_table) =
          list.fold(
            res,
            #([], map.new()),
            fn(curr, r) { #([r.0, ..curr.0], map.insert(curr.1, r.1, r.2)) },
          )
        p.return(#(stmts, symbol_table))
      },
      src,
    )
    |> result.map_error(ParseError(path, _))
  try(res, state, return)
}
