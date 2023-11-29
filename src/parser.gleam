import core.{
  type Error, type Expr1, type Library0, type Library1, type Module0,
  type Module1, type Monad, type State, type Stmt1, App1, Arg1, ArgMode,
  Builtin1, CouldntOpenFile, Def1, Func1, Ident1, Int1, Inter1, Library1,
  Module1, ParseError, Projection1, TInter1, TPi1, monadic_map,
}
import gleam/string
import gleam/int
import gleam/result
import gleam/list
import gleam/map.{type Map}
import party.{
  type Parser, alphanum, alt, char, choice, digit, do, end, lazy,
  lowercase_letter, many, many1, not, perhaps, pos, return, satisfy, try,
}
import simplifile

pub fn parse_lib(lib: Library0, state: State) -> Monad(Library1) {
  use entry, state <- core.do(parse_module(lib.entry, state))
  core.return(Library1(lib.path, entry), state)
}

fn parse_module(mod: Module0, state: State) -> Monad(Module1) {
  use parses, state <- monadic_map(
    mod.files,
    state,
    fn(filename, state) {
      use code, state <- core.try(
        simplifile.read(filename)
        |> result.replace_error(CouldntOpenFile(filename)),
        state,
      )
      parse(filename, code, state)
    },
  )

  use subs, state <- monadic_map(mod.subs, state, parse_module)
  let #(ast, symbol_table) =
    list.fold(
      parses,
      #([], map.new()),
      fn(curr, new) { #(list.append(new.0, curr.0), map.merge(curr.1, new.1)) },
    )
  core.return(
    Module1(mod.path, subs, symbol_table, mod.files, list.reverse(ast)),
    state,
  )
}

fn intstring() -> Parser(Int, Nil) {
  try(party.map(many1(digit()), string.concat), int.parse)
}

fn intlit() -> Parser(Expr1, Nil) {
  use pos <- do(pos())
  use i <- do(try(party.map(many1(digit()), string.concat), int.parse))
  return(Int1(pos, i))
}

fn identstring() -> Parser(String, e) {
  use first <- do(lowercase_letter())
  use rest <- do(party.map(many(alt(alphanum(), char("_"))), string.concat))
  return(first <> rest)
}

fn identlit(path: String) -> Parser(Expr1, e) {
  use pos <- do(pos())
  use x <- do(identstring())
  return(Ident1(pos, path, x))
}

fn builtin() -> Parser(Expr1, e) {
  use pos <- do(pos())
  use _ <- do(char("#"))
  use s <- do(identstring())
  return(Builtin1(pos, s))
}

fn lamlit(path: String) -> Parser(Expr1, Nil) {
  use pos <- do(pos())
  use _ <- do(party.string("fn"))
  use _ <- do(ws())
  use _ <- do(char("("))
  use args <- do(comma_sep({
    use _ <- do(ws())
    use res <- do(perhaps(char("@")))
    let imp = result.is_ok(res)
    use _ <- do(ws())
    use arg <- do(identstring())
    use _ <- do(ws())
    use res <- do(perhaps(char(":")))
    use argt <- do(case res {
      Ok(_) -> lazy(expr(path))
      Error(_) -> return(Builtin1(pos, "dyn"))
    })
    return(Arg1(ArgMode(imp), arg, argt))
  }))
  use _ <- do(char(")"))
  use e <- do(lazy(expr(path)))
  return(Func1(pos, args, e))
}

fn paren(p: Parser(a, e)) -> Parser(a, e) {
  use _ <- do(char("("))
  use x <- do(p)
  use _ <- do(char(")"))
  return(x)
}

fn paren_expr(path) {
  paren(lazy(expr(path)))
}

fn sep(parser: Parser(a, e), by s: Parser(b, e)) -> Parser(List(a), e) {
  use mb_a <- do(perhaps(parser))
  case mb_a {
    Ok(a) -> {
      use res <- do(perhaps(s))
      case res {
        Ok(_) -> {
          use rest <- do(sep(parser, by: s))
          return([a, ..rest])
        }
        Error(Nil) -> return([a])
      }
    }
    Error(Nil) -> return([])
  }
}

fn comma_sep(parser: Parser(a, e)) -> Parser(List(a), e) {
  sep(parser, by: char(","))
}

fn tforall(path: String) -> Parser(Expr1, Nil) {
  use pos <- do(pos())
  use _ <- do(party.string("Fn"))
  use _ <- do(ws())
  use _ <- do(char("("))
  use args <- do(comma_sep({
    use _ <- do(ws())
    use res <- do(perhaps(char("@")))
    let imp = result.is_ok(res)
    use _ <- do(ws())
    use arg <- do(identstring())
    use _ <- do(ws())
    use _ <- do(char(":"))
    use t <- do(lazy(expr(path)))
    return(Arg1(ArgMode(imp), arg, t))
  }))
  use _ <- do(char(")"))
  use body <- do(lazy(expr(path)))
  return(TPi1(pos, args, body))
}

fn tinter(path: String) -> Parser(Expr1, Nil) {
  use pos <- do(pos())
  use _ <- do(party.string("inter"))
  use _ <- do(not(alt(alphanum(), char("_"))))
  use _ <- do(ws())
  use _ <- do(char("{"))
  use fields <- do(comma_sep({
    use _ <- do(ws())
    use name <- do(identstring())
    use _ <- do(ws())
    use _ <- do(char(":"))
    use t <- do(lazy(expr(path)))
    return(#(name, t))
  }))
  use _ <- do(char("}"))
  return(TInter1(pos, fields))
}

fn inter(path: String) -> Parser(Expr1, Nil) {
  use pos <- do(pos())
  use _ <- do(party.char("["))
  use fields <- do(comma_sep({
    use _ <- do(ws())
    use name <- do(identstring())
    use _ <- do(ws())
    use _ <- do(char(":"))
    use val <- do(lazy(expr(path)))
    use _ <- do(char(":"))
    use t <- do(lazy(expr(path)))
    return(#(name, val, t))
  }))
  case list.is_empty(fields) {
    // I was too lazy to make a comma_sep1
    True -> {
      use _ <- do(not(char("]")))
      panic("")
    }
    False -> {
      use _ <- do(char("]"))
      return(Inter1(pos, fields))
    }
  }
}

fn ws() -> Parser(Nil, a) {
  let comment = fn() {
    use _ <- do(party.string("//"))
    use _ <- do(many(satisfy(fn(c) { c != "\n" })))
    return("")
  }
  use _ <- do(many(choice([char(" "), char("\t"), char("\n"), comment()])))
  return(Nil)
}

fn postfix(e: Expr1, path: String) -> Parser(Expr1, Nil) {
  use pos <- do(pos())
  use mb_call <- do(perhaps(paren(comma_sep(lazy(expr(path))))))
  case mb_call {
    Ok(args) -> {
      let e = App1(pos, e, args)
      use _ <- do(ws())
      postfix(e, path)
    }
    Error(Nil) -> {
      use mb_arr <- do(perhaps(party.string("->")))
      case mb_arr {
        Ok(_) -> {
          use rhs <- do(lazy(expr(path)))
          let e = TPi1(pos, [Arg1(ArgMode(False), "_", e)], rhs)
          use _ <- do(ws())
          postfix(e, path)
        }
        Error(Nil) -> {
          use mb_dot <- do(perhaps(char(".")))
          use _ <- do(ws())
          case mb_dot {
            Ok(_) -> {
              use _ <- do(ws())
              use idx <- do(intstring())
              let e = Projection1(pos, e, idx)
              use _ <- do(ws())
              postfix(e, path)
            }
            Error(Nil) -> return(e)
          }
        }
      }
    }
  }
}

fn expr(path: String) -> fn() -> Parser(Expr1, Nil) {
  fn() {
    use _ <- do(ws())
    use lit <- do(choice([
      tinter(path),
      tforall(path),
      inter(path),
      intlit(),
      paren_expr(path),
      lamlit(path),
      builtin(),
      identlit(path),
    ]))
    use _ <- do(ws())
    use res <- do(postfix(lit, path))
    use _ <- do(ws())
    return(res)
  }
}

fn fn_def(path: String) -> Parser(#(Stmt1, String, Expr1), Nil) {
  use _ <- do(ws())
  use pos <- do(pos())
  use _ <- do(party.string("fn"))
  use _ <- do(not(alt(alphanum(), char("_"))))
  use _ <- do(ws())
  use name <- do(identstring())
  use _ <- do(ws())
  use _ <- do(char("("))
  use args <- do(comma_sep({
    use _ <- do(ws())
    use res <- do(perhaps(char("@")))
    let imp = result.is_ok(res)
    use arg <- do(identstring())
    use _ <- do(ws())
    use _ <- do(char(":"))
    use t <- do(lazy(expr(path)))
    return(Arg1(ArgMode(imp), arg, t))
  }))
  use _ <- do(char(")"))
  use _ <- do(ws())
  use mb_ret_t <- do(perhaps(party.string("->")))
  use ret_t <- do(case mb_ret_t {
    Ok(_) -> expr(path)()
    Error(Nil) -> return(Builtin1(pos, "dyn"))
  })
  let t = TPi1(pos, args, ret_t)
  use _ <- do(ws())
  use _ <- do(char("{"))
  use body <- do(expr(path)())
  use _ <- do(char("}"))
  use _ <- do(lazy(ws))
  return(#(Def1(pos, name, t, Func1(pos, args, body)), name, t))
}

fn stmt(path: String) -> Parser(#(Stmt1, String, Expr1), Nil) {
  choice([fn_def(path)])
}

fn parse(
  path: String,
  src: String,
  state: State,
) -> Monad(#(List(Stmt1), Map(String, Expr1))) {
  let res =
    party.go(
      {
        use res <- do(many1(stmt(path)))
        use _ <- do(end())
        let #(stmts, symbol_table) =
          list.fold(
            res,
            #([], map.new()),
            fn(curr, r) { #([r.0, ..curr.0], map.insert(curr.1, r.1, r.2)) },
          )
        return(#(stmts, symbol_table))
      },
      src,
    )
    |> result.map_error(ParseError(path, _))
  core.try(res, state, core.return)
}
