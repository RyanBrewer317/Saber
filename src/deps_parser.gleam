import party.{
  Parser, alt, char, choice, digit, do, lazy, letter, many, many1, map, not,
  perhaps, return, satisfy, seq, string as str,
}
import gleam/string.{concat}
import gleam/int
import gleam/result
import gleam/list
import gleam/map.{Map}

fn comment() -> Parser(Nil, e) {
  use _ <- do(char("#"))
  use _ <- do(many(satisfy(fn(c) { c != "\n" })))
  return(Nil)
}

fn utf(code: Int) -> String {
  let assert Ok(codepoint) = string.utf_codepoint(code)
  string.from_utf_codepoints([codepoint])
}

fn escape() -> Parser(String, e) {
  use _ <- do(char("\\"))
  use c <- do(choice([
    char("n"),
    char("t"),
    char("b"),
    char("r"),
    char("f"),
    char("\""),
    char("\\"),
  ]))
  case c {
    "n" -> return("\n")
    "t" -> return("\t")
    "b" -> return(utf(0x0008))
    "r" -> return("\r")
    "f" -> return("\f")
    "\"" -> return("\"")
    "\\" -> return("\\")
    "u" | "U" -> {
      let hex_str =
        choice([
          char("_")
          |> map(fn(_) { "" }),
          digit(),
          satisfy(fn(c) {
            list.contains(string.to_graphemes("aAbBcCdDeEfF"), c)
          }),
        ])
      let hex =
        hex_str
        |> map(fn(c) {
          c
          |> int.base_parse(16)
          |> result.lazy_unwrap(fn() { panic("") })
        })
      use d1 <- do(hex)
      use d2 <- do(hex)
      use d3 <- do(hex)
      use d4 <- do(hex)
      case c {
        "U" -> {
          use d5 <- do(hex)
          use d6 <- do(hex)
          use d7 <- do(hex)
          use d8 <- do(hex)
          return(utf(
            d1 * 268_435_456 + d2 * 16_777_216 + d3 * 1_048_576 + d4 * 65_536 + d5 * 4096 + d6 * 256 + d7 * 16 + d8,
          ))
        }
        _ -> {
          return(utf(d1 * 4096 + d2 * 256 + d3 * 16 + d4))
        }
      }
    }
  }
}

fn string() -> Parser(String, e) {
  use _ <- do(char("\""))
  use body <- do(
    many(alt(escape(), satisfy(fn(c) { c != "\"" })))
    |> map(concat),
  )
  use _ <- do(char("\""))
  return(body)
}

fn literal_string() -> Parser(String, e) {
  use _ <- do(char("'"))
  use body <- do(
    many(satisfy(fn(c) { c != "'" }))
    |> map(concat),
  )
  use _ <- do(char("'"))
  return(body)
}

fn ws() -> Parser(Nil, e) {
  use _ <- do(many(choice([
    char(" "),
    char("\t"),
    char("\n"),
    char("\r"),
    comment()
    |> map(fn(_) { "" }),
  ])))
  return(Nil)
}

pub type Expr {
  StringExpr(String)
  TableExpr(Map(String, Expr))
}

fn expr() -> Parser(Expr, e) {
  use _ <- do(ws())
  use e <- do(choice([
    alt(string(), literal_string())
    |> map(StringExpr),
  ]))
  use _ <- do(ws())
  return(e)
}

fn key() -> Parser(String, e) {
  choice([
    many1(choice([letter(), digit(), char("_")]))
    |> map(concat),
    string(),
    literal_string(),
  ])
}

fn key_val_pair() -> Parser(#(String, Expr), e) {
  use _ <- do(ws())
  use k <- do(key())
  use _ <- do(ws())
  use _ <- do(char("="))
  use e <- do(expr())
  return(#(k, e))
}

pub fn parse() -> Parser(
  #(Map(String, Expr), Map(String, Map(String, Expr))),
  e,
) {
  use top_rows <- do(
    many(key_val_pair())
    |> map(map.from_list),
  )
  use rest <- do(many({
    use _ <- do(ws())
    use _ <- do(char("["))
    use _ <- do(ws())
    use name <- do(key())
    use _ <- do(ws())
    use _ <- do(char("]"))
    use _ <- do(ws())
    use rows <- do(many(key_val_pair()))
    return(#(name, map.from_list(rows)))
  }))
  return(#(top_rows, map.from_list(rest)))
}
