import parser
import fresh
import build_ast
import constants
import type_infer
import gleam/io
import gleam/list
import grammar as g
import concurrent as c
import gleam/erlang
import anf

pub fn main() {
  let assert Ok(id_server): g.MaybeActor(fresh.IdServer(a)) = fresh.go()
  let assert Ok(constant_server): g.MaybeActor(constants.ConstantServer) =
    constants.go()

  let parse_stream = c.new()
  let ast_stream = c.new()
  let typed_stream = c.new()
  let anf_stream = c.new()

  let assert Ok(code) = erlang.get_line(">> ")
  // "def main(a: Int): (fn(f: Int->Int) f(fn(a: Int) 2))(fn(n: Int) n)"
  // "def id<t>: fn(x: t) x def foo(bar: Int): ((id<Int->Int>)(id<Int>))((id<Int>)(4))"
  // "def main(x: Int): (fn(f: Dyn) f(7))(fn(n: Int) n)"
  // io.println(code)
  let prog = parser.parse(code, to: parse_stream)

  case prog {
    Error(e) -> {
      io.print("Parse error: ")
      io.debug(e)
      Nil
    }
    Ok(_) -> {
      build_ast.go(
        id_server,
        constant_server,
        from: parse_stream,
        to: ast_stream,
      )
      case type_infer.go(id_server, from: ast_stream, to: typed_stream) {
        Ok(type_server) -> {
          anf.go(id_server, from: typed_stream, to: anf_stream)
          let stmts = c.collect(anf_stream)
          io.println("")
          list.map(
            stmts,
            fn(stmt) {
              g.pretty_anf_stmt(stmt)
              |> io.println
            },
          )
          type_infer.end(type_server)
          Nil
        }
        Error(s) -> io.println("Type error: " <> s)
      }
    }
  }
  fresh.end(id_server)
  constants.end(constant_server)
  io.println("")
  main()
}
