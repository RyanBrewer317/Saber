import gleam/erlang/file
import gleam/io
import gleam/result
import gleam/map
import gleam/list
import parser
import core.{pretty_expr3}
import monad.{CouldntOpenFile, do, try}
import ast
import type_check
import eval

pub fn main() {
  let m = {
    use code <- try(
      file.read("main.sb")
      |> result.map_error(CouldntOpenFile),
    )
    io.println(code)
    use prog <- do(parser.parse(code))
    // todo: use/store renames for pretty printing purposes?
    use #(ast, _) <- do(monad.reduce(prog, #([], map.new()), ast.iteratee))
    use #(typed_ast, _) <- do(monad.reduce(list.reverse(ast), #([], map.new()), type_check.iteratee))
    eval.go(list.reverse(typed_ast))
  }
  case monad.run(m) {
    Ok(e) -> ">> " <> pretty_expr3(e)
    Error(e) -> monad.pretty_err(e)
  }
  |> io.println()
}
