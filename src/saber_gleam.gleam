import gleam/io
import gleam/result
import gleam/map
import gleam/list
import parser
import core.{CouldntOpenFile, pretty_err, pretty_expr}
import monad.{do, try}
import ast
import type_check
import elab
import eval

@external(javascript, "./comp_helpers.mjs", "readFile")
fn read_file(path: String) -> Result(String, String)

pub fn main() {
  let m = {
    use code <- try(
      read_file("main.sb")
      |> result.map_error(CouldntOpenFile),
    )
    io.println(code)
    use prog <- do(parser.parse(code))
    // todo: use/store renames for pretty printing purposes?
    use #(ast, _) <- do(monad.reduce(prog, #([], []), ast.iteratee))
    // io.debug(list.reverse(ast))
    use #(typed_ast, _) <- do(monad.reduce(
      list.reverse(ast),
      #([], map.new()),
      type_check.iteratee,
    ))
    use #(elaborated_ast) <- do(monad.reduce(
      list.reverse(typed_ast),
      #([]),
      elab.iteratee,
    ))
    eval.go(list.reverse(elaborated_ast))
  }
  case monad.run(m) {
    Ok(e) -> ">> " <> pretty_expr(e)
    Error(e) -> pretty_err(e)
  }
  |> io.println()
}
