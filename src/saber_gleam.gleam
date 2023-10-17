import gleam/io
import gleam/result
import gleam/list
import parser
import core.{CouldntOpenFile, Library0, Module0, pretty_err}
import monad.{Monad, do, return, try}
import ast
import type_check
import elab
import eval
import shellout.{arguments}
import simplifile

fn construct_library(path: String) -> Monad(Library0) {
  use entry <- do(construct_module(path <> "/src"))
  return(Library0(path, entry))
}

fn construct_module(path: String) -> Monad(Module0) {
  let assert True = simplifile.is_directory(path)
  use file_names <- try(
    simplifile.list_contents(path)
    |> result.replace_error(CouldntOpenFile(path)),
  )
  use #(subs, files) <- do({
    use file_name, #(subs, files) <- monad.reduce(
      from: #([], []),
      over: file_names,
    )
    case simplifile.is_directory(path <> "/" <> file_name) {
      True -> {
        use module <- do(construct_module(path <> "/" <> file_name))
        return(#([module, ..subs], files))
      }
      False -> return(#(subs, [file_name, ..files]))
    }
  })
  return(Module0(path, subs, files))
}

pub fn main() {
  let m = {
    use path <- try(
      list.at(arguments(), 1)
      |> result.replace_error(CouldntOpenFile("no path provided")),
    )
    use lib0 <- do(construct_library(path))
    use lib1 <- do(parser.parse_lib(lib0))
    use lib2 <- do(ast.build_lib(lib1))
    use lib3 <- do(type_check.annotate_lib(lib2))
    use lib4 <- do(elab.elaborate_lib(lib3))
    use _ <- do(eval.eval_lib(lib4))
    return(Nil)
  }
  // use rslt <- do(eval.eval_lib(lib4))
  // return(rslt)
  case monad.run(m) {
    Ok(Nil) -> "ok"
    Error(e) -> pretty_err(e)
  }
  |> io.println()
}
