import gleam/io
import gleam/result
import gleam/list
// import gleam/string
import parser
import core.{
  type Library0, type Module0, type Monad, type State, CouldntOpenFile, Library0,
  Module0, do, monadic_fold, pretty_err, return, try,
}
import ast
import type_check
import eval
import shellout.{arguments}
import simplifile

fn construct_library(path: String, state: State) -> Monad(Library0) {
  use entry, state <- do(construct_module(path <> "/src", state))
  return(Library0(path, entry), state)
}

fn construct_module(path: String, state: State) -> Monad(Module0) {
  let assert True = simplifile.is_directory(path)
  use file_names, state <- try(
    simplifile.get_files(path)
    |> result.replace_error(CouldntOpenFile(path)),
    state,
  )
  use #(subs, files), state <- do({
    use #(subs, files), file_name, state <-
      monadic_fold(
        from: #([], []),
        over: file_names,
        using: state,
        with: _,
        then: return,
      )
    case simplifile.is_directory(file_name) {
      True -> {
        use module, state <- do(construct_module(file_name, state))
        return(#([module, ..subs], files), state)
      }
      False -> return(#(subs, [file_name, ..files]), state)
    }
  })
  return(Module0(path, subs, files), state)
}

// deals with an inconsistency in shellout across targets
@target(javascript)
fn argpos() {
  1
}

@target(erlang)
fn argpos() {
  0
}

pub fn main() {
  let m = {
    use state <- core.start()
    use path, state <- try(
      list.at(arguments(), argpos())
      |> result.replace_error(CouldntOpenFile("no path provided")),
      state,
    )
    use lib0, state <- do(construct_library(path, state))
    use lib1, state <- do(parser.parse_lib(lib0, state))
    use lib2, state <- do(ast.build_lib(lib1, state))
    use lib3, state <- do(type_check.annotate_lib(lib2, state))
    // io.println(string.join(list.map(lib3.entry.ast, core.pretty_stmt3), "\n\n"))
    // io.println("Type checked!")
    eval.eval_lib(lib3, state)
  }
  let res = core.eval(m)
  case res {
    Ok(Nil) -> "ok"
    Error(e) -> pretty_err(e)
  }
  |> io.println()
}
