import gleam/io
import gleam/result
import gleam/list
import parser
import core.{CouldntOpenFile, Library0, Module0, pretty_err}
import monad.{Monad, State, do, monadic_fold, return, try}
import ast
import type_check
import elab
import eval
import shellout.{arguments}
import simplifile

fn construct_library(path: String, state: State) -> Monad(Library0) {
  use entry, state2 <- do(construct_module(path <> "/src", state))
  return(Library0(path, entry), state2)
}

fn construct_module(path: String, state: State) -> Monad(Module0) {
  let assert True = simplifile.is_directory(path)
  use file_names, state2 <- try(
    simplifile.list_contents(path)
    |> result.replace_error(CouldntOpenFile(path)),
    state,
  )
  use #(subs, files), state3 <- do({
    use #(subs, files), file_name, statex <-
      monadic_fold(
        from: #([], []),
        over: file_names,
        using: state2,
        with: _,
        then: return,
      )
    case simplifile.is_directory(path <> "/" <> file_name) {
      True -> {
        use module, statex2 <- do(construct_module(
          path <> "/" <> file_name,
          statex,
        ))
        return(#([module, ..subs], files), statex2)
      }
      False -> return(#(subs, [file_name, ..files]), statex)
    }
  })
  return(Module0(path, subs, files), state3)
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
    use state <- monad.start()
    use path, state2 <- try(
      list.at(arguments(), argpos())
      |> result.replace_error(CouldntOpenFile("no path provided")),
      state,
    )
    use lib0, state3 <- do(construct_library(path, state2))
    use lib1, state4 <- do(parser.parse_lib(lib0, state3))
    use lib2, state5 <- do(ast.build_lib(lib1, state4))
    use lib3, state6 <- do(type_check.annotate_lib(lib2, state5))
    use lib4, state7 <- do(elab.elaborate_lib(lib3, state6))
    eval.eval_lib(lib4, state7)
  }
  let res = monad.eval(m)
  case res {
    Ok(Nil) -> "ok"
    Error(e) -> pretty_err(e)
  }
  |> io.println()
}
