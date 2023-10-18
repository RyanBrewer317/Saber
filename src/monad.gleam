import core.{Error, Id}
import gleam/io
import gleam/string_builder.{StringBuilder}

pub opaque type State {
  State(id: Id, log: StringBuilder)
}

pub opaque type Monad(a) {
  Cont(State, a)
  Fail(Error)
}

pub fn do(ma: Monad(a), f: fn(a, State) -> Monad(b)) -> Monad(b) {
  case ma {
    Cont(state, val) -> f(val, state)
    Fail(e) -> Fail(e)
  }
}

pub fn fresh(state: State, k: fn(Id, State) -> Monad(b)) -> Monad(b) {
  k(state.id, State(state.id + 1, state.log))
}

pub fn return(x: a, state: State) -> Monad(a) {
  Cont(state, x)
}

pub fn try(
  x: Result(a, Error),
  state: State,
  k: fn(a, State) -> Monad(b),
) -> Monad(b) {
  case x {
    Ok(val) -> k(val, state)
    Error(e) -> Fail(e)
  }
}

pub fn fail(e: Error) -> Monad(a) {
  Fail(e)
}

pub fn eval(ma: Monad(a)) -> Result(a, Error) {
  case ma {
    Cont(state, val) -> {
      io.println(string_builder.to_string(state.log))
      Ok(val)
    }
    Fail(e) -> Error(e)
  }
}

pub fn start(k: fn(State) -> Monad(a)) -> Monad(a) {
  k(State(0, string_builder.new()))
}

pub fn monadic_map(
  l: List(a),
  state: State,
  f: fn(a, State) -> Monad(b),
  k: fn(List(b), State) -> Monad(c),
) -> Monad(c) {
  case l {
    [] -> k([], state)
    [x, ..xs] -> {
      case f(x, state) {
        Cont(state2, x2) -> {
          use xs2, state3 <- monadic_map(xs, state2, f)
          k([x2, ..xs2], state3)
        }
        Fail(e) -> Fail(e)
      }
    }
  }
}

pub fn fmap(
  ma: Monad(a),
  f: fn(a) -> b,
  k: fn(b, State) -> Monad(c),
) -> Monad(c) {
  case ma {
    Cont(state, x) -> k(f(x), state)
    Fail(e) -> Fail(e)
  }
}

pub fn monadic_fold(
  over l: List(a),
  from base: b,
  using state: State,
  with f: fn(b, a, State) -> Monad(b),
  then k: fn(b, State) -> Monad(c),
) -> Monad(c) {
  case l {
    [] -> k(base, state)
    [x, ..xs] -> {
      case f(base, x, state) {
        Cont(state2, b2) -> {
          monadic_fold(xs, b2, state2, f, k)
        }
        Fail(e) -> Fail(e)
      }
    }
  }
}

pub fn when(
  cond: Bool,
  ma: Monad(Nil),
  state: State,
  k: fn(State) -> Monad(a),
) -> Monad(a) {
  case cond {
    True ->
      case ma {
        Cont(state2, Nil) -> k(state2)
        Fail(e) -> Fail(e)
      }
    False -> k(state)
  }
}

pub fn unwrap(ma: Monad(a)) -> a {
  case ma {
    Cont(_, x) -> x
    Fail(_) -> panic("")
  }
}

pub type DebugStatus {
  Y
  N
}

pub fn log(
  msg: String,
  state: State,
  dbg: DebugStatus,
  k: fn(State) -> Monad(a),
) -> Monad(a) {
  // io.println(msg)
  k(State(
    state.id,
    case dbg {
      Y -> string_builder.append(state.log, msg <> "\n")
      N -> state.log
    },
  ))
}
