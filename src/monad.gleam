import core.{Id, Type3, pretty_type3}
import gleam/erlang/file.{Reason}
import party.{ParseError, Position}
import gleam/string

pub type Error {
  CouldntOpenFile(Reason)
  ParseError(ParseError(Nil))
  Undefined(Position, String)
  TypeError(Type3, Type3)
  CallingNonFunction(Type3)
  CallingNonForall(Type3)
  TypeAtRuntime(Type3)
}

pub fn pretty_err(e: Error) -> String {
  case e {
    CouldntOpenFile(r) -> "Error! Couldn't open the Saber file! Reason: " <> string.inspect(r)
    ParseError(p) -> "Parse error! " <> string.inspect(p)
    Undefined(pos, s) -> "Error! Undefined variable " <> s <> " at position " <> string.inspect(pos)
    TypeError(t1, t2) -> "Type error! Couldn't unify " <> pretty_type3(t1) <> " and " <> pretty_type3(t2)
    CallingNonFunction(t) -> "Type error! Calling non-function of type " <> pretty_type3(t) <> " as if it were a function"
    CallingNonForall(t) -> "Type error! Calling non-forall of type " <> pretty_type3(t) <> " as if it were a type-abstraction"
    TypeAtRuntime(t) -> "Runtime error! Found a type at runtime, " <> pretty_type3(t)
  }
}

pub opaque type Monad(a) {
  Monad(fn(Id) -> Result(#(a, Id), Error))
}

pub fn fresh(f: fn(Id) -> Monad(a)) -> Monad(a) {
  do(Monad(fn(curr_id) { Ok(#(curr_id, curr_id + 1)) }), f)
}

pub fn return(x: a) -> Monad(a) {
  Monad(fn(curr_id) { Ok(#(x, curr_id)) })
}

pub fn do(ma: Monad(a), f: fn(a) -> Monad(b)) -> Monad(b) {
  Monad(fn(curr_id) {
    let Monad(g) = ma
    case g(curr_id) {
      Ok(#(val, curr_id)) -> {
        let Monad(h) = f(val)
        h(curr_id)
      }
      Error(e) -> Error(e)
    }
  })
}

pub fn lift(x: Result(a, Error)) -> Monad(a) {
  case x {
    Ok(val) -> Monad(fn(curr_id) { Ok(#(val, curr_id)) })
    Error(e) -> Monad(fn(_) { Error(e) })
  }
}

pub fn try(x: Result(a, Error), f: fn(a)->Monad(b)) -> Monad(b) {
  do(lift(x), f)
}

pub fn fail(e: Error) -> Monad(a) {
  Monad(fn(_) { Error(e) })
}

pub fn run(ma: Monad(a)) -> Result(a, Error) {
  let Monad(f) = ma
  case f(0) {
    Ok(#(val, _)) -> Ok(val)
    Error(e) -> Error(e)
  }
}

pub fn map(l: List(a), f: fn(a) -> Monad(b)) -> Monad(List(b)) {
  case l {
    [] -> return([])
    [x, ..xs] -> {
      use x2 <- do(f(x))
      use xs2 <- do(map(xs, f))
      return([x2, ..xs2])
    }
  }
}

pub fn reduce(l: List(a), base: b, f: fn(a, b)->Monad(b)) -> Monad(b) {
  case l {
    [] -> return(base)
    [x, ..xs] -> {
      use b2 <- do(f(x, base))
      reduce(xs, b2, f)
    }
  }
}