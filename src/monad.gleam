import core.{Id, Error}

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