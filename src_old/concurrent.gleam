import grammar
import gleam/erlang/process
import gleam/iterator

pub type Stream(a) {
  Stream(process.Subject(grammar.PipeMessage(a)))
}

pub fn new() -> Stream(a) {
  Stream(process.new_subject())
}

pub fn next(s: Stream(a)) -> Result(a, Nil) {
  let Stream(subject) = s
  // io.println("Waiting for message on subject...")
  case process.receive(subject, 15 * 60 * 1000) {
    Ok(grammar.Another(a)) -> {
      // io.println("Received message on subject!")
      Ok(a)
    }
    Ok(grammar.Done) -> {
      // io.println("Received Done message on subject!")
      Error(Nil)
    }
    Error(Nil) -> {
      // io.println("Subject timed out waiting for message")
      Error(Nil)
    }
  }
}

pub fn write(s: Stream(a), value: a) -> Stream(a) {
  let Stream(subject) = s
  process.send(subject, grammar.Another(value))
  s
}

pub fn finish(s: Stream(a)) -> Nil {
  let Stream(subject) = s
  process.send(subject, grammar.Done)
}

pub fn to_iterator(s: Stream(a)) -> iterator.Iterator(a) {
  iterator.unfold(
    Nil,
    fn(_) {
      case next(s) {
        Ok(a) -> {
          // io.debug(a)
          iterator.Next(a, Nil)
        }
        Error(Nil) -> {
          // io.println("done")
          iterator.Done
        }
      }
    },
  )
}

pub fn collect(s: Stream(a)) -> List(a) {
  to_iterator(s)
  |> iterator.to_list()
}

pub fn defer(f: fn()->Nil, g: fn()->Nil) -> Nil {
  g()
  f()
}

pub fn with(stream: Stream(a), f: fn()->b) -> b {
  let out = f()
  finish(stream)
  out
}

pub fn foreach_unless_error(stream: Stream(a), f: fn(a) -> Result(Nil, c)) -> Result(Nil, c) {
  case next(stream) {
    Ok(a) -> {
      case f(a) {
        Ok(Nil) -> foreach_unless_error(stream, f)
        Error(err) -> Error(err)
      }
    }
    Error(Nil) -> Ok(Nil)
  }
}