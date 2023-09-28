import gleam/otp/actor
import gleam/erlang/process
import grammar

pub opaque type IdMessage(a) {
  NewId(process.Subject(grammar.Id(a)))
  NoMoreIds
}

pub type IdServer(a) =
  process.Subject(IdMessage(a))

pub fn go() {
  actor.start(0, handle_message)
}

fn handle_message(
  m: IdMessage(a),
  prev_id: grammar.Id(b),
) -> actor.Next(grammar.Id(c)) {
  case m, prev_id {
    NoMoreIds, _ -> actor.Stop(process.Normal)
    NewId(client), id -> {
      process.send(client, id)
      actor.Continue(id + 1)
    }
  }
}

pub fn get(id_server: process.Subject(IdMessage(a))) -> grammar.Id(a) {
  actor.call(id_server, NewId, 50)
}

pub fn end(id_server: process.Subject(IdMessage(a))) -> Nil {
  process.send(id_server, NoMoreIds)
}
