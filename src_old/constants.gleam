import gleam/otp/actor
import gleam/erlang/process
import grammar as g
import gleam/map

pub opaque type ConstantMessage {
  IntLit(g.Id(g.ExprPh), Int)
  GetInt(process.Subject(Int), g.Id(g.ExprPh))
  ShutDown
}

pub type ConstantServer =
  process.Subject(ConstantMessage)

pub fn go() -> g.MaybeActor(ConstantServer) {
  actor.start(
    map.new(),
    fn(msg, constants) {
      case msg {
        IntLit(id, val) -> actor.Continue(map.insert(constants, id, val))
        GetInt(client, id) -> {
          let assert Ok(val) = map.get(constants, id)
          process.send(client, val)
          actor.Continue(constants)
        }
        ShutDown -> actor.Stop(process.Normal)
      }
    },
  )
}

pub fn end(constant_server: ConstantServer) -> Nil {
  process.send(constant_server, ShutDown)
}

pub fn int(server: ConstantServer, id: g.Id(g.ExprPh), val: Int) -> Nil {
  actor.send(server, IntLit(id, val))
}
