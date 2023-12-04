import core.{
  type Expr3, type Ident, type Library3, type Module3, type Monad, type PureExpr,
  type State, type Stmt3, App, App3, Builtin, Builtin3, Def3, Func, Func3,
  Global, Ident, Ident3, Int, Int3, Inter3, Local, Projection3, do, monadic_fold,
  monadic_map, return,
}
import gleam/list
import gleam/map.{type Map, get, insert}

type Library =
  Library3

type Module =
  Module3

type Stmt =
  Stmt3

type Expr =
  Expr3

pub fn eval_lib(lib: Library, state: State) -> Monad(Nil) {
  use defs, state <- do(eval_mod(lib.entry, state))
  case get(defs, Global("main")) {
    Ok(Func([], body)) -> {
      use _, state <- do(normalize(body, state, defs))
      return(Nil, state)
    }
    Ok(_) -> panic("")
    Error(Nil) -> return(Nil, state)
  }
}

fn eval_mod(mod: Module, state: State) -> Monad(Map(Ident, PureExpr)) {
  let assert [s, ..ss] = mod.ast
  use start_heap, state <- do(stmt(s, map.new(), state))
  use heap, state <- monadic_fold(
    ss,
    start_heap,
    state,
    fn(heap, s, state) {
      use heap2, state <- do(stmt(s, heap, state))
      return(map.merge(heap2, heap), state)
    },
  )
  return(heap, state)
}

fn stmt(
  s: Stmt,
  heap: Map(Ident, PureExpr),
  state: State,
) -> Monad(Map(Ident, PureExpr)) {
  case s {
    Def3(_, name, _, val) -> {
      use val2, state <- do(expr(
        val,
        insert(heap, Global(name), erase(val, heap)),
        state,
      ))
      return(insert(map.new(), Global(name), val2), state)
    }
  }
}

fn erase(e: Expr, defs: Map(Ident, PureExpr)) -> PureExpr {
  let erase = erase(_, defs)
  case e {
    Int3(_, i) -> Int(i)
    Ident3(_, _, id) ->
      case id {
        Local(id) -> Ident(id)
        Global(_) ->
          case map.get(defs, id) {
            Ok(val) -> val
            Error(Nil) -> panic("undefined global during typechecking")
          }
      }
    Builtin3(_, _, name) -> Builtin(name)
    Projection3(_, _, e, _) -> erase(e)
    Func3(_, _, args, body) ->
      Func(
        list.map(list.filter(args, fn(a) { !a.mode.implicit }), fn(a) { a.id }),
        erase(body),
      )
    App3(_, _, func, args) -> {
      let args = list.filter(args, fn(a) { !{ a.0 }.implicit })
      App(erase(func), list.map(args, fn(a) { erase(a.1) }))
    }
    Inter3(_, _, args) -> {
      let assert Ok(first) = list.first(args)
      erase(first.1)
    }
    _ -> panic("")
  }
}

fn normalize(
  e: PureExpr,
  state: State,
  heap: Map(Ident, PureExpr),
) -> Monad(PureExpr) {
  case e {
    Ident(id) ->
      case map.get(heap, Local(id)) {
        Ok(val) -> return(val, state)
        Error(Nil) ->
          panic(
            "undefined variable in pure expr, which should be during typechecking",
          )
      }
    App(func, args) -> {
      use args, state <- monadic_map(
        args,
        state,
        fn(a, state) { normalize(a, state, heap) },
      )
      use func, state <- do(normalize(func, state, heap))
      case func {
        Func(formals, body) -> {
          let assert False = list.length(formals) != list.length(args)
          let heap =
            map.merge(
              map.from_list(list.zip(list.map(formals, Local), args)),
              heap,
            )
          normalize(body, state, heap)
        }
        Builtin("print") -> {
          let assert [Int(i)] = args
          core.log(i)
          return(Int(i), state)
        }
        _ -> {
          core.log(heap)
          core.log(func)
          case Nil {
            Nil -> panic("")
          }
        }
      }
    }
    Int(_) | Builtin(_) | Func(_, _) -> return(e, state)
  }
}

fn expr(e: Expr, defs: Map(Ident, PureExpr), state: State) -> Monad(PureExpr) {
  normalize(erase(e, defs), state, map.new())
}
