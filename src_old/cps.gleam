// import fresh
// import grammar as g
// import concurrent as c
// import gleam/iterator
// import gleam/list
// import type_infer.{TypeServer, get_type}

// pub fn go(
//   id_server: fresh.IdServer(a),
//   from input: c.Stream(g.Stmt),
//   to output: c.Stream(g.CPSStmt),
// ) -> Nil {
//   use <- c.with(output)
//   use stmt <- iterator.each(c.to_iterator(input))
//   c.write(output, convert_stmt(id_server, stmt))
// }

// fn convert_stmt(id_server: fresh.IdServer(a), types: TypeServer(b), stmt: g.Stmt) -> g.CPSStmt {
//     case stmt {
//         g.Def(id, expr) -> {
//             let assert Ok(t) = get_type(types, id)
//             g.CPSDef(id, convert_expr(id_server, expr, k: g.CPSHalt(convert_type(t), _)))
//         }
//     }
// }

// fn convert_expr(id_server: fresh.IdServer(a), types: TypeServer(b), expr: g.Expr, k k: fn(g.CPSVal)->g.CPSExpr) -> g.CPSExpr {
//     case expr {
//         g.Ident(_id, ref) -> {
//             let assert Ok(t) = get_type(types, ref)
//             k(g.CPSLocal(ref, t))
//         }
//         g.Int(id) -> {
//             let assert Ok(t) = get_type(types, id)
//             k(g.CPSInt(id, t))
//         }
//         g.Lambda(id, argid, mb_argt, body) -> {

//         }
//     }
// }

// fn convert_type(t: g.Type) -> g.Type {
//     let cont = fn(u) {
//         g.TFunc(u, g.TConstr("Void", []))
//     }
//     case t {
//         g.TVar(_) -> t
//         g.TConstr(s, ts) -> g.TConstr(s, list.map(ts, convert_type))
//         g.TFunc(a, b) -> cont(g.TConstr("Tuple", [convert_type(a), cont(b)]))
//         g.TForall(id, body) -> cont(g.TForall(id, cont(convert_type(body))))
//         g.TDynamic -> g.TDynamic
//     }
// }
