open Ast

(******************************************************************************)
(*                                simplify expressions                        *)
(******************************************************************************)

let rec simplify_expr = function
| True -> True
| False -> False
| Var x -> Var x
| Map(e1,e2) -> Map(simplify_expr e1,simplify_expr e2)
| IntConst n -> IntConst n
| AddrConst n -> AddrConst n
| StringConst s -> StringConst s
| Not e -> (match simplify_expr e with
  | True -> False
  | False -> True
  | e' -> Not e')
| And(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | True,e2' -> e2'
  | False,_ -> False
  | e1',True -> e1'
  | _,False -> False
  | e1',e2' -> And(e1',e2'))
| Or(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | True,_ -> True
  | False,e2' -> e2'
  | _,True -> True
  | e1',False -> e1'
  | e1',e2' -> Or(e1',e2'))
| Add(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | IntConst n1,IntConst n2 -> IntConst (n1+n2)
  | e1',e2' -> Add(e1',e2'))
| Sub(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | (e1',e2') when e1'=e2' -> IntConst 0
  | (e1',Sub(e1'',e2')) when e1'=e1'' -> e2'
  | e1',e2' -> Sub(e1',e2'))
| Mul(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | IntConst n1,IntConst n2 -> IntConst (n1*n2)
  | e1',e2' -> Mul(e1',e2'))
| Div(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | IntConst n1,IntConst n2 -> IntConst (n1/n2)
  | e1',e2' -> Div(e1',e2'))
| Eq(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | IntConst n1,IntConst n2 -> if n1=n2 then True else False
  | e1',e2' -> Eq(e1',e2'))
| Neq(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | IntConst n1,IntConst n2 -> if n1<>n2 then True else False
  | e1',e2' -> Neq(e1',e2'))
| Leq(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | IntConst n1,IntConst n2 -> if n1<=n2 then True else False
  | e1',e2' -> Leq(e1',e2'))
| Le(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | IntConst n1,IntConst n2 -> if n1<n2 then True else False
  | e1',e2' -> Le(e1',e2'))
| Geq(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | IntConst n1,IntConst n2 -> if n1>=n2 then True else False
  | e1',e2' -> Geq(e1',e2'))
| Ge(e1,e2) -> (match simplify_expr e1, simplify_expr e2 with
  | IntConst n1,IntConst n2 -> if n1>n2 then True else False
  | e1',e2' -> Ge(e1',e2'))
| Bal(t) -> Bal(t)
| BalPre(t) -> BalPre(t)
| IfE(e1,e2,e3) -> (match simplify_expr e1 with
  | True -> simplify_expr e2
  | False -> simplify_expr e3
  | e1' -> IfE(e1',simplify_expr e2,simplify_expr e3))
| MapUpd(e1,e2,e3) -> MapUpd(simplify_expr e1,simplify_expr e2,simplify_expr e3)