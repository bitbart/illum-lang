open Ast

(******************************************************************************)
(*                          NF0: lists for seq and if branches                *)
(******************************************************************************)

let rec cmdNF_of_cmd = function
| Skip -> []
| VarAssign(x,e) -> [VarAssignNF(x,e)]
| MapAssign(x,e1,e2) -> [MapAssignNF(x,e1,e2)]
| Send(x,e,tok) -> [SendNF(x,e,tok)]
| Req(e) -> [ReqNF(e)]
| If(e,c1,c2) -> let c1' = cmdNF_of_cmd c1 in 
  (match cmdNF_of_cmd c2 with
  | [IfNF l2] -> [IfNF ((e,c1')::l2)]
  | c2' -> [IfNF [(e,c1');(True,c2')]])
| Seq(c1,c2) -> (cmdNF_of_cmd c1) @ (cmdNF_of_cmd c2) 

let fun_declNF_of_fun_decl = function
  | Constr(x,vl,c,nl) -> ConstrNF(x,vl,cmdNF_of_cmd c,nl)
  | Proc(x,vl,rl,c,nl) -> ProcNF(x,vl,rl,cmdNF_of_cmd c,nl)

let rec fun_declsNF_of_fun_decls = function
  | EmptyFunDecls -> EmptyFunDeclsNF
  | FunDeclSeq(f,fl) -> FunDeclSeqNF(fun_declNF_of_fun_decl f,fun_declsNF_of_fun_decls fl)

let nf0 = function
  | Contract(x,vl,fdl) -> ContractNF(x,vl,fun_declsNF_of_fun_decls fdl)


(******************************************************************************)
(*                                simplify expressions                        *)
(******************************************************************************)

let rec simplify_expr = function
| True -> True
| False -> False
| Var x -> Var x
| Map(x,e) -> Map(x,simplify_expr e)
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
  | IntConst n1,IntConst n2 -> IntConst (n1-n2)
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
| IfE(e1,e2,e3) -> (match simplify_expr e1 with
  | True -> simplify_expr e2
  | False -> simplify_expr e3
  | e1' -> IfE(e1',simplify_expr e2,simplify_expr e3))

(******************************************************************************)
(*                                NF1: no nested if-then-else                  *)

(******************************************************************************)
(*                            NF1: no nested if-then-else                     *)
(******************************************************************************)

let is_nf1_cmd1 = function 
| IfNF(_) -> false
| ReqNF(_) -> false
| _ -> true

let is_nf1_cmd = function
| [IfNF bl]
| [ReqNF(_);IfNF bl] -> List.fold_left (fun b (_,c) -> b && (List.for_all is_nf1_cmd1 c)) true bl
| ReqNF(_)::cl 
| cl -> List.for_all is_nf1_cmd1 cl

let is_nf1_fun = function
  | ConstrNF(_,_,c,_) 
  | ProcNF(_,_,_,c,_) -> is_nf1_cmd c

let rec list_of_fun_decls = function
  | EmptyFunDeclsNF -> []
  | FunDeclSeqNF(f,fl) -> f :: (list_of_fun_decls fl)

let is_nf1 = function
  ContractNF(_,_,fdl) -> List.for_all is_nf1_fun (list_of_fun_decls fdl)


let rec subst_var x e = function
| True -> True
| False -> False
| Var y when y=x -> e
| Var y -> Var y
| Map(y,e') -> Map(y,subst_var x e e')
| IntConst n -> IntConst n
| AddrConst n -> AddrConst n
| StringConst s -> StringConst s
| Not e1 -> Not (subst_var x e e1)
| And(e1,e2) -> And(subst_var x e e1,subst_var x e e2)
| Or(e1,e2) -> Or(subst_var x e e1,subst_var x e e2)
| Add(e1,e2) -> Add(subst_var x e e1,subst_var x e e2)
| Sub(e1,e2) -> Sub(subst_var x e e1,subst_var x e e2)
| Mul(e1,e2) -> Mul(subst_var x e e1,subst_var x e e2)
| Div(e1,e2) -> Div(subst_var x e e1,subst_var x e e2)
| Eq(e1,e2) -> Eq(subst_var x e e1,subst_var x e e2)
| Neq(e1,e2) -> Neq(subst_var x e e1,subst_var x e e2)
| Leq(e1,e2) -> Leq(subst_var x e e1,subst_var x e e2)
| Le(e1,e2) -> Le(subst_var x e e1,subst_var x e e2) 
| Geq(e1,e2) -> Geq(subst_var x e e1,subst_var x e e2)
| Ge(e1,e2) -> Ge(subst_var x e e1,subst_var x e e2)
| Bal(t) -> Bal(t)
| IfE(e1,e2,e3) -> IfE(subst_var x e e1,subst_var x e e2,subst_var x e e3)

let rec subst_map x e e' = function
| True -> True
| False -> False
| Var y -> Var y
| Map(y,ey) when y=x -> IfE(Eq(ey,e),e',Map(y,subst_map x e e' ey))
| Map(y,ey) -> Map(y,subst_var x e ey)
| IntConst n -> IntConst n
| AddrConst n -> AddrConst n
| StringConst s -> StringConst s
| Not e1 -> Not (subst_map x e e' e1)
| And(e1,e2) -> And(subst_map x e e' e1,subst_map x e e' e2)
| Or(e1,e2)  -> Or (subst_map x e e' e1,subst_map x e e' e2)
| Add(e1,e2) -> Add(subst_map x e e' e1,subst_map x e e' e2)
| Sub(e1,e2) -> Sub(subst_map x e e' e1,subst_map x e e' e2)
| Mul(e1,e2) -> Mul(subst_map x e e' e1,subst_map x e e' e2)
| Div(e1,e2) -> Div(subst_map x e e' e1,subst_map x e e' e2)
| Eq(e1,e2)  -> Eq (subst_map x e e' e1,subst_map x e e' e2)
| Neq(e1,e2) -> Neq(subst_map x e e' e1,subst_map x e e' e2)
| Leq(e1,e2) -> Leq(subst_map x e e' e1,subst_map x e e' e2)
| Le(e1,e2)  -> Le (subst_map x e e' e1,subst_map x e e' e2) 
| Geq(e1,e2) -> Geq(subst_map x e e' e1,subst_map x e e' e2)
| Ge(e1,e2)  -> Ge (subst_map x e e' e1,subst_map x e e' e2)
| Bal(t) -> Bal(t)
| IfE(e1,e2,e3) -> IfE(subst_map x e e' e1,subst_map x e e' e2,subst_map x e e' e3)

let rec subst_bal x (e:expr) t = function
| Bal(t') when t'=t -> Sub(Bal(t),e)
| Bal(t') -> Bal(t')                  
| Map(y,e') -> Map(y,subst_bal x e t e')
| Not e1 -> Not (subst_bal x e t e1)
| And(e1,e2) -> And(subst_bal x e t e1,subst_bal x e t e2)
| Or(e1,e2) -> Or(subst_bal x e t e1,subst_bal x e t e2)
| Add(e1,e2) -> Add(subst_bal x e t e1,subst_bal x e t e2)
| Sub(e1,e2) -> Sub(subst_bal x e t e1,subst_bal x e t e2)
| Mul(e1,e2) -> Mul(subst_bal x e t e1,subst_bal x e t e2)
| Div(e1,e2) -> Div(subst_bal x e t e1,subst_bal x e t e2)
| Eq(e1,e2) -> Eq(subst_bal x e t e1,subst_bal x e t e2)
| Neq(e1,e2) -> Neq(subst_bal x e t e1,subst_bal x e t e2)
| Leq(e1,e2) -> Leq(subst_bal x e t e1,subst_bal x e t e2)
| Le(e1,e2) -> Le(subst_bal x e t e1,subst_bal x e t e2) 
| Geq(e1,e2) -> Geq(subst_bal x e t e1,subst_bal x e t e2)
| Ge(e1,e2) -> Ge(subst_bal x e t e1,subst_bal x e t e2)
| e -> e

let nf1_push_assign_if = function
| (VarAssignNF(x,e), IfNF bl) -> IfNF(List.map (fun (ei,ci) -> (subst_var x e ei,VarAssignNF(x,e)::ci)) bl)
| (MapAssignNF(x,e,e'), IfNF bl) -> IfNF(List.map (fun (ei,ci) -> (subst_map x e e' ei,MapAssignNF(x,e,e')::ci)) bl)
| _ -> failwith "nf1_push_assign_if"

let nf1_push_send_if = function
| (SendNF(a,e,t), IfNF(bl)) -> IfNF(List.map (fun (ei,ci) -> (subst_bal a e t ei,SendNF(a,e,t)::ci)) bl)
| _ -> failwith "nf1_push_assign_if"

let nf1_push_if_cmd = function
| (IfNF(bl),c) -> IfNF(List.map (fun (ei,ci) -> (ei,ci@[c])) bl)
| _ -> failwith "nf1_push_if_cmd"

let nf1_pull_assign_req = function
| (VarAssignNF(x,e),ReqNF(er)) -> [ReqNF(subst_var x e er); VarAssignNF(x,e)]
| (MapAssignNF(x,e,e'),ReqNF(er)) -> [ReqNF(subst_map x e e' er); MapAssignNF(x,e,e')]
| _ -> failwith "nf1_pull_assign_req"

let nf1_pull_send_req = function
| (SendNF(a,e,t),ReqNF(er)) -> [ReqNF(subst_bal a e t er); SendNF(a,e,t)]
| _ -> failwith "nf1_pull_send_req"

(* merges the branch conditions with the req conditions *)
let bexpr_of_if_req = List.fold_left
  (fun b (ei,ci) -> Or(b,(match ci with
  | ReqNF(er)::_ -> And(ei,er)
  | _ -> True)))
  False

let nf1_drop_if_req = List.map 
  (fun (ei,ci) -> (ei, match ci with 
  | ReqNF(_)::cl -> cl
  | cl -> cl))

let rec nf1_cmd = function
| c when is_nf1_cmd c -> c
| [IfNF bl] -> bl 
  |> List.map (fun (ei,ci) -> (ei,nf1_cmd ci)) 
  |> List.map (fun (ei,ci) -> match ci with 
    | [IfNF bl'] -> List.map (fun (ei',ci') -> (And(ei,ei'),ci')) bl'
    | _ -> [(ei,ci)])
  |> List.flatten
  |> fun bl1 -> [ ReqNF (simplify_expr (bexpr_of_if_req bl1)) ; IfNF (nf1_drop_if_req bl1) ]
| VarAssignNF(x,e)::IfNF(bl)::cl -> nf1_cmd ((nf1_push_assign_if (VarAssignNF(x,e), IfNF bl))::cl)
| MapAssignNF(x,e,e')::IfNF(bl)::cl -> nf1_cmd ((nf1_push_assign_if (MapAssignNF(x,e,e'), IfNF bl))::cl)
| VarAssignNF(x,e)::ReqNF(er)::cl -> nf1_cmd ((nf1_pull_assign_req (VarAssignNF(x,e), ReqNF(er)))@cl)
| MapAssignNF(x,e,e')::ReqNF(er)::cl -> nf1_cmd ((nf1_pull_assign_req (MapAssignNF(x,e,e'), ReqNF(er)))@cl)
| SendNF(a,e,t)::ReqNF(er)::cl -> nf1_cmd ((nf1_pull_send_req (SendNF(a,e,t), ReqNF(er)))@cl)
| ReqNF(e1)::ReqNF(e2)::cl -> nf1_cmd (ReqNF(And(e1,e2))::cl)
| SendNF(x,e,t)::IfNF(bl)::cl -> nf1_cmd ((nf1_push_send_if (SendNF(x,e,t), IfNF(bl)))::cl)
| IfNF(bl)::c::cl -> nf1_cmd ((nf1_push_if_cmd (IfNF(bl),c))::cl)
| c::cl -> nf1_cmd (c::nf1_cmd cl)
| _ -> failwith "nf1_cmd"

let nf1_fun = function
  | ConstrNF(a,fml,c,nl) -> ConstrNF(a,fml,nf1_cmd c,nl) 
  | ProcNF(f,a,fml,c,nl) -> ProcNF(f,a,fml,nf1_cmd c,nl)

let rec nf1_fun_decls = function
| EmptyFunDeclsNF -> EmptyFunDeclsNF
| FunDeclSeqNF(f,fl) -> FunDeclSeqNF(nf1_fun f,nf1_fun_decls fl)

let nf1 = function
  ContractNF(x,vl,fdl) -> ContractNF(x,vl,nf1_fun_decls fdl)
