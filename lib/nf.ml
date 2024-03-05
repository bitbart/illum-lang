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
(*                            NF1: no nested if-then-else                     *)
(******************************************************************************)

let is_nf1_cmd1 = function 
| IfNF(_) -> false
| _ -> true

let rec is_nf1_cmd = function
| [IfNF(ifl)] -> List.fold_left (fun b (_,c) -> b && is_nf1_cmd c) true ifl
| l -> List.for_all is_nf1_cmd1 l

let is_nf1_fun = function
  | ConstrNF(_,_,c,_) 
  | ProcNF(_,_,_,c,_) -> is_nf1_cmd c

let rec list_of_fun_decls = function
  | EmptyFunDeclsNF -> []
  | FunDeclSeqNF(f,fl) -> f :: (list_of_fun_decls fl)

let is_nf1 = function
  ContractNF(_,_,fdl) -> List.for_all is_nf1_fun (list_of_fun_decls fdl)


let rec subst_var x (e:expr) = function
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
| (VarAssignNF(x,e), IfNF(bl)) -> IfNF(List.map (fun (ei,ci) -> (subst_var x e ei,VarAssignNF(x,e)::ci)) bl)
| _ -> failwith "nf1_push_assign_if"

let nf1_push_send_if = function
| (SendNF(x,e,t), IfNF(bl)) -> IfNF(List.map (fun (ei,ci) -> (subst_bal x e t ei,SendNF(x,e,t)::ci)) bl)
| _ -> failwith "nf1_push_assign_if"

let nf1_push_if_cmd = function
| (IfNF(bl),c) -> IfNF(List.map (fun (ei,ci) -> (ei,ci@[c])) bl)
| _ -> failwith "nf1_push_if_cmd"

let rec nf1_cmd = function
| c when is_nf1_cmd c -> c
| [IfNF bl] -> bl 
  |> List.map (fun (ei,ci) -> (ei,nf1_cmd ci)) 
  |> List.map (fun (ei,ci) -> match ci with 
    | [IfNF bl'] -> [ List.map (fun (ei',ci') -> (And(ei,ei'),ci')) bl' ]
    | _ -> (ei,ci)) (* manca il flatten! *)
  |> fun bl1 -> [IfNF bl1] 
| VarAssignNF(x,e)::IfNF(bl)::cl -> nf1_cmd ((nf1_push_assign_if (VarAssignNF(x,e), IfNF(bl)))::cl)
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
