open Ast
open Utils
open Simplify_expr

(******************************************************************************)
(*                          NF0: lists for seq and if branches                *)
(******************************************************************************)

let rec cmdNF_of_cmd = function
| Skip -> []
| VarAssign(x,e) -> [VarAssignNF(x,e)]
(* | MapAssign(x,e1,e2) -> [MapAssignNF(x,e1,e2)] *)
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

(* subst_var x e e' replaces all the occurrences of base variable x in e' with e *)
let rec subst_var x e = function
| True -> True
| False -> False
| Var y when y=x -> e
| Var y -> Var y
| Map(e1,e2) -> Map(subst_var x e e1,subst_var x e e2)
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
| BalPre(t) -> BalPre(t)
| IfE(e1,e2,e3) -> IfE(subst_var x e e1,subst_var x e e2,subst_var x e e3)
| MapUpd(e1,e2,e3) -> MapUpd(subst_var x e e1,subst_var x e e2,subst_var x e e3)

(* subst_bal t e e' replaces all the occurrences of Bal(t) in e' with e *)
let rec subst_bal t (e:expr) = function
| Bal(t') when t'=t -> e
| Bal(t')    -> Bal(t')                  
| Map(y,e')  -> Map(y,subst_bal t e e')
| Not e1     -> Not(subst_bal t e e1)
| And(e1,e2) -> And(subst_bal t e e1,subst_bal t e e2)
| Or(e1,e2)  -> Or (subst_bal t e e1,subst_bal t e e2)
| Add(e1,e2) -> Add(subst_bal t e e1,subst_bal t e e2)
| Sub(e1,e2) -> Sub(subst_bal t e e1,subst_bal t e e2)
| Mul(e1,e2) -> Mul(subst_bal t e e1,subst_bal t e e2)
| Div(e1,e2) -> Div(subst_bal t e e1,subst_bal t e e2)
| Eq(e1,e2)  -> Eq (subst_bal t e e1,subst_bal t e e2)
| Neq(e1,e2) -> Neq(subst_bal t e e1,subst_bal t e e2)
| Leq(e1,e2) -> Leq(subst_bal t e e1,subst_bal t e e2)
| Le(e1,e2)  -> Le (subst_bal t e e1,subst_bal t e e2) 
| Geq(e1,e2) -> Geq(subst_bal t e e1,subst_bal t e e2)
| Ge(e1,e2)  -> Ge (subst_bal t e e1,subst_bal t e e2)
| e -> e

let nf1_push_assign_if = function
| (VarAssignNF(x,e), IfNF bl) -> IfNF(List.map (fun (ei,ci) -> (subst_var x e ei,VarAssignNF(x,e)::ci)) bl)
| _ -> failwith "nf1_push_assign_if"

let nf1_push_send_if = function
| (SendNF(a,e,t), IfNF(bl)) -> IfNF(List.map (fun (ei,ci) -> (subst_bal t (Sub(Bal(t),e)) ei,SendNF(a,e,t)::ci)) bl)
| _ -> failwith "nf1_push_assign_if"

let nf1_push_if_cmd = function
| (IfNF(bl),c) -> IfNF(List.map (fun (ei,ci) -> (ei,ci@[c])) bl)
| _ -> failwith "nf1_push_if_cmd"

let nf1_pull_assign_req = function
| (VarAssignNF(x,e),ReqNF(er)) -> [ReqNF(subst_var x e er); VarAssignNF(x,e)]
| _ -> failwith "nf1_pull_assign_req"

let nf1_pull_send_req = function
| (SendNF(a,e,t),ReqNF(er)) -> [ReqNF(subst_bal t (Sub(Bal(t),e)) er); SendNF(a,e,t)]
| _ -> failwith "nf1_pull_send_req: should never happen"

(* bexpr_of_if_req constructs the top-level require condition. *)
(* It merges the branch conditions with the req conditions in the if-else-branches *)
(* Tests: nf1/test6.hll, nf1/test7.hll *)
let bexpr_of_if_req bl = List.fold_left
  (fun (breq,bif) (ei,ci) -> 
    (Or(breq,(match ci with
    | ReqNF(er)::_ -> And(bif, And(ei,er))
    | _ -> True))),
    And(bif,Not ei)
  )
  (False,True)
  bl
  |> fst

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
  |> fun bl1 -> [ ReqNF ( simplify_expr (bexpr_of_if_req bl1)) ; IfNF (nf1_drop_if_req bl1) ]
| VarAssignNF(x,e)::IfNF(bl)::cl -> nf1_cmd ((nf1_push_assign_if (VarAssignNF(x,e), IfNF bl))::cl)
| VarAssignNF(x,e)::ReqNF(er)::cl -> nf1_cmd ((nf1_pull_assign_req (VarAssignNF(x,e), ReqNF(er)))@cl)
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

(******************************************************************************)
(*                                       NF2: SSA                             *)
(******************************************************************************)

let is_ssa cl = List.fold_left
  (fun (b,wl) c -> match c with 
    | SimAssign al -> List.map fst al |> fun l -> (b && is_disjoint l wl, l@wl)
    | SendNF(_,_,_) -> (b,wl)
    | SkipNF -> (true,wl)
    | _ -> (false,wl)
  )
  (true,[]) 
  cl
  |> fst

let is_nf2_cmd = function
| [] -> true
| [ReqNF _; IfNF bl]
| [IfNF bl] -> List.map snd bl |> List.for_all is_ssa
| ReqNF _ ::cl 
| cl -> is_ssa cl

let is_nf2_fun = function
  | ConstrNF(_,_,c,_) 
  | ProcNF(_,_,_,c,_) -> is_nf2_cmd c

let is_nf2 = function
  ContractNF(_,_,fdl) -> List.for_all is_nf2_fun (list_of_fun_decls fdl)

(* ssavars i xl decorates the state variables xl with the index i *)
(* Warning: this is potentially unsafe, e.g. if the contract state includes x and x_0 *)

(* ssa_var st x generates a variable identifier for variable x in SSA state st *)
let ssa_var st x = x ^ "_" ^ (string_of_int (st x))
and ssa_vars xl = List.fold_right (fun x xl' -> (x ^ "_0")::xl') xl [] 

(* ssa_tok st t generates a variable identifier for token t in SSA state st *)
let ssa_tok st t = "bal_" ^ t ^ "_" ^ (string_of_int (st t))
(* ssa_tok_fin x generates a variable identifier for the final balance of token t *)
and ssa_tok_fin t = "bal_" ^ t ^ "_fin"
and ssa_toks tl = List.fold_right (fun x tl' -> ("bal_" ^ x ^ "_0")::tl') tl []

(* ssa_inc st x increases the index of the SSA variable x in SSA state st *)
let ssa_inc st x = fun y -> if x=y then st x + 1 else st y

let simassign_init xl tl zl =
  let zl' = diff zl xl in 
  let xl0,tl0,zl0 = (ssa_vars xl,ssa_toks tl,ssa_vars zl') in
  SimAssign(
    (List.map2 (fun y x -> (y,Var x)) (xl0 @ zl0) (xl @ zl')) @
    (List.map2 (fun y x -> (y,BalPre x)) tl0 tl)
  )

let simassign_end xl tl st =
  SimAssign(
    (List.map (fun x -> (x,Var (ssa_var st x))) xl) @
    (List.map (fun t -> (ssa_tok_fin t,Var (ssa_tok st t))) tl)
  )

let ssa_rw_expr st e =  
  (* (List.fold_left (fun _ s -> print_endline s) () xl); *)
  let (xl,tl) = (vars_of_expr e, toks_of_expr e) in
  (* replaces the variables in e with SSA variables *)
  let e1 = List.fold_left (fun e' x -> subst_var x (Var (ssa_var st x)) e') e xl in
  (* replaces the balance(T) subexpr in e with SSA variables *)
  List.fold_left (fun e' t -> subst_bal t (Var (ssa_tok st t)) e') e1 tl

let ssa_rw_cmd1 st = function
| VarAssignNF(x,e) -> let st' = ssa_inc st x in ([SimAssign [ssa_var st' x, ssa_rw_expr st e]], st')
| SendNF(x,e,t) -> let st' = ssa_inc st t in 
  let e' = Sub(Bal(t),e) in
  (
    [
    SendNF(ssa_var st x,ssa_rw_expr st e,t);
    SimAssign([ssa_tok st' t, ssa_rw_expr st e'])
    ], 
    st'
  )
| _ -> failwith "ssa_rw_cmd1: should not happen"

let nf2_cmd1_list cl =
  let st0 = fun _ -> 0 in 
  List.fold_left (fun (l,st) c -> let (cl',st') = ssa_rw_cmd1 st c in (l@cl', st')) ([],st0) cl
  
let nf2_if xl tl zl bl =
  let c_init = simassign_init xl tl zl in
  bl 
  |> List.map (fun (ei,ci) -> (ei,nf2_cmd1_list ci))
  |> fun y -> [ IfNF (List.map (fun (ei,(ci,sti)) -> (ei,(c_init::ci)@[simassign_end xl tl sti])) y) ]

let nf2_cmd xl tl zl = function
| [] -> []
| [(ReqNF er); IfNF bl] -> (ReqNF er)::nf2_if xl tl zl bl
| [IfNF bl] -> nf2_if xl tl zl bl 
| (ReqNF er)::cl -> 
    let c_init = simassign_init xl tl zl in
    let (cl',st') = nf2_cmd1_list cl in
    let c_end = simassign_end xl tl st' in
    ((ReqNF er)::c_init::cl')@[c_end]
| cl -> 
    let c_init = simassign_init xl tl zl in
    let (cl',st') = nf2_cmd1_list cl in
    let c_end = simassign_end xl tl st' in
    (c_init::cl')@[c_end]

let nf2_fun xl = function
  | ConstrNF(al,fml,c,nl) -> ConstrNF(al,fml,nf2_cmd xl (toks_of_cmd c) (List.map snd al) c,nl) 
  | ProcNF(f,al,fml,c,nl) -> ProcNF(f,al,fml,nf2_cmd xl (toks_of_cmd c) (List.map snd al) c,nl)

let rec nf2_fun_decls xl = function
| EmptyFunDeclsNF -> EmptyFunDeclsNF
| FunDeclSeqNF(f,fl) -> FunDeclSeqNF(nf2_fun xl f,nf2_fun_decls xl fl)

let nf2 = function ContractNF(x,vl,fdl) -> 
  ContractNF(x,vl,nf2_fun_decls (vars_of_var_decls vl) fdl)

(******************************************************************************)
(*                              NF3: move transfers up                        *)
(******************************************************************************)

let rec is_nf3_assign = function
| [] -> true
| SimAssign _ ::cl -> is_nf3_assign cl
| _ -> false

let rec is_nf3_send = function
| [] -> true 
| SendNF _ ::cl -> is_nf3_send cl
| SimAssign _ ::cl -> is_nf3_assign cl
| _ -> false

let is_nf3_cmd = function
| [IfNF bl]
| [ReqNF(_);IfNF bl] -> List.fold_left (fun b (_,c) -> b && is_nf3_send c) true bl
| ReqNF(_)::cl -> is_nf3_send cl
| cl -> is_nf3_send cl

let rec nf3_cmd = function
| c when is_nf3_cmd c -> c
| ReqNF(e)::cl -> ReqNF(e)::(nf3_cmd cl)
| SendNF(a,e,t)::cl -> SendNF(a,e,t)::(nf3_cmd cl)
(* TODO: adjust expressions! *)
| SimAssign(al)::SendNF(a,e,t)::cl -> SendNF(a,e,t)::(nf3_cmd (SimAssign(al)::cl))
| [IfNF _] -> failwith "nf3_cmd: if case not implemented" 
| c::cl -> nf3_cmd (c::nf3_cmd cl)
| _ -> failwith "nf3_cmd"

let nf3_fun = function
  | ConstrNF(al,fml,c,nl) -> ConstrNF(al,fml,nf3_cmd c,nl) 
  | ProcNF(f,al,fml,c,nl) -> ProcNF(f,al,fml,nf3_cmd c,nl)

let rec nf3_fun_decls = function
| EmptyFunDeclsNF -> EmptyFunDeclsNF
| FunDeclSeqNF(f,fl) -> FunDeclSeqNF(nf3_fun f,nf3_fun_decls fl)

let nf3 = function ContractNF(x,vl,fdl) -> 
  ContractNF(x,vl,nf3_fun_decls fdl)
