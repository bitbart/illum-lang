open Ast
open Utils
open Simplify_expr
open Typecheck

(******************************************************************************)
(*                          NF0: lists for seq and if branches                *)
(******************************************************************************)

let rec cmdNF_of_cmd = function
| Skip -> []
| VarAssign(x,e) -> [VarAssignNF(x,e)]
(* | MapAssign(x,e1,e2) -> [MapAssignNF(x,e1,e2)] *)
| Xfer(x,e,t) -> [XferNF(x,e,t)]
| Req(e) -> [ReqNF(e)]
| If(e,c1,c2) -> let c1' = cmdNF_of_cmd c1 in 
  (match cmdNF_of_cmd c2 with
  | [IfNF l2] -> [IfNF ((e,c1')::l2)]
  | c2' -> [IfNF [(e,c1');(True,c2')]])
| Seq(c1,c2) -> (cmdNF_of_cmd c1) @ (cmdNF_of_cmd c2) 

let fmodsNF_of_fmods fml = 
  let rec fmodsNF_of_fmods_rec = function
    | EmptyFMods -> [] 
    | FModSeq(f,m') -> f::(fmodsNF_of_fmods_rec m') in
  let fml' = fmodsNF_of_fmods_rec fml in 
{ 
  auths  = List.fold_left (fun l m -> match m with AuthFMod x -> x::l | _ -> l) [] fml';
  afters = List.fold_left (fun l m -> match m with AfterFMod e -> e::l | _ -> l) [] fml';
  inputs = List.fold_left (fun l m -> match m with InputFMod(e,t) -> (e,t)::l | _ -> l) [] fml';
}

let rec var_declsNF_of_var_decls = function
| EmptyVarDecls -> []
| VarDeclSeq(VarDecl(t,x),vl') -> (x,TBase t)::(var_declsNF_of_var_decls vl') 
| VarDeclSeq(MapDecl(t1,t2,x),vl') -> (x,TMap(t1,t2))::(var_declsNF_of_var_decls vl')

let fun_declNF_of_fun_decl = function
  | Constr(xl,fml,vdl,c,nl) -> ConstrNF(xl,fmodsNF_of_fmods fml,var_declsNF_of_var_decls vdl,cmdNF_of_cmd c,nl)
  | Proc(f,xl,fml,vdl,c,nl) -> ProcNF(f,xl,fmodsNF_of_fmods fml,var_declsNF_of_var_decls vdl,cmdNF_of_cmd c,nl)

let rec fun_declsNF_of_fun_decls = function
  | EmptyFunDecls -> []
  | FunDeclSeq(f,fl) -> (fun_declNF_of_fun_decl f)::(fun_declsNF_of_fun_decls fl)

let nf0 = function
  | Contract(x,vdl,fdl) -> ContractNF(x,var_declsNF_of_var_decls vdl,fun_declsNF_of_fun_decls fdl)


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
  | ConstrNF(_,_,_,c,_) 
  | ProcNF(_,_,_,_,c,_) -> is_nf1_cmd c

let is_nf1 = function
  ContractNF(_,_,fdl) -> List.for_all is_nf1_fun fdl

let nf1_push_assign_if = function
| (VarAssignNF(x,e), IfNF bl) -> IfNF(List.map (fun (ei,ci) -> (simplify_expr (subst_var x e ei),VarAssignNF(x,e)::ci)) bl)
| _ -> failwith "nf1_push_assign_if"

let nf1_push_send_if = function
| (XferNF(x,e,t), IfNF bl) -> IfNF(List.map (fun (ei,ci) -> (simplify_expr (subst_bal t (Sub(Bal(t),e)) ei),XferNF(x,e,t)::ci)) bl)
| _ -> failwith "nf1_push_assign_if"

let nf1_push_if_cmd = function
| (IfNF bl,c) -> IfNF(List.map (fun (ei,ci) -> (ei,ci@[c])) bl)
| _ -> failwith "nf1_push_if_cmd"

let nf1_pull_assign_req = function
| (VarAssignNF(x,e),ReqNF(er)) -> [ReqNF(subst_var x e er); VarAssignNF(x,e)]
| _ -> failwith "nf1_pull_assign_req"

let nf1_pull_send_req = function
| (XferNF(x,e,t),ReqNF(er)) -> [ReqNF(subst_bal t (Sub(Bal(t),e)) er); XferNF(x,e,t)]
| _ -> failwith "nf1_pull_send_req: should never happen"

(* bexpr_of_if_req constructs the top-level require condition. *)
(* It merges the branch conditions with the req conditions in the if-else-branches *)
(* Tests: nf1/test6.hll, nf1/test7.hll *)
let bexpr_of_if_req bl = List.fold_left
  (fun (breq,bif) (ei,ci) -> 
    (Or(breq,(match ci with
    | ReqNF(er)::_ -> And(bif, And(ei,er))
    | _ -> And(bif,ei)))),
    And(bif,Not ei)
  )
  (False,True)
  bl
  |> fst

let nf1_has_req = List.fold_left 
  (fun b (_,ci) -> match ci with ReqNF(_)::_ -> true | _ -> b)
  false

let nf1_drop_if_req = List.map 
  (fun (ei,ci) -> (ei, match ci with 
  | ReqNF(_)::cl -> cl
  | cl -> cl))

let rec nf1_cmd = function
| c when is_nf1_cmd c -> c
| [IfNF bl] -> bl 
  |> List.map (fun (ei,ci) -> (ei,nf1_cmd ci)) 
  |> List.map (fun (ei,ci) -> match ci with 
    | [IfNF bl'] -> List.map (fun (ei',ci') -> (simplify_expr (And(ei,ei')),ci')) bl'
    | _ -> [(ei,ci)])
  |> List.flatten
  |> fun bl1 -> 
    if nf1_has_req bl1 
    then [ ReqNF (simplify_expr (bexpr_of_if_req bl1)) ; IfNF (nf1_drop_if_req bl1) ]
    else [ IfNF bl1 ]
| VarAssignNF(x,e)::(IfNF bl)::cl -> nf1_cmd ((nf1_push_assign_if (VarAssignNF(x,e), IfNF bl))::cl)
| VarAssignNF(x,e)::ReqNF(er)::cl -> nf1_cmd ((nf1_pull_assign_req (VarAssignNF(x,e), ReqNF(er)))@cl)
| XferNF(x,e,t)::ReqNF(er)::cl -> nf1_cmd ((nf1_pull_send_req (XferNF(x,e,t), ReqNF(er)))@cl)
| ReqNF(e1)::ReqNF(e2)::cl -> nf1_cmd (ReqNF(And(e1,e2))::cl)
| XferNF(x,e,t)::IfNF(bl)::cl -> nf1_cmd ((nf1_push_send_if (XferNF(x,e,t), IfNF(bl)))::cl)
| (IfNF bl)::c::cl -> nf1_cmd ((nf1_push_if_cmd (IfNF bl,c))::cl) (* FIXME: require in test/nf1/test13.hll *)
| c::cl -> nf1_cmd (c::nf1_cmd cl)
| _ -> failwith "nf1_cmd: should never happen"

let nf1_fun = function
  | ConstrNF(a,fml,vdl,c,nl) -> ConstrNF(a,fml,vdl,nf1_cmd c,nl) 
  | ProcNF(f,a,fml,vdl,c,nl) -> ProcNF(f,a,fml,vdl,nf1_cmd c,nl)

let nf1 = function
  ContractNF(x,vl,fdl) -> ContractNF(x, vl, List.map nf1_fun fdl)

(******************************************************************************)
(*                                       NF2: SSA                             *)
(******************************************************************************)

let is_ssa cl = List.fold_left
  (fun (b,wl) c -> match c with 
    | SimAssign al -> List.map fst al |> fun l -> (b && is_disjoint l wl, l@wl)
    | XferNF(_,_,_) -> (b,wl)
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
  | ConstrNF(_,_,_,c,_) 
  | ProcNF(_,_,_,_,c,_) -> is_nf2_cmd c

let is_nf2 = function
  ContractNF(_,_,fdl) -> List.for_all is_nf2_fun fdl

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

let rec rw_bal_in_balpre = function
| Bal(t)     -> BalPre(t)
| Map(e1,e2) -> Map(rw_bal_in_balpre e1, rw_bal_in_balpre e2)
| Not e ->      Not(rw_bal_in_balpre e) 
| And(e1,e2) -> And(rw_bal_in_balpre e1, rw_bal_in_balpre e2)
| Or (e1,e2) -> Or (rw_bal_in_balpre e1, rw_bal_in_balpre e2) 
| Add(e1,e2) -> Add(rw_bal_in_balpre e1, rw_bal_in_balpre e2)
| Sub(e1,e2) -> Sub(rw_bal_in_balpre e1, rw_bal_in_balpre e2)
| Mul(e1,e2) -> Mul(rw_bal_in_balpre e1, rw_bal_in_balpre e2)
| Div(e1,e2) -> Div(rw_bal_in_balpre e1, rw_bal_in_balpre e2)
| Eq (e1,e2) -> Eq (rw_bal_in_balpre e1, rw_bal_in_balpre e2)
| Neq(e1,e2) -> Neq(rw_bal_in_balpre e1, rw_bal_in_balpre e2)
| Leq(e1,e2) -> Leq(rw_bal_in_balpre e1, rw_bal_in_balpre e2)
| Le (e1,e2) -> Le (rw_bal_in_balpre e1, rw_bal_in_balpre e2)  
| Geq(e1,e2) -> Geq(rw_bal_in_balpre e1, rw_bal_in_balpre e2) 
| Ge (e1,e2) -> Ge (rw_bal_in_balpre e1, rw_bal_in_balpre e2)
| IfE(e1,e2,e3) ->    IfE   (rw_bal_in_balpre e1, rw_bal_in_balpre e2, rw_bal_in_balpre e3)
| MapUpd(e1,e2,e3) -> MapUpd(rw_bal_in_balpre e1, rw_bal_in_balpre e2, rw_bal_in_balpre e3)
| e -> e

let add_inputs t inl = 
  let inl' = List.map (fun (e',t') -> (t',e')) inl in
  match List.assoc_opt t inl' with Some e -> let e' = rw_bal_in_balpre e in Add(BalPre(t),e') | None -> BalPre(t)

let rec add_inputs_req inl = function
| Bal(t)     ->  
  List.map (fun (e',t') -> (t',e')) inl
  |> fun inl' -> (match List.assoc_opt t inl' with Some e -> Add(BalPre(t),e) | None -> BalPre(t))
| Map(e1,e2) -> Map(add_inputs_req inl e1, rw_bal_in_balpre e2)
| Not e ->      Not(add_inputs_req inl e) 
| And(e1,e2) -> And(add_inputs_req inl e1, add_inputs_req inl e2)
| Or (e1,e2) -> Or (add_inputs_req inl e1, add_inputs_req inl e2) 
| Add(e1,e2) -> Add(add_inputs_req inl e1, add_inputs_req inl e2)
| Sub(e1,e2) -> Sub(add_inputs_req inl e1, add_inputs_req inl e2)
| Mul(e1,e2) -> Mul(add_inputs_req inl e1, add_inputs_req inl e2)
| Div(e1,e2) -> Div(add_inputs_req inl e1, add_inputs_req inl e2)
| Eq (e1,e2) -> Eq (add_inputs_req inl e1, add_inputs_req inl e2)
| Neq(e1,e2) -> Neq(add_inputs_req inl e1, add_inputs_req inl e2)
| Leq(e1,e2) -> Leq(add_inputs_req inl e1, add_inputs_req inl e2)
| Le (e1,e2) -> Le (add_inputs_req inl e1, add_inputs_req inl e2)  
| Geq(e1,e2) -> Geq(add_inputs_req inl e1, add_inputs_req inl e2) 
| Ge (e1,e2) -> Ge (add_inputs_req inl e1, add_inputs_req inl e2)
| IfE(e1,e2,e3) ->    IfE   (add_inputs_req inl e1, add_inputs_req inl e2, add_inputs_req inl e3)
| MapUpd(e1,e2,e3) -> MapUpd(add_inputs_req inl e1, add_inputs_req inl e2, add_inputs_req inl e3)
| e -> e


let simassign_init xl tl zl ins =
  let zl' = diff zl xl in 
  let xl0,tl0,zl0 = (ssa_vars xl,ssa_toks tl,ssa_vars zl') in
  SimAssign(
    (List.map2 (fun y x -> (y,Var x)) (xl0 @ zl0) (xl @ zl')) @
    (List.map2 (fun y x -> (y,add_inputs x ins)) tl0 tl)
  )

let simassign_end xl tl st =
  SimAssign(
    (List.map (fun x -> (x,Var (ssa_var st x))) xl) @
    (List.map (fun t -> (ssa_tok_fin t,Var (ssa_tok st t))) tl)
  )

let ssa_rw_expr st tl e =  
  (* (List.fold_left (fun _ s -> print_endline s) () xl); *)
  let xl = vars_of_expr e in
  (* replaces the variables in e with SSA variables *)
  let e1 = List.fold_left (fun e' x -> subst_var x (Var (ssa_var st x)) e') e xl in
  (* replaces the balance(T) subexpr in e with SSA variables *)
  List.fold_left (fun e' t -> subst_bal t (Var (ssa_tok st t)) e') e1 tl

let ssa_rw_cmd1 st tl = function
| VarAssignNF(x,e) -> let st' = ssa_inc st x in ([SimAssign [ssa_var st' x, ssa_rw_expr st tl e]], st')
| XferNF(x,e,t) -> let st' = ssa_inc st t in 
  let e' = Sub(Bal(t),e) in
  (
    [
    XferNF(ssa_var st x,ssa_rw_expr st tl e,t); (* check x *)
    SimAssign([ssa_tok st' t, ssa_rw_expr st tl e'])
    ], 
    st'
  )
| _ -> failwith "ssa_rw_cmd1: should not happen"

let nf2_cmd1_list tl cl =
  let st0 = fun _ -> 0 in 
  List.fold_left (fun (l,st) c -> let (cl',st') = ssa_rw_cmd1 st tl c in (l@cl', st')) ([],st0) cl
  
let nf2_if xl tl zl inl bl =
  let c_init = simassign_init xl tl zl inl in
  bl 
  |> List.map (fun (ei,ci) -> (ei,nf2_cmd1_list tl ci))
  |> fun y -> [ IfNF (List.map (fun (ei,(ci,sti)) -> (ei,(c_init::ci)@[simassign_end xl tl sti])) y) ]

let nf2_cmd xl tl zl inl = function
| [] -> []
| [(ReqNF er); IfNF bl] ->
    let er' = simplify_expr (add_inputs_req inl er) in 
    (ReqNF er')::(nf2_if xl tl zl inl bl) 
| [IfNF bl] -> nf2_if xl tl zl inl bl 
| (ReqNF er)::cl -> 
    let c_init = simassign_init xl tl zl inl in
    let (cl',st') = nf2_cmd1_list tl cl in
    let c_end = simassign_end xl tl st' in
    let er' = simplify_expr (add_inputs_req inl er) in
    (ReqNF er')::(c_init::cl')@[c_end]
| cl -> 
    let c_init = simassign_init xl tl zl inl in
    let (cl',st') = nf2_cmd1_list tl cl in
    let c_end = simassign_end xl tl st' in
    (c_init::cl')@[c_end]

let nf2_fun xl tl = function
  | ConstrNF(al,fml,vdl,c,nl) -> ConstrNF(al,fml,vdl,nf2_cmd xl tl (List.map fst al) fml.inputs c,nl) 
  | ProcNF(f,al,fml,vdl,c,nl) -> ProcNF(f,al,fml,vdl,nf2_cmd xl tl (List.map fst al) fml.inputs c,nl)

let nf2 = function ContractNF(x,vl,fdl) -> 
  let tl = toks_of_contract (ContractNF(x,vl,fdl)) in
  ContractNF(x,vl,List.map (fun f -> nf2_fun (List.map fst vl) tl f) fdl)

(******************************************************************************)
(*                              NF3: move transfers up                        *)
(******************************************************************************)

let rec is_nf3_assign = function
| [] -> true
| SimAssign _ ::cl -> is_nf3_assign cl
| _ -> false

let rec is_nf3_send = function
| [] -> true 
| XferNF _ ::cl -> is_nf3_send cl
| SimAssign _ ::cl -> is_nf3_assign cl
| _ -> false

let is_nf3_cmd = function
| [IfNF bl]
| [ReqNF(_);IfNF bl] -> bl |> List.map snd |> List.for_all is_nf3_send 
| ReqNF(_)::cl -> is_nf3_send cl
| cl -> is_nf3_send cl

let is_nf3_fun = function
  | ConstrNF(_,_,_,c,_) 
  | ProcNF(_,_,_,_,c,_) -> is_nf3_cmd c

let is_nf3 = function
  ContractNF(_,_,fdl) -> List.for_all is_nf3_fun fdl

let rec nf3_cmd = function
| c when is_nf3_cmd c -> c
| ReqNF(e)::cl -> ReqNF(e)::(nf3_cmd cl)
| XferNF(x,e,t)::cl -> XferNF(x,e,t)::(nf3_cmd cl)
| SimAssign(al)::XferNF(x,e,t)::cl -> 
  let af = fun_of_list al in
  let x' = match simsubst af (Var x) with
  | Var y -> y
  | _ -> failwith "nf3_cmd: should never happen" in
  XferNF(x',simsubst af e,t)::(nf3_cmd (SimAssign(al)::cl))
| [IfNF bl] -> List.map (fun (ei,ci) -> (ei,nf3_cmd ci)) bl |> fun bl' -> [IfNF bl']
| c::cl -> nf3_cmd (c::nf3_cmd cl)
| _ -> failwith "nf3_cmd: should never happen"

let nf3_fun = function
  | ConstrNF(al,fml,vdl,c,nl) -> ConstrNF(al,fml,vdl,nf3_cmd c,nl) 
  | ProcNF(f,al,fml,vdl,c,nl) -> ProcNF(f,al,fml,vdl,nf3_cmd c,nl)

let nf3 = function ContractNF(x,vl,fdl) -> 
  ContractNF(x,vl,List.map nf3_fun fdl)

(******************************************************************************)
(*                                 NF4: fold assignments                      *)
(******************************************************************************)

let rec is_nf4_send = function
| [] -> true 
| XferNF _ ::cl -> is_nf4_send cl
| [ SimAssign _ ] -> true
| _ -> false

let is_nf4_cmd = function
| [IfNF bl]
| [ReqNF(_);IfNF bl] -> bl |> List.map snd |> List.for_all is_nf4_send 
| ReqNF(_)::cl -> is_nf4_send cl
| cl -> is_nf4_send cl

let is_nf4_fun = function
  | ConstrNF(_,_,_,c,_) 
  | ProcNF(_,_,_,_,c,_) -> is_nf4_cmd c

let is_nf4 = function
  ContractNF(_,_,fdl) -> List.for_all is_nf4_fun fdl

let is_simassign = function
| SimAssign _ -> true
| _ -> false

let collapse_simassign2 c1 c2 = match (c1,c2) with
| (SimAssign al1, SimAssign al2) -> 
  let al1' = List.map (fun (x,e) -> (x, simsubst (fun_of_list al2) e)) al1 in
  SimAssign(al1')
| _ -> failwith "collapse_simassign2: should never happen"

let collapse_simassign cl = match List.rev cl with
| [] -> SimAssign []
| c::cl' -> List.fold_left (fun c1 c2 -> collapse_simassign2 c1 c2) c cl'

let rec nf4_cmd = function
| [ReqNF er; IfNF bl] -> [ ReqNF er; IfNF (List.map (fun (ei,ci) -> (ei,nf4_cmd ci)) bl)]
| [IfNF bl] -> [IfNF (List.map (fun (ei,ci) -> (ei,nf4_cmd ci)) bl)]
| cl -> (List.filter (fun c -> not (is_simassign c)) cl) @
        [collapse_simassign (List.filter is_simassign cl)] 

let nf4_fun = function
  | ConstrNF(al,fml,vdl,c,nl) -> ConstrNF(al,fml,vdl,nf4_cmd c,nl) 
  | ProcNF(f,al,fml,vdl,c,nl) -> ProcNF(f,al,fml,vdl,nf4_cmd c,nl)

let nf4 = function ContractNF(x,vl,fdl) -> 
  ContractNF(x,vl,List.map nf4_fun fdl)

  let nf c = c |> nf0 |> typecheck |> nf1 |> nf2 |> nf3 |> nf4 