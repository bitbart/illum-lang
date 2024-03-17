open Ast
open Utils
open Nf
open Simplify_expr

(******************************************************************************)
(*                                 HeLLUM -> ILLUM compiler                   *)
(******************************************************************************)

let clause_names (f:string) = f, "Post-" ^ f

let tokbal t = "bal_" ^ t

let tokvars tl = 
  tl
  |> List.map (fun t -> tokbal t) 

let decs authl afterAl afterRl = { auth = authl; afterAbs = afterAl; afterRel = afterRl }

let hllc_pay_clause a v t = 
  {
  name = "Pay";
  spar = [a;v;t];
  dpar = [];
  walp = [(Var v,t)];
  prep = True;
  cntr = [ (decs [] [] [], Send([a,Var v,t])) ] 
}

let hllc_check_clause b = 
  {
  name = "Check";
  spar = [b];
  dpar = [];
  walp = [];
  prep = Var b;
  cntr = [ (decs [] [] [], Send([])) ]
  (* Send ["Null",IntConst 0,"T"] *) 
}

let rec rw_balances inl = function
| Map(e1,e2) -> Map(rw_balances inl e1, rw_balances inl e2)
| Not e -> Not (rw_balances inl e) 
| And(e1,e2) -> And(rw_balances inl e1, rw_balances inl e2)
| Or (e1,e2) -> Or (rw_balances inl e1, rw_balances inl e2) 
| Add(e1,e2) -> Add(rw_balances inl e1, rw_balances inl e2)
| Sub(e1,e2) -> Sub(rw_balances inl e1, rw_balances inl e2)
| Mul(e1,e2) -> Mul(rw_balances inl e1, rw_balances inl e2)
| Div(e1,e2) -> Div(rw_balances inl e1, rw_balances inl e2)
| Eq (e1,e2) -> Eq (rw_balances inl e1, rw_balances inl e2)
| Neq(e1,e2) -> Neq(rw_balances inl e1, rw_balances inl e2)
| Leq(e1,e2) -> Leq(rw_balances inl e1, rw_balances inl e2)
| Le (e1,e2) -> Le (rw_balances inl e1, rw_balances inl e2)  
| Geq(e1,e2) -> Geq(rw_balances inl e1, rw_balances inl e2) 
| Ge (e1,e2) -> Ge (rw_balances inl e1, rw_balances inl e2)
| Bal(t) -> Var (tokbal t)
| BalPre(t) -> Var (tokbal t)
  (* (match List.assoc_opt t inl with Some e -> Add(Var (tokbal t),e) | None -> Var (tokbal t)) *)
| IfE(e1,e2,e3) -> IfE(rw_balances inl e1, rw_balances inl e2, rw_balances inl e3)
| MapUpd(e1,e2,e3) -> MapUpd(rw_balances inl e1, rw_balances inl e2, rw_balances inl e3)
| c -> c

let hllc_body_branch_pay inputs cl = cl

(* construct list of parameters for Pay calls *)
|> List.fold_left (fun d c -> match c with XferNF(x,e,t) -> [Var x;e;Var t]::d | _ -> d) []
(* construct Pay calls *)
|> List.map (fun args -> ("Pay",List.map (rw_balances (List.map (fun (e,t) -> (t,simplify_expr e)) inputs)) args))

let hllc_body_branch_post f inputs cl = 
  cl 
  |> List.filter (fun c -> match c with SimAssign _ -> true | _ -> false)
  |> function 
  | [ SimAssign al ] ->
    al 
    |> List.map snd
    |> List.map simplify_expr
    |> List.map (rw_balances (List.map (fun (e,t) -> (t,simplify_expr e)) inputs))
    |> fun l -> (snd (clause_names f),l) 
  | _ -> failwith "hllc_body_branch_post: cannot happen"

let hllc_body_branch_check b inputs = if b=True then [] else 
  let b' = rw_balances (List.map (fun (e,t) -> (t,simplify_expr e)) inputs) b in ["Check",[b']]

let hllc_body_branch f b inputs cl = 
  let pays = hllc_body_branch_pay inputs cl in
  let post = hllc_body_branch_post f inputs cl in
  let chck = hllc_body_branch_check b inputs  in
  [ Call (chck @ pays @ [post]) ]

let hllc_body_cmd f inputs = function
| [] -> []
| [IfNF bl] -> 
  bl 
  |> List.fold_left (fun (b,l) (e,cl) -> (Or(b,e), (simplify_expr (And(Not b,e)),cl)::l)) (False,[])
  |> snd
  |> List.map (fun (e,cl) -> hllc_body_branch f e inputs cl)
  |> List.flatten
  |> List.rev
| cl -> hllc_body_branch f True inputs cl

let funding_pre (tl:tok list) (inputs:(expr * tok) list) : (expr * tok) list = 
  let rev_inputs = List.map (fun (e,t) -> (t,e)) inputs in
  tl 
  |> List.map (fun t -> (Var (tokbal t),t))
  |> List.map (fun (e,t) -> match List.assoc_opt t rev_inputs with Some e' -> (Add(e,rw_balances inputs e'),t) | None -> (e,t))
 
let hllc_body f al fml xl tl cl = 
  let (b,cl') = match cl with 
  | ReqNF e::tl -> (simplify_expr (rw_balances [] e),tl) (* CHECK! *)
  | _ -> (True,cl)
  in
{
  name = fst (clause_names f);
  spar = xl @ (tokvars tl);
  dpar = al;
  walp = funding_pre tl fml.inputs;
  prep = b;
  cntr =  
    hllc_body_cmd f fml.inputs cl'
    |> List.map (fun c -> (decs [] [] [],c)) 
}

let hllc_post_branch xl tl = function
 | ConstrNF(_,_,_,_,_) -> failwith "hllc_post_branch: should never happen"  
 | ProcNF(g,_,fml,_,_,_) -> 
    let exl = List.map (fun x -> Var x) xl in
    let etl = List.map (fun t -> Var (tokbal t)) tl in
    (decs fml.auths fml.afters [], Call [(g,exl @ etl)]) (* FIXME: questionmark parameter *)

let hllc_post f xl tl nl fdl =
{
  name = snd (clause_names f);
  spar = xl @ (tokvars tl);
  dpar = [];
  walp = List.map (fun t -> (Var (tokbal t),t)) tl;
  prep = True;
  cntr =
  List.filter (fun fd -> match fd with ProcNF(g,_,_,_,_,_) -> List.mem g nl | _ -> false) fdl
  |> List.map (hllc_post_branch xl tl) 
}

let hllc_fun xl tl fdl = function
  | ConstrNF(al,fml,_,cl,nl) -> [ hllc_body "constructor" (List.map fst al) fml xl tl cl; hllc_post "constructor" xl tl nl fdl ]
  | ProcNF(f,al,fml,_,cl,nl) -> [ hllc_body f (List.map fst al) fml xl tl cl; hllc_post f xl tl nl fdl ]

let complete_next f_univ = function
| [] -> f_univ
| nl -> nl

let fix_next f_univ = function
| ConstrNF(a,fml,vdl,cl,nl) -> ConstrNF(a,fml,vdl,cl,complete_next f_univ nl)
| ProcNF(f,al,fml,vdl,cl,nl) -> ProcNF(f,al,fml,vdl,cl,complete_next f_univ nl)

let hllc_nf = function ContractNF(cname,vl,fdl) ->
  let f_univ = List.fold_left (fun fl fd -> match fd with ProcNF(f,_,_,_,_,_) -> f::fl | _ -> fl) [] fdl in 
  let fdl' = List.map (fix_next f_univ) fdl in 
  let tl = toks_of_contract (ContractNF(cname,vl,fdl)) in
  List.flatten (List.map (fun fd -> hllc_fun (List.map fst vl) tl fdl' fd) fdl')
   @ [ hllc_pay_clause "a" "v" "t"; hllc_check_clause "b" ]

let hllc (c:contract) : clause list =
  c |> nf |> hllc_nf 
