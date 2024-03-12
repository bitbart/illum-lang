open Ast
open Utils
open Nf

(******************************************************************************)
(*                                 HeLLUM -> ILLUM compiler                   *)
(******************************************************************************)

let clause_names (f:string) = f, "Post-" ^ f

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
  cntr = [ (decs [] [] [], Send(["Null",IntConst 0,"T"])) ] 
}

let hllc_body_branch_pay cl = cl
(* construct list of parameters for Pay calls *)
|> List.fold_left (fun d c -> match c with XferNF(x,e,t) -> [Var x;e;Var t]::d | _ -> d) []
(* construct Pay calls *)
|> List.map (fun args -> ("Pay",args))

let hllc_body_branch_post f cl = 
  cl 
  |> List.filter (fun c -> match c with SimAssign _ -> true | _ -> false)
  |> function 
  | [ SimAssign al ] ->
    al 
    |> List.map snd 
    |> fun l -> (snd (clause_names f),l) (* FIXME: Post-F parameters *)
  | _ -> failwith "hllc_body_branch_post: cannot happen"

let hllc_body_branch_check b = if b=True then [] else ["Check",[b]]

let hllc_body_branch f b cl = 
  let pays = hllc_body_branch_pay cl in
  let post = hllc_body_branch_post f cl in
  let chck = hllc_body_branch_check b in
  [ Call (chck @ pays @ [post]) ]

let hllc_body_cmd f = function
| [] -> []
| [IfNF bl] -> 
  bl 
  |> List.fold_left (fun l (e,cl) -> (e,cl)::l) [] (* FIXME: construct Check conditions *)
  |> List.map (fun (e,cl) -> hllc_body_branch f e cl)
  |> List.flatten
| cl -> hllc_body_branch f True cl

let hllc_body f al fml xl tl cl = 
  let (b,cl') = match cl with 
  | ReqNF e::tl -> (e,tl)
  | _ -> (True,cl)
in
print_endline "***FIXME: funding precondition";
{
  name = fst (clause_names f);
  spar = xl @ tl;
  dpar = al;
  walp = fml.inputs; (* FIXME *)
  prep = b;
  cntr =  
    hllc_body_cmd f cl' 
    |> List.map (fun c -> (decs [] [] [],c)) 
}

let hllc_post_branch = function
 | ConstrNF(_,_,_,_) -> failwith "hllc_post_branch: constructor not implemented"  
 | ProcNF(g,_,fml,_,_) -> 
    (decs fml.auths fml.afters [], Call [(g,[])]) (* FIXME: g parameters *)

let hllc_post f xl tl nl fdl =
{
  name = snd (clause_names f);
  spar = xl @ tl;
  dpar = [];
  walp = [];
  prep = True;
  cntr =
  List.filter (fun fd -> match fd with ProcNF(g,_,_,_,_) -> List.mem g nl | _ -> false) fdl
  |> List.map hllc_post_branch
}

let hllc_fun xl tl fdl = function
  | ConstrNF(_ (* a *),_ (* fml *),_ (* c *),_ (* nl *)) -> [] (* ConstrNF(a,fml,nf2_cmd xl (toks_of_cmd c) c,nl) *)
  | ProcNF(f,al,fml,cl,nl) -> [ hllc_body f (List.map snd al) fml xl tl cl; hllc_post f xl tl nl fdl ]

let hllc_nf = function ContractNF(_,vl,fdl) -> 
  List.flatten (List.map (fun fd -> hllc_fun (vars_of_var_decls vl) (toks_of_fun_decls fdl) fdl fd) fdl)
  @
  [ hllc_pay_clause "a" "v" "t"; hllc_check_clause "b" ]

let hllc (c:contract) : clause list =
  c |> nf |> hllc_nf 
