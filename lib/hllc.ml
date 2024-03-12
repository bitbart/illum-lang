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

let hllc_cmd = function
| [] -> []
| [IfNF _] -> failwith "NOPE"
(* | [ XferNF(a,e,t); SimAssign(yl) ] -> failwith "NOPE" *)
| cl -> cl
|> List.fold_left (fun d c -> match c with XferNF(x,e,t) -> [Var x;e;Var t]::d | _ -> d) []
|> fun l -> [ Call (List.map (fun args -> ("Pay",args)) l) ]

let hllc_body f al fml xl tl cl = 
  let b = match cl with 
  | ReqNF e::_ -> e
  | _ -> True
in
print_endline "***FIXME: funding precondition";
{
  name = fst (clause_names f);
  spar = xl @ tl;
  dpar = al;
  walp = fml.inputs; (* FIXME *)
  prep = b;
  cntr =  
    hllc_cmd cl 
    |> List.map (fun c -> (decs [] [] [],c)) 
}

let hllc_post_branch = function
 | ConstrNF(_,_,_,_) -> failwith "hllc_post_branch: constructor not implemented"  
 | ProcNF(g,_,fml,_,_) -> 
    (decs fml.auths fml.afters [], Call [(g,[])])

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
