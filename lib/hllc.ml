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
  spar = [v;a];
  dpar = [];
  walp = [(Var v,t)];
  prep = True;
  cntr = [ (decs [] [] [], Send(a,Var v,t)) ] 
}

let hllc_check_clause b = 
  {
  name = "Check";
  spar = [b];
  dpar = [];
  walp = [];
  prep = Var b;
  cntr = [ (decs [] [] [], Send("Null",IntConst 0,"T")) ] 
}

let hllc_cmd = function
| [] -> []
| [IfNF _] -> failwith "NOPE"
(* | [ XferNF(a,e,t); SimAssign(yl) ] -> failwith "NOPE" *)
| cl -> cl
|> List.fold_left (fun d c -> match c with XferNF(x,e,t) -> [e;Var t;Var x]::d | _ -> d) []
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
  cntr = let d = { auth = []; afterAbs = []; afterRel = [] } in 
  hllc_cmd cl 
  |> List.map (fun c -> (d,c)) 
}

let hllc_fun xl tl = function
  | ConstrNF(_ (* a *),_ (* fml *),_ (* c *),_ (* nl *)) -> [] (* ConstrNF(a,fml,nf2_cmd xl (toks_of_cmd c) c,nl) *)
  | ProcNF(f,al,fml,cl,_ (* nl *)) -> [ hllc_body f (List.map snd al) fml xl tl cl ]

let rec hllc_fun_decls xl tl = function
| EmptyFunDeclsNF -> [ hllc_pay_clause "a" "v" "t"; hllc_check_clause "b" ]  
| FunDeclSeqNF(f,fl) -> (hllc_fun xl tl f) @ (hllc_fun_decls xl tl fl)

let hllc_nf = function ContractNF(_,vl,fdl) -> 
  hllc_fun_decls (vars_of_var_decls vl) (toks_of_fun_decls fdl) fdl

let hllc (c:contract) : clause list =
  c |> nf |> hllc_nf 
