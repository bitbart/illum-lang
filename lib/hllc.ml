open Ast
open Utils
open Nf

(******************************************************************************)
(*                                 HeLLUM -> ILLUM compiler                   *)
(******************************************************************************)

let clause_names (f:string) = f, "Post-" ^ f

let hllc_cmd = function
| [] -> []
| [IfNF _] -> failwith "NOPE"
| [ReqNF _; IfNF _] -> failwith "NOPE"
| (ReqNF _)::_ -> failwith "NOPE"
(* | [ XferNF(a,e,t); SimAssign(yl) ] -> failwith "NOPE" *)
| _ -> [Call []]

let hllc_body f al fml xl tl cl = 
  let b = match cl with 
  | ReqNF e::_ -> e
  | _ -> True
in
{
  name = fst (clause_names f);
  spar = xl @ tl;
  dpar = al;
  walp = fml.inputs; (* (expr * tok) list; (e:T, ...) = wallet in the funding precondition *)
  prep = b;
  cntr = let d = { auth = []; afterAbs = []; afterRel = [] } in 
  hllc_cmd cl 
  |> List.map (fun c -> (d,c)) 
}

let hllc_fun xl tl = function
  | ConstrNF(_ (* a *),_ (* fml *),_ (* c *),_ (* nl *)) -> [] (* ConstrNF(a,fml,nf2_cmd xl (toks_of_cmd c) c,nl) *)
  | ProcNF(f,al,fml,cl,_ (* nl *)) -> [ hllc_body f (List.map snd al) fml xl tl cl ]

let rec hllc_fun_decls xl tl = function
| EmptyFunDeclsNF -> []  
| FunDeclSeqNF(f,fl) -> (hllc_fun xl tl f) @ (hllc_fun_decls xl tl fl)

let hllc_nf = function ContractNF(_,vl,fdl) -> 
  hllc_fun_decls (vars_of_var_decls vl) (toks_of_fun_decls fdl) fdl

let hllc (c:contract) : clause list =
  c |> nf |> hllc_nf 
