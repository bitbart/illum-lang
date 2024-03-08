open Ast
open Utils
open Nf

(******************************************************************************)
(*                                 HeLLUM -> ILLUM compiler                   *)
(******************************************************************************)


let hllc_cmd xl tl = function
| [IfNF bl] -> failwith "NOPE"
| [ReqNF er; IfNF bl] -> failwith "NOPE"
| (ReqNF er)::cl -> failwith "NOPE"
| [ SendNF(a,e,t); SimAssign(yl) ] -> failwith "NOPE"
| cl -> failwith "NOPE"

let hllc_fun xl tl = function
  | ConstrNF(a,fml,c,nl) -> [] (* ConstrNF(a,fml,nf2_cmd xl (toks_of_cmd c) c,nl) *)
  | ProcNF(f,a,fml,c,nl) -> hllc_cmd xl tl c

let rec hllc_fun_decls xl tl = function
| EmptyFunDeclsNF -> []  
| FunDeclSeqNF(f,fl) -> (hllc_fun xl tl f) @ (hllc_fun_decls xl tl fl)

let hllc_nf = function ContractNF(x,vl,fdl) -> 
  hllc_fun_decls (vars_of_var_decls vl) (toks_of_fun_decls fdl) fdl

let hllc (c:contract) : clause list =
  c |> nf0 |> nf1 |> nf2 |> hllc_nf 
