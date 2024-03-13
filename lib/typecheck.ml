open Ast
open Utils
open Prettyprint

exception UnboundVar of ide
type hlltype = TBase of btype | TMap of btype * btype

let string_of_hlltype = function
  | TBase t -> string_of_btype t
  | TMap(t1,t2) -> "mapping(" ^ string_of_btype t1 ^ " => " ^ string_of_btype t2 ^ ")"

let string_of_type_error(e,t_act,t_exp) =  
  "Type error: " ^ string_of_expr e ^ 
  " has type " ^ string_of_hlltype t_act ^ 
  " but an expression was expected of type " ^ string_of_hlltype t_exp 

(******************************************************************************)
(*                          Type checking of NF0 contracts                    *)
(******************************************************************************)

let rec env_of_ldecls = function
    | [] -> fun x -> raise (UnboundVar x)
    | (t,x)::l -> bind (env_of_ldecls l) x (TBase t)

let rec env_of_gdecls = function
| EmptyVarDecls -> fun x -> raise (UnboundVar x)
| VarDeclSeq(VarDecl(t,x),vl') -> bind (env_of_gdecls vl') x (TBase t)
| VarDeclSeq(MapDecl(t1,t2,x),vl') -> bind (env_of_gdecls vl') x (TMap(t1,t2))

let expect_type e t_act t_exp =
  if t_exp = t_act then true
  else failwith (string_of_type_error(e,t_act,t_exp))

  let rec typecheck_expr env = function
| True 
| False -> TBase TBool
| IntConst _
| AddrConst _ -> TBase TInt
| StringConst _ -> TBase TString
| Var x -> env x
| Map(_,_) -> failwith "typecheck_expr: Map not implemented" (* FIXME *)
| Not e' -> 
    expect_type e' (typecheck_expr env e') (TBase TBool) 
    |> fun _ -> TBase TBool 
| And(e1,e2)
| Or(e1,e2) -> 
    expect_type e1 (typecheck_expr env e1) (TBase TBool) 
    |> fun _ -> expect_type e2 (typecheck_expr env e2) (TBase TBool)
    |> fun _ -> TBase TBool 
| Add(e1,e2)
| Sub(e1,e2)
| Mul(e1,e2)
| Div(e1,e2) -> 
    expect_type e1 (typecheck_expr env e1) (TBase TInt)
    |> fun _ -> expect_type e2 (typecheck_expr env e2) (TBase TInt)
    |> fun _ -> (TBase TInt)
| Eq(e1,e2)
| Neq(e1,e2)
| Leq(e1,e2)
| Le(e1,e2) 
| Geq(e1,e2)
| Ge(e1,e2) -> 
    let t1 = typecheck_expr env e1 in 
    let t2 = typecheck_expr env e2 in
    expect_type e2 t1 t2
    |> fun _ -> TBase TBool
| Bal(_)
| BalPre(_) -> TBase TInt
| IfE(e1,e2,e3) -> 
  let t1 = typecheck_expr env e1 in 
  let t2 = typecheck_expr env e2 in
  let t3 = typecheck_expr env e3 in
  expect_type e1 t1 (TBase TBool)
  |> fun _ -> expect_type e2 t2 t3
  |> fun _ -> t2
| MapUpd(_,_,_) -> failwith "FIXME"

let typecheck_cmd1 (env:ide -> hlltype) = function
| SkipNF -> true
| VarAssignNF(x,e) -> expect_type e (typecheck_expr env e) (env x)
| XferNF(_,e,_) -> expect_type e (typecheck_expr env e) (TBase TInt)
| ReqNF e -> expect_type e (typecheck_expr env e) (TBase TBool)
| IfNF _ -> true (* FIXME *) 
| SimAssign al -> List.for_all (fun (x,e) -> expect_type e (typecheck_expr env e) (env x)) al

let fail_if_false b s c = if not b then failwith s else c

let typecheck_fun_gen f_univ env (f,a,_,cl,nl) = 
  let env' = piecewise (env_of_ldecls a) env in ()
  |> fail_if_false (subseteq nl f_univ) ("Next of " ^ f ^ " not in contract functions");
  List.for_all (typecheck_cmd1 env') cl

let typecheck_fun f_univ env = function
  | ConstrNF(a,fml,cl,nl) -> ()
    |> fail_if_false (no_dup (List.map snd a)) "Duplicate arguments in constructor "
    |> fun _ -> typecheck_fun_gen f_univ env ("constructor",a,fml,cl,nl)
  | ProcNF(f,a,fml,cl,nl) -> ()
    |> fail_if_false (no_dup (List.map snd a)) ("Duplicate arguments in function ")
    |> fun _ -> typecheck_fun_gen f_univ env (f,a,fml,cl,nl)

let typecheck c = match c with ContractNF(_,vl,fdl) -> 
  let f_univ = List.fold_left (fun fl fd -> match fd with ProcNF(f,_,_,_,_) -> f::fl | _ -> fl) [] fdl in
  let env = env_of_gdecls vl in 
  c
  |> fail_if_false (no_dup (vars_of_var_decls vl)) "Duplicate global variables" 
  |> fun _ -> List.for_all (typecheck_fun f_univ env) fdl
  |> fun _ ->  c

let ok_typecheck c = try typecheck c |> fun _ -> true with _ -> false
