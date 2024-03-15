open Ast
open Utils
open Prettyprint

exception UnboundVar of ide

(******************************************************************************)
(*                          Type checking of NF0 contracts                    *)
(******************************************************************************)

let rec env_of_ldecls = function
    | [] -> fun x -> raise (UnboundVar x)
    | (t,x)::l -> bind (env_of_ldecls l) x (TBase t)

let rec env_of_var_decls = function
| [] -> fun x -> raise (UnboundVar x)
| (x,t)::l -> bind (env_of_var_decls l) x t
(*  VarDeclSeq(MapDecl(t1,t2,x),vl') -> bind (env_of_var_decls vl') x (TMap(t1,t2)) *) (* FIXME: remove? *)

let unbox = function
| TBase b -> b
| _ -> failwith "unbox: not a base type"

let bsubtype b1 b2 = match b1,b2 with
| (t1,t2) when t1=t2 -> true
| (TUint,TInt) -> true
| _ -> false
let subtype t1 t2 = match t1,t2 with
| (TBase b1,TBase b2) -> bsubtype b1 b2
| (TMap(b1,b2),TMap(b1',b2')) -> (bsubtype b2 b2') && (bsubtype b1' b1)
| _ -> false
let bjoin b1 b2 = match b1,b2 with
| (b1,b2) when b1=b2 -> b1
| (TUint,TInt) -> TInt
| (TInt,TUint) -> TInt
| _ -> failwith "bjoin: incompatible types"
let bmeet b1 b2 = match b1,b2 with
| (b1,b2) when b1=b2 -> b1
| (TUint,TInt) -> TUint
| (TInt,TUint) -> TUint
| _ -> failwith "bmeet: incompatible types"
let meet t1 t2 = match t1,t2 with
| (TBase b1,TBase b2) -> TBase (bmeet b1 b2)
| (TMap(b1,b2),TMap(b1',b2')) -> TMap(bjoin b1 b1',bmeet b2 b2')
| _ -> failwith "meet: incompatible types"

let expect_type e t_act t_exp =
  if subtype t_act t_exp then true
  else failwith (string_of_type_error(e,t_act,t_exp))

let expect_comparable e t1 t2 =
    if subtype t1 t2 || subtype t2 t1 then true
    else failwith (string_of_type_error(e,t1,t2)) (* FIXME: adapt error string *)
  
let rec typecheck_expr env = function
| True 
| False -> TBase TBool
| IntConst _ -> TBase TUint
| AddrConst _ -> TBase TAddr
| StringConst _ -> TBase TString
| Var x -> env x
| Map(e1,e2) -> (match typecheck_expr env e1 with
  | TMap(b1,b2) -> 
    let t2 = typecheck_expr env e2 in
    expect_type e2 t2 (TBase b1) 
    |> fun _ -> TBase b2
  | _ -> failwith ("Type error: " ^ string_of_expr e1 ^ " in " ^ string_of_expr (Map(e1,e2)) ^ " is not a mapping"))
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
    let t1 = typecheck_expr env e1 in
    let t2 = typecheck_expr env e2 in
    expect_type e1 t1 (TBase TInt)
    |> fun _ -> expect_type e2 t2 (TBase TInt)
    |> fun _ -> meet t1 t2
| Eq(e1,e2)
| Neq(e1,e2)
| Leq(e1,e2)
| Le(e1,e2) 
| Geq(e1,e2)
| Ge(e1,e2) -> 
    let t1 = typecheck_expr env e1 in 
    let t2 = typecheck_expr env e2 in
    expect_comparable e2 t1 t2
    |> fun _ -> TBase TBool
| Bal(_)
| BalPre(_) -> TBase TInt
| IfE(e1,e2,e3) -> 
  let t1 = typecheck_expr env e1 in 
  let t2 = typecheck_expr env e2 in
  let t3 = typecheck_expr env e3 in
  expect_type e1 t1 (TBase TBool)
  |> fun _ -> expect_comparable e2 t2 t3
  |> fun _ -> meet t2 t3
| MapUpd(e1,e2,e3) -> 
  let t1 = typecheck_expr env e1 in 
  let b2 = unbox (typecheck_expr env e2) in
  let b3 = unbox (typecheck_expr env e3) in
  expect_type e1 (TMap(b2,b3)) t1
  |> fun _ -> (TMap(b2,b3))
let typecheck_cmd1 (env:ide -> hlltype) = function
| SkipNF -> true
| VarAssignNF(x,e) -> expect_type e (typecheck_expr env e) (env x)
| XferNF(_,e,_) -> expect_type e (typecheck_expr env e) (TBase TInt)
| ReqNF e -> expect_type e (typecheck_expr env e) (TBase TBool)
| IfNF _ -> true (* FIXME *) 
| SimAssign al -> List.for_all (fun (x,e) -> expect_type e (typecheck_expr env e) (env x)) al

let fail_if_false b s c = if not b then failwith s else c

let typecheck_fun_gen f_univ env (f,a,_,vdl,cl,nl) = 
  let env1 = piecewise (env_of_ldecls a) env in 
  let env2 = piecewise (env_of_var_decls vdl) env1 in ()
  |> fail_if_false (subseteq nl f_univ) ("Next of " ^ f ^ " not in contract functions");
  List.for_all (typecheck_cmd1 env2) cl

let typecheck_fun f_univ env = function
  | ConstrNF(a,fml,vdl,cl,nl) -> ()
    |> fail_if_false (no_dup (List.map snd a)) "Duplicate arguments in constructor "
    |> fun _ -> typecheck_fun_gen f_univ env ("constructor",a,fml,vdl,cl,nl)
  | ProcNF(f,a,fml,vdl,cl,nl) -> () (* FIXME: local variables *)
    |> fail_if_false (no_dup (List.map snd a)) ("Duplicate arguments in function ")
    |> fun _ -> typecheck_fun_gen f_univ env (f,a,fml,vdl,cl,nl)

let typecheck_dup_inputs fdl = fdl 
  |> List.map (fun fd -> match fd with ConstrNF(_,fml,_,_,_) -> fml.inputs | ProcNF(_,_,fml,_,_,_) -> fml.inputs)
  |> List.fold_left (fun t_opt inl -> match find_dup (List.map snd inl) with None -> t_opt | Some x -> Some x) None
  |> fun t_opt -> match t_opt with None -> true | Some t -> failwith ("Multiply defined input of token " ^ t) 

let typecheck_dup_constr fdl = fdl 
  |> List.filter (fun fd -> match fd with ConstrNF(_,_,_,_,_) -> true | _ -> false)
  |> fun l -> if List.length l > 1 then failwith "Multiply defined constructor" else true 
  
let rec typecheck_dup_fun = function
| [] -> true
| f::l -> if List.mem f l then failwith ("Multiply defined function " ^ f) else typecheck_dup_fun l

let typecheck c = match c with ContractNF(_,vl,fdl) -> 
  let f_univ = List.fold_left (fun fl fd -> match fd with ProcNF(f,_,_,_,_,_) -> f::fl | _ -> fl) [] fdl in
  let env = env_of_var_decls vl in 
  c
  |> fun _ -> typecheck_dup_fun f_univ
  |> fun _ -> typecheck_dup_constr fdl
  |> fun _ -> typecheck_dup_inputs fdl
  |> fail_if_false (no_dup (List.map fst vl)) "Duplicate global variables" 
  |> fun _ -> List.for_all (typecheck_fun f_univ env) fdl
  |> fun _ ->  c

let ok_typecheck c = try typecheck c |> fun _ -> true with _ -> false
