open Ast
open Utils
open Prettyprint

exception UnboundVar of ide

(******************************************************************************)
(*                          Type checking of NF0 contracts                    *)
(******************************************************************************)

let rec env_of_ldecls = function
| [] -> fun x -> raise (UnboundVar x)
| (x,t)::l -> bind (env_of_ldecls l) x (TBase t)

let rec env_of_var_decls = function
| [] -> fun x -> raise (UnboundVar x)
| (x,t)::l -> bind (env_of_var_decls l) x t

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

let expect_type f e t_act t_exp =
  if subtype t_act t_exp then true
  else failwith (string_of_type_error(f,e,t_act,t_exp))

let expect_comparable f e t1 t2 =
    if subtype t1 t2 || subtype t2 t1 then true
    else failwith (string_of_type_error(f,e,t1,t2)) (* FIXME: adapt error string *)
  
let rec typecheck_expr f env = function
| True 
| False -> TBase TBool
| IntConst _ -> TBase TUint
| AddrConst _ -> TBase TAddr
| StringConst _ -> TBase TString
| Var x -> env x
| Map(e1,e2) -> (match typecheck_expr f env e1 with
  | TMap(b1,b2) -> 
    let t2 = typecheck_expr f env e2 in
    expect_type f e2 t2 (TBase b1) 
    |> fun _ -> TBase b2
  | _ -> failwith ("Type error in" ^ f ^ ": " ^ string_of_expr e1 ^ " in " ^ string_of_expr (Map(e1,e2)) ^ " is not a mapping"))
| Not e' -> 
    expect_type f e' (typecheck_expr f env e') (TBase TBool) 
    |> fun _ -> TBase TBool
| Hash e' -> 
      expect_type f e' (typecheck_expr f env e') (TBase TString) 
      |> fun _ -> TBase TString
| And(e1,e2)
| Or(e1,e2) -> 
    expect_type f e1 (typecheck_expr f env e1) (TBase TBool) 
    |> fun _ -> expect_type f e2 (typecheck_expr f env e2) (TBase TBool)
    |> fun _ -> TBase TBool 
| Add(e1,e2)
| Sub(e1,e2)
| Mul(e1,e2)
| Div(e1,e2) -> 
    let t1 = typecheck_expr f env e1 in
    let t2 = typecheck_expr f env e2 in
    expect_type f e1 t1 (TBase TInt)
    |> fun _ -> expect_type f e2 t2 (TBase TInt)
    |> fun _ -> meet t1 t2
| Mod(e1,e2) -> 
    let t1 = typecheck_expr f env e1 in
    let t2 = typecheck_expr f env e2 in
    expect_type f e1 t1 (TBase TInt)
    |> fun _ -> expect_type f e2 t2 (TBase TInt)
    |> fun _ -> meet t1 t2
| Eq(e1,e2)
| Neq(e1,e2)
| Leq(e1,e2)
| Le(e1,e2) 
| Geq(e1,e2)
| Ge(e1,e2) -> 
    let t1 = typecheck_expr f env e1 in 
    let t2 = typecheck_expr f env e2 in
    expect_comparable f e2 t1 t2
    |> fun _ -> TBase TBool
| Bal(_)
| BalPre(_) -> TBase TInt
| IfE(e1,e2,e3) -> 
  let t1 = typecheck_expr f env e1 in 
  let t2 = typecheck_expr f env e2 in
  let t3 = typecheck_expr f env e3 in
  expect_type f e1 t1 (TBase TBool)
  |> fun _ -> expect_comparable f e2 t2 t3
  |> fun _ -> meet t2 t3
| MapUpd(e1,e2,e3) -> 
  let t1 = typecheck_expr f env e1 in 
  let b2 = unbox (typecheck_expr f env e2) in
  let b3 = unbox (typecheck_expr f env e3) in
  expect_type f e1 (TMap(b2,b3)) t1
  |> fun _ -> (TMap(b2,b3))
| VerSig(e1,e2) -> 
  let t1 = typecheck_expr f env e1 in 
  let _ = typecheck_expr f env e2 in (* FIXME? *)
  expect_type f e1 t1 (TBase TAddr) 
  |> fun _ -> (TBase TBool)
| Expand(_,_) -> failwith "typecheck: something went wrong with view expansion: perhaps undefined or mutually recursive views?"
| StrLen e1 -> 
  let t1 = typecheck_expr f env e1 in
  expect_type f e1 t1 (TBase TString)
  |> fun _ -> TBase TInt
| SubStr(e1,e2,e3) -> 
  let t1 = typecheck_expr f env e1 in 
  let t2 = typecheck_expr f env e2 in
  let t3 = typecheck_expr f env e3 in
  expect_type f e1 t1 (TBase TString)
  |> fun _ -> expect_type f e2 t2 (TBase TInt)
  |> fun _ -> expect_type f e3 t3 (TBase TInt)
  |> fun _ -> TBase TString
| IntOfString e' ->
      expect_type f e' (typecheck_expr f env e') (TBase TString) 
    |> fun _ -> TBase TInt

let typecheck_cmd1 f (env:ide -> hlltype) = function
| SkipNF -> true
| VarAssignNF(x,e) -> expect_type f e (typecheck_expr f env e) (env x)
| XferNF(_,e,_) -> expect_type f e (typecheck_expr f env e) (TBase TInt)
| ReqNF e -> expect_type f e (typecheck_expr f env e) (TBase TBool)
| IfNF _ -> true (* FIXME *) 
| SimAssign al -> List.for_all (fun (x,e) -> expect_type f e (typecheck_expr f env e) (env x)) al

let fail_if_false b s c = if not b then failwith s else c

let typecheck_fun_gen f_univ env (f,al,_,vdl,cl,nl) = 
  let env1 = piecewise (env_of_ldecls al) env in 
  let env2 = piecewise (env_of_var_decls vdl) env1 in ()
  |> fail_if_false (subseteq nl f_univ) ("Next of " ^ f ^ " not in contract functions");
  List.for_all (typecheck_cmd1 f env2) cl

let typecheck_fun f_univ env = function
  | ConstrNF(a,fml,vdl,cl,nl) -> ()
    |> fail_if_false (no_dup (List.map fst a)) "Duplicate arguments in constructor "
    |> fun _ -> typecheck_fun_gen f_univ env ("constructor",a,fml,vdl,cl,nl)
  | ProcNF(f,a,fml,vdl,cl,nl) -> () (* FIXME: local variables *)
    |> fail_if_false (no_dup (List.map fst a)) ("Duplicate arguments in function ")
    |> fun _ -> typecheck_fun_gen f_univ env (f,a,fml,vdl,cl,nl)

let typecheck_dup_inputs fdl = fdl 
  |> List.map (fun fd -> match fd with ConstrNF(_,fml,_,_,_) -> fml.inputs | ProcNF(_,_,fml,_,_,_) -> fml.inputs)
  |> List.fold_left (fun t_opt inl -> match find_dup (List.map snd inl) with None -> t_opt | Some x -> Some x) None
  |> fun t_opt -> match t_opt with None -> true | Some t -> failwith ("Multiply defined input of token " ^ t) 

let typecheck_auths_nodup f authl = match find_dup authl with 
| None -> true 
| Some x -> failwith ("Duplicate authorization of " ^ x ^ " in " ^ f)

(* checks that each variable in auth(...) is defined globally or locally *)
let typecheck_auths_isdef f vl lvl authl =
  let lvars = List.map fst lvl in
  let gvars = List.map fst vl in 
  match find_notin authl (gvars @ lvars) with 
| None -> true 
| Some x ->  failwith ("Authorization of undefined variable " ^ x ^ " in " ^ f)

let typecheck_auths_welltyped f vl lvl authl =
  let env1 = env_of_var_decls vl in
  let env2 = piecewise (env_of_ldecls lvl) env1 in 
  List.for_all (fun x -> let tx = typecheck_expr f env2 (Var x) in expect_type f (Var x) tx (TBase TAddr)) authl

let typecheck_auths vl fdl = fdl 
  |> (* for each function declaration, construct (auth list, local var names list) *)
    List.map (fun fd -> match fd with 
    | ConstrNF(al,fml,_,_,_) -> ("constructor", fml.auths, al) 
    | ProcNF(f,al,fml,_,_,_) -> (f, fml.auths, al))
  |> List.for_all (fun (f,authl,al) -> 
    typecheck_auths_nodup f authl
    &&
    typecheck_auths_isdef f vl al authl
    &&
    typecheck_auths_welltyped f vl al authl
  ) 

let typecheck_vars vl fdl = let vl' = List.map fst vl in
  fdl 
  |> (* for each function declaration, construct (auth list, local var names list) *)
    List.map (fun fd -> match fd with 
    | ConstrNF(al,_,lvl,_,_) -> ("constructor", List.map fst al, List.map fst lvl) 
    | ProcNF(f,al,_,lvl,_,_) -> (f, List.map fst al, List.map fst lvl))
  |> List.for_all (fun (f,al,lvl) -> 
    (match find_dup (al@lvl) with None -> true | Some x -> failwith ("Formal parameter " ^ x ^ " in " ^ f ^ " overlaps with local variable"))
    &&
    (match find_dup (al@vl') with None -> true | Some x -> failwith ("Formal parameter " ^ x ^ " in " ^ f ^ " overlaps with global variable"))
    &&
    (match find_dup (lvl@vl') with None -> true | Some x -> failwith ("Local variable " ^ x ^ " in " ^ f ^ " overlaps with global variable"))
    )

(* check that there exists at most one constructor *)
let typecheck_dup_constr fdl = fdl 
  |> List.filter (fun fd -> match fd with ConstrNF(_,_,_,_,_) -> true | _ -> false)
  |> fun l -> if List.length l > 1 then failwith "Multiply defined constructor" else true 
  
(* checke that there are no duplicate function names *)
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
  |> fun _ -> typecheck_auths vl fdl
  |> fun _ -> typecheck_vars vl fdl
  |> fail_if_false (no_dup (List.map fst vl)) "Duplicate global variables" 
  |> fun _ -> List.for_all (typecheck_fun f_univ env) fdl
  |> fun _ ->  c

let ok_typecheck c = try typecheck c |> fun _ -> true with _ -> false