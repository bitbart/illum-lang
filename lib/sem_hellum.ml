open Ast

(******************************************************************************)
(*                             Types for HeLLUM semantics                     *)
(******************************************************************************)

type address = string
type envval1 = VInt of int | VAddress of address | VString of string | VBool of bool
type envval = VBase of envval1 | VMap of (envval1 -> envval1)
type env = (ide * envval) list  (* association list *)

(* a transaction is a function name with a list of actual parameters *)
type transaction = {
  fid : ide;                            (* called function *)
  args: envval1 list;                   (* actual parameters *)
  signers: ide list;                    (* signers *)
  trans: (address * tok * int) list;    (* token transfers *)
  timestamp: int;                       (* timestamp *) 
}

(* FIXME: tx to increase block number*)

type wallet = (tok * int) list

type cstate = {                         (* contract state *)
  vars : env;                           (* variables *)
  init: bool;                           (* constructed? *)
  balance : wallet;                     (* wallet *)
}

type bcstate = {                        (* blockchain state *)
  cst : cstate;                         (* contract state *)
  ast : (address * wallet) list;        (* EOAs wallets *)
}

let bind_assoc f x v = 
  (x,v)::(List.filter (fun (x',_) -> x<>x') f)

(* updates a contract variable in blockchain state *)
let bind_bcstate (st:bcstate) x v = {
  cst = { st.cst with vars = bind_assoc st.cst.vars x v };
  ast = st.ast;
}

(* transfers v:t from contract balance to address a *)
let xfer_bcstate (st:bcstate) (v:int) (t:tok) (a:address) = 
  let bal_t = List.assoc t st.cst.balance in
  if bal_t < v then failwith "xfer_bcstate: insufficient contract balance"
  else 
  {
  cst = { st.cst with balance = bind_assoc st.cst.balance t (bal_t - v) };
  ast = let wa = List.assoc a st.ast in (* a's old wallet *) 
        let wa' = bind_assoc wa t (v + List.assoc t wa) in (* a's new wallet *) 
        bind_assoc st.ast a wa';
  }

(* binds formal parameters to actual parameters *)
let bind_fun_vars fpars apars =
  if List.length fpars = List.length apars then
    List.map2 (fun f a -> (f,VBase a)) fpars apars
  else failwith "bind_fun_vars: different number of formal and actual parameters"

let default_val_btype = function
  | TInt 
  | TUint -> VInt 0
  | TString -> VString ""
  | TBool -> VBool false
  | TAddr -> VAddress ""  (* FIXME: address string or int? *)

let default_val_hlltype = function
  | TBase ty -> VBase(default_val_btype ty)
  | TMap(_,ty2) -> VMap(fun _ -> default_val_btype ty2)

let bind_contr_vars vl = List.fold_left
  (fun r (x,ty) -> bind_assoc r x (default_val_hlltype ty)) [] vl

(* binds local variables to default values *)
let bind_loc_vars r al = List.fold_left
  (fun r' (x,ty) -> bind_assoc r' x (VBase (default_val_btype ty))) r al


(******************************************************************************)
(*                       Big-step semantics of expressions                    *)
(******************************************************************************)

exception TypeError of string


let rec sem_expr (st:cstate) env = function
    True -> VBase (VBool true)
  | False -> VBase (VBool false)
  | Var x -> List.assoc x st.vars
  | Map(e1,e2) -> (match sem_expr st env e1, sem_expr st env e2 with
      VMap m,VBase v -> VBase (m v)
    | _ -> failwith "sem_expr: Map")
  | IntConst n -> VBase (VInt n)
  | AddrConst a -> VBase (VInt a)
  | StringConst s -> VBase (VString s)
  | Not e -> (match sem_expr st env e with
      VBase (VBool b) -> VBase (VBool (not b))
    | _ -> raise (TypeError "Not"))
  | And(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase (VBool v1),VBase (VBool v2)) -> VBase(VBool(v1 && v2))
    | _ -> raise (TypeError "And"))
  | Or(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase (VBool v1),VBase (VBool v2)) -> VBase(VBool(v1 || v2))
    | _ -> raise (TypeError "Or"))
  | Add(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase (VInt v1),VBase (VInt v2)) -> VBase(VInt(v1 + v2))
    | _ -> raise (TypeError "Add"))
  | Sub(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase (VInt v1),VBase (VInt v2)) -> VBase(VInt(v1 - v2))
    | _ -> raise (TypeError "Sub"))
  | Mul(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase (VInt v1),VBase (VInt v2)) -> VBase(VInt(v1 * v2))
    | _ -> raise (TypeError "Mul"))
  | Div(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase (VInt v1),VBase (VInt v2)) -> VBase(VInt(v1 / v2))
    | _ -> raise (TypeError "Div"))
  | Eq(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase v1,VBase v2) -> VBase(VBool(v1 = v2))
    | _ -> raise (TypeError "Eq"))
  | Neq(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase v1,VBase v2) -> VBase(VBool(v1 <> v2))
    | _ -> raise (TypeError "Neq"))
  | Le(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase (VInt v1),VBase (VInt v2)) -> VBase(VBool(v1 < v2))
    | _ -> raise (TypeError "Le"))
  | Leq(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase (VInt v1),VBase (VInt v2)) -> VBase(VBool(v1 <= v2))
    | _ -> raise (TypeError "Leq"))
  | Ge(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase (VInt v1),VBase (VInt v2)) -> VBase(VBool(v1 > v2))
    | _ -> raise (TypeError "Ge"))
  | Geq(e1,e2) -> (match (sem_expr st env e1,sem_expr st env e2)  with
    | (VBase (VInt v1),VBase (VInt v2)) -> VBase(VBool(v1 >= v2))
    | _ -> raise (TypeError "Geq"))
  | IfE(e1,e2,e3) -> (match sem_expr st env e1 with
    | VBase (VBool true) -> sem_expr st env e2
    | VBase (VBool false) -> sem_expr st env e3
    | _ -> raise (TypeError "Not"))
  | Bal(t) -> VBase (VInt (List.assoc t st.balance))  
  | BalPre(_) -> failwith "sem_expr: BalPre cannot happen"       
  | MapUpd(_,_,_) -> failwith "sem_expr: mapupd not implemented"
  | VerSig(_,_) -> failwith "sem_expr: versig not implemented"
  | Expand(_,_) -> failwith "sem_expr: Expand cannot happen"


(******************************************************************************)
(*                              Semantics of commands                         *)
(******************************************************************************)

let rec sem_cmd1 bcst env = function
  | SkipNF -> bcst,env
  | VarAssignNF(x,e) -> 
      let v = sem_expr bcst.cst env e in 
      bind_bcstate bcst x v, env (* FIXME: x in parameter or local *)
  | XferNF(x,e,t) ->
      let cbal = bcst.cst.balance in
      (match sem_expr bcst.cst env e with 
      | VBase (VInt v) when List.assoc t cbal >= v -> 
          xfer_bcstate bcst v t x, env
      | VBase (VInt _) -> failwith "sem_cmd1: Xfer with insufficient contract balance" 
      | _ -> failwith "sem_cmd1: Xfer of non-int value") 
  | ReqNF(e) -> 
      (match sem_expr bcst.cst env e with 
      | VBase (VBool true) -> bcst,env
      | VBase (VBool false) -> failwith "sem_cmd1: Req with false condition"
      | _ -> failwith "sem_cmd1: Req of non-boolean value")
  | IfNF(bl) -> (match bl with 
    | [] -> failwith "IfNF with empty branches cannot happen"
    | (e,cl)::bl' -> 
        (match sem_expr bcst.cst env e with 
        | VBase (VBool true) -> sem_cmd bcst env cl
        | VBase (VBool false) -> sem_cmd1 bcst env (IfNF bl')
        | _ -> failwith "sem_cmd1: IfNF of non-boolean value"))
  | SimAssign(_) -> failwith "SimAssign cannot happen"

and sem_cmd bcst env = function
  | [] -> bcst,env
  | c::cl -> let bcst',env' = sem_cmd1 bcst env c in sem_cmd bcst' env' cl

(******************************************************************************)
(*                       HeLLUM semantics: functions                          *)
(******************************************************************************)  

let sem_fun bcst tx = function
  | ConstrNF(_,_,_,_,_) -> failwith "TODO" 
  | ProcNF(f,al,_,vdl,cl,_) when f = tx.fid ->
      (* bind formal parameters to actual parameters *) 
      let env1 = bind_fun_vars (List.map fst vdl) tx.args in
      (* bind local variables to default values *)
      let env2 = bind_loc_vars env1 al in
      fst (sem_cmd bcst env2 cl)
  | _ -> failwith "sem_fun: cannot happen" 

(******************************************************************************)
(*                       HeLLUM semantics: contracts                          *)
(******************************************************************************)  

let get_fun_name = function
  | ConstrNF(_,_,_,_,_) -> "constructor"
  | ProcNF(f,_,_,_,_,_) -> f

let sem_contr bcst tx = function
  ContractNF(_,_,fdl) -> 
    if (not bcst.cst.init && tx.fid <> "constructor")
    then failwith "Calling function before constructor"
    else if (bcst.cst.init && tx.fid = "constructor")
    then failwith "Constructor cannot be called twice"
    else match List.filter (fun f -> get_fun_name f = tx.fid) fdl with
    | [] -> bcst
    | [f] -> sem_fun bcst tx f
    | _ -> failwith "sem_contr: cannot happen (multiple functions with the same name)"

let init_cstate = function
  ContractNF(_,vl,_) -> {
    vars = bind_contr_vars vl;
    init = false;
    balance = [];
  }

(******************************************************************************)
(*                               HeLLUM semantics                             *)
(******************************************************************************)  

let trace1 bcst tx contr = 
  try sem_contr bcst tx contr 
  with _ -> bcst                (* invalid transactions are ignored *)

let trace n bcst txl = 
  List.fold_left (fun st tx -> trace1 st tx n) bcst txl