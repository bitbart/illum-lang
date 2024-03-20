open Ast

type envval1 = VInt of int | VString of string | VBool of bool
type envval = VBase of envval1 | VMap of (envval1 -> envval1)
type env = ide -> envval

let topenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv (el,_,_) = el



(*
type conf = St of state | Cmd of cmd * state


let apply st x = match topenv st x with
    IVar l -> getmem st l
  | _ -> failwith "apply error"

exception PredOfZero
exception NoRuleApplies

let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")
    
let bind f x v = fun y -> if y=x then v else f y

let is_val = function
    True -> true
  | False -> true
  | IntConst _ -> true
  | StringConst _ -> true
  | AddrConst _ -> true
  | _ -> false
*)

(******************************************************************************)
(*                       Big-step semantics of expressions                    *)
(******************************************************************************)


let eval_expr _ = function
    True -> VBool true
  | False -> VBool false
  | _ -> failwith "TODO"

(*
  | Var x -> Int (apply st x)
  | IntConst n -> Int n
  | AddrConst n -> Int n   (* check! *)
  | StringConst s -> String s
  | Not(e) -> (match eval_expr st e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not")
    )
  | And(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 && b2)
      | _ -> raise (TypeError "And")
    )
  | Or(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 || b2)
      | _ -> raise (TypeError "Or")
    )
  | Add(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Int(n1 + n2)
      | _ -> raise (TypeError "Add")
    )    
  | Sub(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) when n1>=n2 -> Int(n1 - n2)
      | _ -> raise (TypeError "Sub")
    )
  | Mul(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Int(n1 * n2)
      | _ -> raise (TypeError "Add")
    )        
  | Eq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 = n2)
      | _ -> raise (TypeError "Eq")
    )
  | Neq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 <> n2)
      | _ -> raise (TypeError "Neq")
    )    
  | Leq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 <= n2)
      | _ -> raise (TypeError "Leq")
    )          
  | Le(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 < n2)
      | _ -> raise (TypeError "Le")
    )          
  | Geq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 >= n2)
      | _ -> raise (TypeError "Geq")
    )          
  | Ge(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 > n2)
      | _ -> raise (TypeError "Ge")
    )
  | Bal(_) -> failwith ("TODO")          
*)

let sem_cmd cenv _ = function
  | SkipNF -> cenv
  | VarAssignNF(_,_) -> failwith "TODO"
  | _ -> failwith "TODO"
    (* bind cenv (eval_expr cenv fenv e) x *)

(*
        | Seq(c1,c2) -> (match trace1_cmd (Cmd(c1,st)) with
          St st1 -> Cmd(c2,st1)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))
    | If(e,c1,c2) -> (match eval_expr st e with
          Bool true -> Cmd(c1,st)
        | Bool false -> Cmd(c2,st)
        | _ -> failwith("if: type error"))
    | Req(_) -> failwith ("TODO")
    | Send(_,_,_) -> failwith ("TODO")
*)

(*
let rec sem_decl (e,l) = function
  | EmptyDecl -> (e,l)
  | IntVar(x) ->  let e' = bind e x (IVar l) in (e',l+1)
  | StringVar(x) ->  let e' = bind e x (IVar l) in (e',l+1)   (* TODO *)                                             
  | Constr(f,a,c) -> let e' = bind e f (IProc(a,c)) in (e',l)                                                
  | Proc(f,a,c) -> let e' = bind e f (IProc(a,c)) in (e',l)
  | DSeq(d1,d2) -> let (e',l') = sem_decl (e,l) d1 in sem_decl (e',l') d2

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]
*)

(**********************************************************************
 trace : int -> contract -> conf list

 Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)

(* 
let trace n (Contract(d)) =
  let (e,l) = sem_decl (botenv,0) d
  in trace_rec n (Cmd(c,([e],botmem,l)))
*)
