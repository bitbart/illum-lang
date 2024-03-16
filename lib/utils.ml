open Ast

(******************************************************************************)
(*                                     Parser driver                          *)
(******************************************************************************)

let parse (s : string) : contract =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.contract Lexer.read_token lexbuf in
  ast

(******************************************************************************)
(*                                     List functions                         *)
(******************************************************************************)

let union l1 l2 = List.fold_left (fun l x -> if List.mem x l then l else x::l) l1 l2

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let is_disjoint l1 l2 = List.fold_left (fun b x -> b && not (List.mem x l2)) true l1

let subseteq l1 l2 = List.fold_left (fun b x -> b && List.mem x l2) true l1

let rec no_dup = function 
| [] -> true
| x::l -> if List.mem x l then false else no_dup l

let rec find_dup = function
| [] -> None
| x::l -> if List.mem x l then Some x else find_dup l

let rec find_notin l1 l2 = match l1 with
| [] -> None
| x::l -> if List.mem x l2 then find_notin l l2 else Some x

let rec last = function
    [] -> failwith "last on empty list"
  | [x] -> x
  | _::l -> last l

let bind f x v = fun y -> if y=x then v else f y

let piecewise f g = fun x -> try f x with _ -> g x

(******************************************************************************)
(*                                Variables in a contract                     *)
(******************************************************************************)

let rec depth_expr = function
    True
  | False
  | IntConst _
  | AddrConst _ 
  | StringConst _                
  | Var _ -> 0
  | Not e -> 1 + depth_expr e
  | Map(e1,e2)
  | And(e1,e2) 
  | Or (e1,e2) 
  | Add(e1,e2)
  | Sub(e1,e2)
  | Mul(e1,e2)
  | Div(e1,e2)      
  | Eq (e1,e2) 
  | Neq(e1,e2)
  | Leq(e1,e2) 
  | Le (e1,e2)
  | Geq(e1,e2) 
  | Ge (e1,e2) 
  | VerSig(e1,e2) -> 1 + max (depth_expr e1) (depth_expr e2)
  | Bal(_) 
  | BalPre(_) -> 0
  | IfE(e1,e2,e3)  
  | MapUpd(e1,e2,e3) -> 1 + max (depth_expr e1) (max (depth_expr e2) (depth_expr e3))

(******************************************************************************)
(*                                Variables in a contract                     *)
(******************************************************************************)

let rec vars_of_expr = function
    True
  | False
  | IntConst _ -> []
  | AddrConst _ -> []
  | StringConst _ -> []               
  | Var x -> [x]
  | Map(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)
  | Not e -> vars_of_expr e
  | And(e1,e2) 
  | Or(e1,e2) 
  | Add(e1,e2)
  | Sub(e1,e2)
  | Mul(e1,e2)
  | Div(e1,e2)      
  | Eq(e1,e2) 
  | Neq(e1,e2)
  | Leq(e1,e2) 
  | Le(e1,e2)
  | Geq(e1,e2) 
  | Ge(e1,e2) 
  | VerSig(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)
  | Bal(_) 
  | BalPre(_) -> []
  | IfE(e1,e2,e3) -> union (vars_of_expr e1) (union (vars_of_expr e2) (vars_of_expr e3))  
  | MapUpd(e1,e2,e3) -> union (vars_of_expr e1) (union (vars_of_expr e2) (vars_of_expr e3))

and vars_of_cmd1 = function
    SkipNF -> []
  | VarAssignNF(x,e) -> union [x] (vars_of_expr e)
  | IfNF bl -> List.fold_left (fun tl (e,cl) -> union tl (union (vars_of_expr e) (vars_of_cmd cl))) [] bl
  | XferNF(x,e,_) -> union [x] (vars_of_expr e)
  | ReqNF e -> vars_of_expr e                    
  | SimAssign _ -> failwith "vars_of_cmd1: SimAssign"
and vars_of_cmd cl = List.fold_left (fun tl c -> union tl (vars_of_cmd1 c)) [] cl
  
(******************************************************************************)
(*                        Variables in the contract state                     *)
(******************************************************************************)

let vars_of_var_decl = function
| VarDecl (_,x) -> x
| MapDecl(_,_,x) -> x

let rec vars_of_var_decls = function
| EmptyVarDecls -> []
| VarDeclSeq(vd,vdl) -> (vars_of_var_decl vd)::(vars_of_var_decls vdl)

(******************************************************************************)
(*                              Tokens in the contract                        *)
(******************************************************************************)

let rec toks_of_expr = function
| True 
| False 
| Var _ 
| IntConst _ 
| AddrConst _
| StringConst _ -> []
| Map(_,e)
| Not e -> toks_of_expr e
| And(e1,e2) 
| Or(e1,e2)  
| Add(e1,e2) 
| Sub(e1,e2) 
| Mul(e1,e2) 
| Div(e1,e2) 
| Eq(e1,e2)  
| Neq(e1,e2) 
| Leq(e1,e2) 
| Le(e1,e2)   
| Geq(e1,e2) 
| Ge(e1,e2) 
| VerSig(e1,e2) -> union (toks_of_expr e1) (toks_of_expr e2)
| Bal(t)
| BalPre(t) -> [t]
| IfE(e1,e2,e3) -> union (toks_of_expr e1) (union (toks_of_expr e2) (toks_of_expr e3))
| MapUpd(e1,e2,e3) -> union (toks_of_expr e1) (union (toks_of_expr e2) (toks_of_expr e3))

let rec toks_of_cmd1 = function
| SkipNF -> []
| VarAssignNF(_,e) -> toks_of_expr e 
| XferNF(_,e,tok) -> union (toks_of_expr e) [tok] 
| ReqNF e -> toks_of_expr e
| IfNF bl -> List.fold_left (fun tl (e,cl) -> union tl (union (toks_of_expr e) (toks_of_cmd cl))) [] bl
| SimAssign al -> al |> List.map snd  |> List.map toks_of_expr |> List.fold_left union [] 
and toks_of_cmd cl = List.fold_left (fun tl c -> union tl (toks_of_cmd1 c)) [] cl

let toks_of_fun = function
  | ConstrNF(_,fml,_,c,_) 
  | ProcNF(_,_,fml,_,c,_) -> union (List.fold_left (fun tl (_,t) -> union tl [t]) [] fml.inputs) (toks_of_cmd c)

let toks_of_fun_decls = List.fold_left (fun tl f -> union tl (toks_of_fun f)) []

let toks_of_contract = function ContractNF(_,_,fdl) -> List.sort compare (toks_of_fun_decls fdl)

(******************************************************************************)
(*                      Substitute variable in expression                     *)
(******************************************************************************)

(* subst_var x e e' replaces all the occurrences of base variable x in e' with e *)
let rec subst_var x e = function
| True -> True
| False -> False
| Var y when y=x -> e
| Var y -> Var y
| Map(e1,e2) -> Map(subst_var x e e1,subst_var x e e2)
| IntConst n -> IntConst n
| AddrConst n -> AddrConst n
| StringConst s -> StringConst s
| Not e1 -> Not (subst_var x e e1)
| And(e1,e2) -> And(subst_var x e e1,subst_var x e e2)
| Or(e1,e2) -> Or(subst_var x e e1,subst_var x e e2)
| Add(e1,e2) -> Add(subst_var x e e1,subst_var x e e2)
| Sub(e1,e2) -> Sub(subst_var x e e1,subst_var x e e2)
| Mul(e1,e2) -> Mul(subst_var x e e1,subst_var x e e2)
| Div(e1,e2) -> Div(subst_var x e e1,subst_var x e e2)
| Eq(e1,e2) -> Eq(subst_var x e e1,subst_var x e e2)
| Neq(e1,e2) -> Neq(subst_var x e e1,subst_var x e e2)
| Leq(e1,e2) -> Leq(subst_var x e e1,subst_var x e e2)
| Le(e1,e2) -> Le(subst_var x e e1,subst_var x e e2) 
| Geq(e1,e2) -> Geq(subst_var x e e1,subst_var x e e2)
| Ge(e1,e2) -> Ge(subst_var x e e1,subst_var x e e2)
| VerSig(e1,e2) -> VerSig(subst_var x e e1,subst_var x e e2)
| Bal(t) -> Bal(t)
| BalPre(t) -> BalPre(t)
| IfE(e1,e2,e3) -> IfE(subst_var x e e1,subst_var x e e2,subst_var x e e3)
| MapUpd(e1,e2,e3) -> MapUpd(subst_var x e e1,subst_var x e e2,subst_var x e e3)

(******************************************************************************)
(*                      Substitute balance in expression                      *)
(******************************************************************************)

(* subst_bal t e e' replaces all the occurrences of Bal(t) in e' with e *)
let rec subst_bal t (e:expr) = function
| Bal(t') when t'=t -> e
| Bal(t')    -> Bal(t')                  
| Map(y,e')  -> Map(y,subst_bal t e e')
| Not e1     -> Not(subst_bal t e e1)
| And(e1,e2) -> And(subst_bal t e e1,subst_bal t e e2)
| Or(e1,e2)  -> Or (subst_bal t e e1,subst_bal t e e2)
| Add(e1,e2) -> Add(subst_bal t e e1,subst_bal t e e2)
| Sub(e1,e2) -> Sub(subst_bal t e e1,subst_bal t e e2)
| Mul(e1,e2) -> Mul(subst_bal t e e1,subst_bal t e e2)
| Div(e1,e2) -> Div(subst_bal t e e1,subst_bal t e e2)
| Eq(e1,e2)  -> Eq (subst_bal t e e1,subst_bal t e e2)
| Neq(e1,e2) -> Neq(subst_bal t e e1,subst_bal t e e2)
| Leq(e1,e2) -> Leq(subst_bal t e e1,subst_bal t e e2)
| Le(e1,e2)  -> Le (subst_bal t e e1,subst_bal t e e2) 
| Geq(e1,e2) -> Geq(subst_bal t e e1,subst_bal t e e2)
| Ge(e1,e2)  -> Ge (subst_bal t e e1,subst_bal t e e2)
| VerSig(e1,e2) -> VerSig(subst_bal t e e1,subst_bal t e e2)
| e -> e


(******************************************************************************)
(*              Substitute expression wrt simultaneous assignment             *)
(******************************************************************************)

let rec fun_of_list= function
| [] -> fun x -> failwith ("fun_of_list: undefined " ^ x)
| (a,b)::l-> let f' = fun_of_list l in fun x -> if x=a then b else f' x  

(* simsubst af e applies the simultaneous assignment af to the expression e. *)
(* If some variable in e is not assigned by af, then it is preserved *)

let rec simsubst (af:ide -> expr) (e:expr) : expr = match e with
| True -> True
| False -> False
| Var x -> (try af x with _ -> Var x)
| Map(e1,e2) -> Map(simsubst af e1,simsubst af e2)
| IntConst n -> IntConst n
| AddrConst n -> AddrConst n
| StringConst s -> StringConst s
| Not e1 -> Not (simsubst af e1)
| And(e1,e2) -> And(simsubst af e1,simsubst af e2)
| Or(e1,e2) ->  Or (simsubst af e1,simsubst af e2)
| Add(e1,e2) -> Add(simsubst af e1,simsubst af e2)
| Sub(e1,e2) -> Sub(simsubst af e1,simsubst af e2)
| Mul(e1,e2) -> Mul(simsubst af e1,simsubst af e2)
| Div(e1,e2) -> Div(simsubst af e1,simsubst af e2)
| Eq(e1,e2) ->  Eq (simsubst af e1,simsubst af e2)
| Neq(e1,e2) -> Neq(simsubst af e1,simsubst af e2)
| Leq(e1,e2) -> Leq(simsubst af e1,simsubst af e2)
| Le(e1,e2) ->  Le (simsubst af e1,simsubst af e2) 
| Geq(e1,e2) -> Geq(simsubst af e1,simsubst af e2)
| Ge(e1,e2) ->  Ge (simsubst af e1,simsubst af e2)
| VerSig(e1,e2) -> VerSig(simsubst af e1,simsubst af e2)
| Bal(t) -> Bal(t)
| BalPre(t) -> BalPre(t)
| IfE(e1,e2,e3)    -> IfE   (simsubst af e1,simsubst af e2,simsubst af e3)
| MapUpd(e1,e2,e3) -> MapUpd(simsubst af e1,simsubst af e2,simsubst af e3)

(******************************************************************************)
(*                          Get functions in a contract                       *)
(******************************************************************************)

let get_fun _ _ = []
(* List.map (fun n -> get_fun n contr) nl *)