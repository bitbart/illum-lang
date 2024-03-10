open Ast

(******************************************************************************)
(*                                     Parser driver                          *)
(******************************************************************************)

let parse (s : string) : contract =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.contract Lexer.read_token lexbuf in
  ast

let union l1 l2 = List.fold_left (fun l x -> if List.mem x l then l else x::l) l1 l2

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let is_disjoint l1 l2 = List.fold_left (fun b x -> b && not (List.mem x l2)) true l1

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
  | Map(x,e) -> union [x] (vars_of_expr e)
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
  | Ge(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)
  | Bal(_) 
  | BalPre(_) -> []
  | IfE(e1,e2,e3) -> union (vars_of_expr e1) (union (vars_of_expr e2) (vars_of_expr e3))  

and vars_of_cmd1 = function
    SkipNF -> []
  | VarAssignNF(x,e) -> union [x] (vars_of_expr e)
  | MapAssignNF(x,e1,e2) -> union [x] (union (vars_of_expr e1) (vars_of_expr e2))
  | IfNF bl -> List.fold_left (fun tl (e,cl) -> union tl (union (vars_of_expr e) (vars_of_cmd cl))) [] bl
  | SendNF(x,e,_) -> union [x] (vars_of_expr e)
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
| Ge(e1,e2) -> union (toks_of_expr e1) (toks_of_expr e2)
| Bal(t)
| BalPre(t) -> [t]
| IfE(e1,e2,e3) -> union (toks_of_expr e1) (union (toks_of_expr e2) (toks_of_expr e3))

let rec toks_of_cmd1 = function
| SkipNF -> []
| VarAssignNF(_,e) -> toks_of_expr e 
| MapAssignNF(_,e1,e2) -> union (toks_of_expr e1) (toks_of_expr e2)
| SendNF(_,e,tok) -> union (toks_of_expr e) [tok] 
| ReqNF e -> toks_of_expr e
| IfNF bl -> List.fold_left (fun tl (e,cl) -> union tl (union (toks_of_expr e) (toks_of_cmd cl))) [] bl
| SimAssign _ -> failwith "toks_of_cmd1: SimAssign"
and toks_of_cmd cl = List.fold_left (fun tl c -> union tl (toks_of_cmd1 c)) [] cl

let toks_of_fun = function
  | ConstrNF(_,_,c,_) 
  | ProcNF(_,_,_,c,_) -> toks_of_cmd c

let rec toks_of_fun_decls = function
| EmptyFunDeclsNF -> []
| FunDeclSeqNF(f,fl) -> union (toks_of_fun f) (toks_of_fun_decls fl)

let toks_of_contract = function ContractNF(_,_,fdl) -> toks_of_fun_decls fdl