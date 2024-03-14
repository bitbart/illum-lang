open Ast
open Utils

(******************************************************************************)
(*                             Pretty printing of AST                         *)
(******************************************************************************)

let string_of_val = function
  | n -> string_of_int n

let rec tabs (t:int) (s:string) = if t=0 then s else tabs (t-1) ("  " ^ s) 

let addparen s d = if d>0 then "(" ^ s ^ ")" else s

let is_simple = function 
| Not _ -> true 
| Eq(e1,e2)
| Neq(e1,e2)
| Leq(e1,e2)
| Le(e1,e2)
| Geq(e1,e2)
| Ge(e1,e2) -> depth_expr e1 = 0 && depth_expr e2 = 0
| _ -> false

let rec string_of_expr e = match e with
    True -> "true"
  | False -> "false"
  | Var x -> x
  | Map(e1,e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | IntConst n -> string_of_int n
  | AddrConst n -> "address(" ^ string_of_int n ^ ")"
  | StringConst s -> "\"" ^ s ^ "\""
  | Not e -> "!(" ^ string_of_expr e ^ ")"
  | And(e1,e2) -> bool_binop e1 e2 "&&" 
  | Or(e1,e2)  -> bool_binop e1 e2 "||"
  | Add(e1,e2) -> int_binop e1 e2 "+"  
  | Sub(e1,e2) -> int_binop e1 e2 "-"  
  | Mul(e1,e2) -> int_binop e1 e2 "*"  
  | Div(e1,e2) -> int_binop e1 e2 "/"  
  | Eq(e1,e2)  -> int_binop e1 e2 "==" 
  | Neq(e1,e2) -> int_binop e1 e2 "!=" 
  | Leq(e1,e2) -> int_binop e1 e2 "<=" 
  | Le(e1,e2)  -> int_binop e1 e2 "<"  
  | Geq(e1,e2) -> int_binop e1 e2 ">=" 
  | Ge(e1,e2)  -> int_binop e1 e2 ">"  
  | Bal(t)     -> "balance(" ^ t ^ ")"
  | BalPre(t)  -> "balance_pre(" ^ t ^ ")"  
  | IfE(e1,e2,e3) -> "(" ^ string_of_expr e1 ^ " ? " ^ string_of_expr e2 ^ " : " ^ string_of_expr e3 ^ ")"
  | MapUpd(e1,e2,e3) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "->" ^ string_of_expr e3 ^ "]"

and bool_binop e1 e2 op  = 
  let s1,s2 = string_of_expr e1,string_of_expr e2 in
  (if is_simple e1 then s1 else addparen s1 (depth_expr e1)) ^ " " ^ op ^ " " ^ 
  (if is_simple e2 then s2 else addparen s2 (depth_expr e2))

and int_binop e1 e2 op = 
  let s1,s2 = string_of_expr e1,string_of_expr e2 in
  addparen s1 (depth_expr e1) ^ op ^ addparen s2 (depth_expr e2) 

(******************************************************************************)
(*                        Pretty-printing of HeLLUM types                     *)
(******************************************************************************)

let string_of_btype = function
  | TBool -> "bool"
  | TInt -> "int"
  | TUint -> "uint"
  | TAddr -> "address"
  | TString -> "string"

let string_of_hlltype = function
  | TBase t -> string_of_btype t
  | TMap(t1,t2) -> "mapping(" ^ string_of_btype t1 ^ " => " ^ string_of_btype t2 ^ ")"

let string_of_type_error(e,t_act,t_exp) =  
  "Type error: " ^ string_of_expr e ^ 
  " has type " ^ string_of_hlltype t_act ^ 
  " but an expression was expected of type " ^ string_of_hlltype t_exp 

(******************************************************************************)
(*                        Pretty-printing of NF contracts                     *)
(******************************************************************************)

let string_of_arg (t,x)= (string_of_btype t) ^ " " ^ x

let string_of_args = List.fold_left (fun s a -> s ^ (if s<>"" then "," else "") ^ (string_of_arg a)) ""

let string_of_nexts = function 
    [] -> ""
  | l -> " next(" ^ (List.fold_left (fun s f -> s ^ (if s<>"" then "," else "") ^ f) "" l) ^ ")"

let rec string_of_cmdNF1 t = function
  | SkipNF -> tabs t "skip;"
  | VarAssignNF(x,e) -> tabs t (x ^ "=" ^ string_of_expr e ^ ";")
  | XferNF(x,e,tok) -> tabs t (x ^ ".transfer(" ^ (string_of_expr e) ^ ":" ^ tok ^ ");")
  | ReqNF(e) -> tabs t ("require " ^ string_of_expr e ^ ";")
  | IfNF [] -> failwith "should never happen"
  | IfNF [(e,c1)] -> 
    tabs t "if (" ^ string_of_expr e ^ ") {\n" ^ 
      (string_of_cmdNF (t+1) c1) ^ 
    "\n" ^ tabs t "}"
  | IfNF [(e1,c1);(True,[])] -> 
    tabs t "if (" ^ string_of_expr e1 ^ ") {\n" ^ 
      (string_of_cmdNF (t+1) c1) ^ 
    "\n" ^ tabs t "}" ^
    "\n" ^ tabs t "else { }\n"
  | IfNF [(e1,c1);(True,c2)] -> 
    tabs t "if (" ^ string_of_expr e1 ^ ") {\n" ^ 
      (string_of_cmdNF (t+1) c1) ^ 
    "\n" ^ tabs t "}" ^
    "\n" ^ tabs t "else {\n" ^ 
      (string_of_cmdNF (t+1) c2 ^ 
    "\n" ^ tabs t "}")
  | IfNF ((e1,c1)::bl) -> let s = string_of_cmdNF1 t (IfNF bl) in
    tabs t "if (" ^ string_of_expr e1 ^ ") {\n" ^ 
      (string_of_cmdNF (t+1) c1) ^ 
    "\n" ^ tabs t "}" ^
    "\n" ^ tabs t "else " ^ 
    s
  | SimAssign al ->
    let (xl,el) = (List.map fst al, List.map snd al) in 
    tabs t (
      (List.fold_left (fun s x -> s ^ (if s<>"" then "," else "") ^ x) "" xl) ^ " |= " ^
      (List.fold_left (fun s e -> s ^ (if s<>"" then "," else "") ^ string_of_expr e) "" el) ^ 
      ";")

and string_of_cmdNF t cl = List.fold_left (fun s c -> s ^ (if s<>"" then "\n" else "") ^ (string_of_cmdNF1 t c)) "" cl

let string_of_fmodsNF (m: fmodsNF) : string =
  (if m.auths = [] then "" 
  else " auth("  ^ (List.fold_left (fun s x -> s ^ (if s<>"" then "," else "") ^ x) "" (m.auths)) ^ ")")
  ^
  (if m.afters = [] then "" 
  else " after(" ^ (List.fold_left (fun s e -> s ^ (if s<>"" then "," else "") ^ (string_of_expr e)) "" (m.afters)) ^ ")")
  ^
  (if m.inputs = [] then ""
  else " input(" ^ (List.fold_left (fun s (e,t) -> s ^ (if s<>"" then "," else "") ^ (string_of_expr e) ^ ":" ^ t) "" (m.inputs)) ^ ")")

let rec string_of_var_declsNF t = function
  | [] -> ""
  | (x,tx)::l -> let s = string_of_var_declsNF t l in 
      tabs (t+1) (string_of_hlltype tx ^ " " ^ x ^ ";" ^ "\n" ^ s)
  
let string_of_fun_declNF = function
  | ConstrNF(a,fml,vdl,c,nl) -> 
    "\n" ^ tabs 1 ("constructor" ^ "(" ^ (string_of_args a) ^ ")" ^ (string_of_fmodsNF fml) ^ " {\n") ^ 
    (string_of_var_declsNF 1 vdl) ^
    (if c=[] then "" else (string_of_cmdNF 2 c) ^ "\n") ^ 
    tabs 1 "}" ^ (string_of_nexts nl)                
  | ProcNF(f,a,fml,vdl,c,nl) -> 
    "\n" ^ tabs 1 ("function " ^ f ^ "(" ^ (string_of_args a) ^ ")" ^ (string_of_fmodsNF fml) ^ " {\n") ^ 
    (string_of_var_declsNF 1 vdl) ^
    (if c=[] then "" else (string_of_cmdNF 2 c) ^ "\n") ^
    tabs 1 "}" ^ (string_of_nexts nl)

let string_of_fun_declsNF = List.fold_left 
  (fun s f -> s ^ (if s<>"" then "\n" else "") ^ (string_of_fun_declNF f)) ""
 
let string_of_contractNF = function ContractNF(c,vdl,fdl) -> 
  "contract " ^ c ^ " {\n" ^ (string_of_var_declsNF 0 vdl) ^ (string_of_fun_declsNF fdl) ^ "\n}"


(******************************************************************************)
(*                        Pretty-printing ILLUM clauses                       *)
(******************************************************************************)

let string_of_decs d = 
  (if d.auth = [] then ""
  else "auth(" ^ (List.fold_left (fun s x -> s ^ (if s<>"" then "," else "") ^ x) "" d.auth) ^ ") ") 
  ^
  (if d.afterAbs = [] then ""
  else "afterAbs(" ^ (List.fold_left (fun s e -> s ^ (if s<>"" then "," else "") ^ string_of_expr e) "" d.afterAbs) ^ ") ")
  ^
  (if d.afterRel = [] then ""
  else "afterRel(" ^ (List.fold_left (fun s e -> s ^ (if s<>"" then "," else "") ^ string_of_expr e) "" d.afterRel) ^ ") ")

let string_of_call1 (x,el) = 
  x ^ "(" ^ (List.fold_left (fun s e -> s ^ (if s<>"" then "," else "") ^ string_of_expr e) "" el) ^ ")"

let rec string_of_contrD = function
| Call [(x,el)] -> "call " ^ string_of_call1 (x,el)
| Call l -> 
  "call( " ^
  (List.fold_left (fun s (x,el) -> s ^ (if s<>"" then " || " else "") ^ string_of_call1 (x,el)) "" l) ^ 
  " )"
| Send l -> "send(" ^ (List.fold_left (fun s (x,e,t) -> s ^ string_of_expr e ^ ":" ^ t ^ " -> " ^ x) "" l) ^ ")"

and string_of_contrC l = List.fold_left 
  (fun s c -> 
   s ^ 
   (if s<>"" then "\n" else "") ^ 
   tabs 2
   (let sdecs = string_of_decs (fst c) in (if sdecs="" then "" else sdecs ^ ": ") ^ 
   string_of_contrD (snd c))
  ) 
  "" l 

let string_of_wallet w = "wallet: " ^
  List.fold_left (fun s (e,t) -> s ^ (if s<>"" then "," else "") ^ string_of_expr e ^ ":" ^ t) "" w

let string_of_prep e = "require: " ^ string_of_expr e

let string_of_clause (c:clause) = 
  "clause " ^ c.name ^ 
  "(" ^ 
  (List.fold_left (fun s x -> s ^ (if s<>"" then "," else "") ^ x) "" c.spar) ^ "; " ^ 
  (List.fold_left (fun s x -> s ^ (if s<>"" then "," else "") ^ x) "" c.dpar) ^ ") {\n" ^
  tabs 1 (string_of_wallet c.walp) ^ "\n" ^
  tabs 1 (string_of_prep c.prep) ^ "\n" ^
  tabs 1 "branch: \n" ^
  string_of_contrC c.cntr ^ 
  "\n}\n"

(*
  name: ide;               (* X = clause name *)
  spar: ide list;          (* alpha = static parameters *)
  dpar: ide list;          (* beta = dynamic parameters *)
  walp: (expr * tok) list; (* (e:T, ...) = wallet in the funding precondition *)
  prep: expr;              (* p = boolean precondition *) 
  cntr: contrC             (* contract code *)
*)

(******************************************************************************)
(*                        Pretty-printing HeLLUM semantics                    *)
(******************************************************************************)

(*
open Sem_hellum

let string_of_env1 s x = match topenv s x with
  | IVar l -> string_of_int l ^ "/" ^ x
  | IProc(a,c) -> "fun(" ^ string_of_args a ^ "){" ^ string_of_cmd 1 c ^ "}/" ^ x

let rec string_of_env s = function
    [] -> ""
  | [x] -> (try string_of_env1 s x with _ -> "")
  | x::dom' -> (try string_of_env1 s x ^ "," ^ string_of_env s dom'
                with _ -> string_of_env s dom')

let string_of_mem1 (m,l) i =
  assert (i<l);
  string_of_val (m i) ^ "/" ^ string_of_int i

let rec range a b = if b<a then [] else a::(range (a+1) b);;

let string_of_mem (m,l) =
  List.fold_left (fun str i -> str ^ (try string_of_mem1 (m,l) i ^ "," with _ -> "")) "" (range 0 (l - 1))

let rec getlocs e = function
    [] -> []
  | x::dom -> try (match e x with
    | IVar l -> l::(getlocs e dom)
    | IProc(_,_) -> [])
    with _ -> getlocs e dom

let string_of_state st dom =
  "[" ^ string_of_env st dom ^ "], " ^
  "[" ^ string_of_mem (getmem st,getloc st) ^ "]" ^ ", " ^
  string_of_int (getloc st)


let string_of_conf vars = function
    St st -> string_of_state st vars
  | Cmd(c,st) -> "<" ^ string_of_cmd 0 c ^ ", " ^ string_of_state st vars ^ ">"

let rec string_of_trace vars = function
    [] -> ""
  | [x] -> (string_of_conf vars x)
  | x::l -> (string_of_conf vars x) ^ "\n -> " ^ string_of_trace vars l

*)