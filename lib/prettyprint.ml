open Ast
open Types
open Utils

(******************************************************************************)
(*                             Pretty printing of AST                         *)
(******************************************************************************)

let string_of_val = function
  | n -> string_of_int n

let rec tabs (t:int) (s:string) = if t=0 then s else tabs (t-1) ("  " ^ s) 

let addparen s d = if d>0 then "(" ^ s ^ ")" else s

let rec string_of_expr e = match e with
    True -> "true"
  | False -> "false"
  | Var x -> x
  | Map(e1,e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | IntConst n -> string_of_int n
  | AddrConst n -> "address(" ^ string_of_int n ^ ")"
  | StringConst s -> "\"" ^ s ^ "\""
  | Not e -> "!(" ^ string_of_expr e ^ ")"
  | And(e1,e2) -> bool_binop (string_of_expr e1) (string_of_expr e2) "&&" (depth_expr e)
  | Or(e1,e2)  -> bool_binop (string_of_expr e1) (string_of_expr e2) "||" (depth_expr e)
  | Add(e1,e2) -> int_binop e1 e2 "+"  (depth_expr e)
  | Sub(e1,e2) -> int_binop e1 e2 "-"  (depth_expr e)
  | Mul(e1,e2) -> int_binop e1 e2 "*"  (depth_expr e)
  | Div(e1,e2) -> int_binop e1 e2 "/"  (depth_expr e)
  | Eq(e1,e2)  -> int_binop e1 e2 "==" (depth_expr e)
  | Neq(e1,e2) -> int_binop e1 e2 "!=" (depth_expr e)
  | Leq(e1,e2) -> int_binop e1 e2 "<=" (depth_expr e)
  | Le(e1,e2)  -> int_binop e1 e2 "<"  (depth_expr e)
  | Geq(e1,e2) -> int_binop e1 e2 ">=" (depth_expr e)
  | Ge(e1,e2)  -> int_binop e1 e2 ">"  (depth_expr e)
  | Bal(t)     -> "balance(" ^ t ^ ")"
  | BalPre(t)  -> "balance_pre(" ^ t ^ ")"  
  | IfE(e1,e2,e3) -> "(" ^ string_of_expr e1 ^ " ? " ^ string_of_expr e2 ^ " : " ^ string_of_expr e3 ^ ")"
  | MapUpd(e1,e2,e3) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "->" ^ string_of_expr e3 ^ "]"

and bool_binop s1 s2 op d = addparen (s1 ^ " " ^ op ^ " " ^ s2) d

and int_binop e1 e2 op d = let s1,s2 = string_of_expr e1,string_of_expr e2 in
  addparen (s1 ^ op ^ (if op="+" then s2 else addparen s2 1)) d 

and string_of_cmd t = function
    Skip -> tabs t "skip;"
  | VarAssign(x,e) -> tabs t (x ^ "=" ^ string_of_expr e ^ ";")
  | Seq(c1,c2) -> (string_of_cmd t c1) ^ "\n" ^ (string_of_cmd t c2)
  | If(e,c1,Skip) -> 
    tabs t "if (" ^ string_of_expr e ^ ") {\n" ^ 
      (string_of_cmd (t+1) c1) ^ 
    "\n" ^ tabs t "}"
  | If(e,c1,c2) -> 
    tabs t "if (" ^ string_of_expr e ^ ") {\n" ^ 
      (string_of_cmd (t+1) c1) ^ 
    "\n" ^ tabs t "}" ^
    "\n" ^ tabs t "else {\n" ^ 
      (string_of_cmd (t+1) c2 ^ 
    "\n" ^ tabs t "}")
  | Send(x,e,tok) -> tabs t (x ^ ".transfer(" ^ (string_of_expr e) ^ ":" ^ tok ^ ");")
  | Req(e) -> tabs t ("require " ^ string_of_expr e ^ ";")


let string_of_btype = function
  | TInt -> "int"
  | TUint -> "uint"
  | TAddr -> "address"
  | TString -> "string"

let string_of_arg (t,x)= (string_of_btype t) ^ " " ^ x

let string_of_args = List.fold_left (fun s a -> s ^ (if s<>"" then "," else "") ^ (string_of_arg a)) ""

let string_of_var_decl = function
  | VarDecl(t,x) -> string_of_btype t ^ " " ^ x
  | MapDecl(t1,t2,x) -> "mapping (" ^ string_of_btype t1 ^ " => " ^ string_of_btype t2 ^ ") " ^ x    

let string_of_fmod = function
  | AuthFMod(x)-> "auth(" ^ x ^ ")"
  | AfterFMod(e) -> "after(" ^ (string_of_expr e) ^ ")"
  | InputFMod(etl) -> "input(" ^ (List.fold_left (fun s (e,t) -> s ^ (if s<>"" then "," else "") ^ (string_of_expr e) ^ ":" ^ t) "" etl) ^ ")"

let rec string_of_fmods = function
  | EmptyFMods -> ""
  | FModSeq(fm,EmptyFMods) -> " " ^ (string_of_fmod fm) ^ " " 
  | FModSeq(fm,fml) -> " " ^ (string_of_fmod fm) ^ (string_of_fmods fml)

let string_of_nexts = function 
    [] -> ""
  | l -> " next(" ^ (List.fold_left (fun s f -> s ^ (if s<>"" then "," else "") ^ f) "" l) ^ ")"

let string_of_fun_decl = function  
  | Constr(a,fml,c,nl) -> 
    "\n" ^ tabs 1 ("constructor " ^ "(" ^ (string_of_args a) ^ ")" ^ (string_of_fmods fml) ^ "{\n") ^ 
      (if c=Skip then "" else (string_of_cmd 2 c) ^ "\n") ^ 
    tabs 1 "}" ^ (string_of_nexts nl)                
  | Proc(f,a,fml,c,nl) -> 
    "\n" ^ tabs 1 ("function " ^ f ^ "(" ^ (string_of_args a) ^ ")" ^ (string_of_fmods fml) ^ "{\n") ^ 
      (if c=Skip then "" else (string_of_cmd 2 c) ^ "\n") ^
    tabs 1 "}" ^ (string_of_nexts nl)

let rec string_of_var_decls = function
  EmptyVarDecls -> "\n"
| VarDeclSeq(vd,vds) -> "\n" ^ 
    tabs 1 (string_of_var_decl vd) ^ ";" ^ 
    (string_of_var_decls vds)

let rec string_of_fun_decls = function
  EmptyFunDecls -> "" 
| FunDeclSeq(fd,fds) -> (string_of_fun_decl fd) ^ "\n" ^ (string_of_fun_decls fds)

let string_of_contract (Contract(c,vdl,fdl)) = 
  "contract " ^ c ^ " { " ^ (string_of_var_decls vdl) ^ (string_of_fun_decls fdl) ^ " }"


(******************************************************************************)
(*                        Pretty-printing of NF0 contracts                    *)
(******************************************************************************)

let rec string_of_cmdNF1 t = function
  | SkipNF -> tabs t "skip;"
  | VarAssignNF(x,e) -> tabs t (x ^ "=" ^ string_of_expr e ^ ";")
  | SendNF(a,e,tok) -> tabs t (string_of_expr a ^ ".transfer(" ^ (string_of_expr e) ^ ":" ^ tok ^ ");")
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
  
let string_of_fun_declNF = function
  | ConstrNF(a,fml,c,nl) -> 
    "\n" ^ tabs 1 ("constructor" ^ "(" ^ (string_of_args a) ^ ")" ^ (string_of_fmods fml) ^ " {\n") ^ 
      (if c=[] then "" else (string_of_cmdNF 2 c) ^ "\n") ^ 
    tabs 1 "}" ^ (string_of_nexts nl)                
  | ProcNF(f,a,fml,c,nl) -> 
    "\n" ^ tabs 1 ("function " ^ f ^ "(" ^ (string_of_args a) ^ ")" ^ (string_of_fmods fml) ^ " {\n") ^ 
      (if c=[] then "" else (string_of_cmdNF 2 c) ^ "\n") ^
    tabs 1 "}" ^ (string_of_nexts nl)

let rec string_of_fun_declsNF = function
  | EmptyFunDeclsNF -> ""
  | FunDeclSeqNF(f,fl) -> (string_of_fun_declNF f) ^ "\n" ^ (string_of_fun_declsNF fl)

let string_of_contractNF = function ContractNF(c,vdl,fdl) -> 
  "contract " ^ c ^ " { " ^ (string_of_var_decls vdl) ^ (string_of_fun_declsNF fdl) ^ " }"


(******************************************************************************)
(*                        Pretty-printing HeLLUM semantics                    *)
(******************************************************************************)

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

let rec last = function
    [] -> failwith "last on empty list"
  | [x] -> x
  | _::l -> last l
