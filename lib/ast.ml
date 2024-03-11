type ide = string

type tok = string

type expr =
  | True
  | False
  | Var of ide
  | Map of expr * expr                (* e1[e2] *)
  | IntConst of int
  | AddrConst of int
  | StringConst of string
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Leq of expr * expr
  | Le of expr * expr           
  | Geq of expr * expr
  | Ge of expr * expr 
  | Bal of tok
  | IfE of expr * expr * expr         (* ternary operator *)
  | BalPre of tok                     (* balance of T before the call *)       
  | MapUpd of expr * expr * expr      (* e1[e2 -> e3] map update*)

(******************************************************************************)
(*                                     HeLLUM AST                             *)
(******************************************************************************)

type cmd =
  | Skip
  | VarAssign of ide * expr
  (* | MapAssign of ide * expr * expr *)  (* equivalent to VarAssign(ide, MapUpdate(Var(ide),e1,e2)) *)
  | Seq of cmd * cmd
  | Send of ide * expr * tok
  | If of expr * cmd * cmd
  | Req of expr             

and btype = TInt | TUint | TAddr | TString

and arg = btype * ide

and args = arg list

type fmod = 
| AuthFMod of ide
| AfterFMod of expr
| InputFMod of (expr * tok) list

and fmods = EmptyFMods | FModSeq of fmod * fmods 

type var_decl =
  | VarDecl of btype * ide
  | MapDecl of btype * btype * ide

and fun_decl =
  | Constr of args * fmods * cmd * (ide list)
  | Proc of ide * args * fmods * cmd * (ide list)           

type var_decls = EmptyVarDecls | VarDeclSeq of var_decl * var_decls

type fun_decls = EmptyFunDecls | FunDeclSeq of fun_decl * fun_decls

type contract = Contract of ide * var_decls * fun_decls


(******************************************************************************)
(*                          NF0: lists for seq and if branches                *)
(******************************************************************************)

type cmdNF1 = 
| SkipNF
| VarAssignNF of ide * expr
(* | MapAssignNF of ide * expr * expr *)
| SendNF of expr * expr * tok
| ReqNF of expr
| IfNF of (expr * cmdNF) list
(* problema: in SimAssign mancano le mappe! *)
| SimAssign of (ide * expr) list (* simultaneous assignment - non produced by the parser *)
and cmdNF = cmdNF1 list

type fun_declNF =
  | ConstrNF of args * fmods * cmdNF * (ide list)
  | ProcNF of ide * args * fmods * cmdNF * (ide list)           

type fun_declsNF = EmptyFunDeclsNF | FunDeclSeqNF of fun_declNF * fun_declsNF

type contractNF = ContractNF of ide * var_decls * fun_declsNF


(******************************************************************************)
(*                                     ILLUM AST                              *)
(******************************************************************************)

type decorators = { 
  auth: ide list;
  afterAbs: expr list;
  afterRel: expr list 
}

type contrD = 
| Call of (ide * expr list) list    (* call X(e1,...;?) | Y(...) | ... *)
| Send of expr * tok * ide          (* send(e:T -> a) *)

and contrC = (decorators * contrD) list

and clause = {
  name: ide;               (* X = clause name *)
  spar: ide list;          (* alpha = static parameters *)
  dpar: ide list;          (* beta = dynamic parameters *)
  walp: (expr * tok) list; (* (e:T, ...) = wallet in the funding precondition *)
  prep: expr;              (* p = boolean precondition *) 
  cntr: contrC             (* contract code *)
}
