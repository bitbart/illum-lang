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
  | Mod of expr * expr
  | Hash of expr
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
  | VerSig of expr * expr             (* verify signature *)
  | Expand of ide * (expr list)       (* expand a view function *)
  | StrLen of expr                    (* string length *)
  | SubStr of expr * expr * expr      (* substring *)
  | IntOfString of expr               (* convert string to int *)

(******************************************************************************)
(*                                     HeLLUM AST                             *)
(******************************************************************************)

type cmd =
  | Skip
  | VarAssign of ide * expr
  | Seq of cmd * cmd
  | Xfer of ide * expr * tok
  | If of expr * cmd * cmd
  | Req of expr

and btype = TInt | TUint | TAddr | TString | TBool
and hlltype = TBase of btype | TMap of btype * btype

and arg = ide * btype

and args = arg list

type fmod = 
| AuthFMod of ide
| AfterFMod of expr
| InputFMod of expr * tok

and fmods = EmptyFMods | FModSeq of fmod * fmods 

type var_decl =
  | VarDecl of btype * ide
  | MapDecl of btype * btype * ide

and var_decls = EmptyVarDecls | VarDeclSeq of var_decl * var_decls

and fun_decl =
  | Constr of args * fmods * var_decls * cmd * (ide list)
  | Proc of ide * args * fmods * var_decls * cmd * (ide list)
  | View of ide * args * expr          

type fun_decls = EmptyFunDecls | FunDeclSeq of fun_decl * fun_decls

type contract = Contract of ide * var_decls * fun_decls

(******************************************************************************)
(*                          NF0: lists for seq and if branches                *)
(******************************************************************************)

type cmdNF1 = 
| SkipNF
| VarAssignNF of ide * expr
| XferNF of ide * expr * tok
| ReqNF of expr
| IfNF of (expr * cmdNF) list
| SimAssign of (ide * expr) list (* simultaneous assignment - non produced by the parser *)
and cmdNF = cmdNF1 list

type fmodsNF = { 
  auths : ide list;
  afters: expr list;
  inputs: (expr * tok) list 
}

type var_declsNF = (ide * hlltype) list

type fun_declNF =
  | ConstrNF of args * fmodsNF * var_declsNF * cmdNF * (ide list)
  | ProcNF of ide           (* function name *) 
            * args          (* function parameters *)
            * fmodsNF       (* function modifiers *)
            * var_declsNF   (* local variables *)
            * cmdNF         (* function body *)
            * (ide list)    (* next callable functions *)       

type contractNF = ContractNF of ide * var_declsNF * (fun_declNF list)


(******************************************************************************)
(*                                     ILLUM AST                              *)
(******************************************************************************)

type decorators = { 
  auth: ide list;
  afterAbs: expr list;
  afterRel: expr list 
}

type contrD = 
| Call of (ide * expr list)  list    (* call X(e1,...;?) | Y(...) | ... *)
| Send of (ide * expr * tok) list    (* send(e:T -> a || ...) *)

(* contrC is a choice between branches *)
and contrC = (decorators * contrD) list

and clause = {
  name: ide;               (* X = clause name *)
  spar: ide list;          (* alpha = static parameters *)
  dpar: ide list;          (* beta = dynamic parameters *)
  walp: (expr * tok) list; (* (e:T, ...) = wallet in the funding precondition *)
  prep: expr;              (* p = boolean precondition *) 
  cntr: contrC             (* contract code *)
}
