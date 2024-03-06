type ide = string

type tok = string

type expr =
  | True
  | False
  | Var of ide
  | Map of ide * expr
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
  | IfE of expr * expr * expr (* ternary operator *)       
              
and cmd =
  | Skip
  | VarAssign of string * expr
  | MapAssign of string * expr * expr
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
  | Var of btype * ide
  | Mapping of btype * btype * ide

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
| VarAssignNF of string * expr
| MapAssignNF of string * expr * expr
| SendNF of ide * expr * tok
| ReqNF of expr
| IfNF of (expr * cmdNF) list
and cmdNF = cmdNF1 list

type fun_declNF =
  | ConstrNF of args * fmods * cmdNF * (ide list)
  | ProcNF of ide * args * fmods * cmdNF * (ide list)           

type fun_declsNF = EmptyFunDeclsNF | FunDeclSeqNF of fun_declNF * fun_declsNF

type contractNF = ContractNF of ide * var_decls * fun_declsNF
