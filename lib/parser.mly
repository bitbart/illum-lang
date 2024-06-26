%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token PLUS PLUSTAKES
%token MINUS MINUSTAKES
%token MUL DIV MOD
%token HASH STRLEN SUBSTR INTOFSTRING
%token EQ NEQ LEQ LE GEQ GE
%token BALANCE
%token <string> ID
%token <string> CONST
%token <string> STRING

%token SKIP
%token TAKES
%token CMDSEP
%token IF
%token ELSE
%token REQ
%token SEND

%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET 
%token EOF

%token CONTRACT
%token CONSTR
%token FUN VIEW
%token TBOOL TINT TUINT TADDR TSTRING TMAPPING ARROW
%token AUTH AFTER INPUT NEXT
%token QMARK SENDSEP TOKSEP ARGSEP

%nonassoc ELSE

%left OR
%left AND
%nonassoc NOT
%left LE GE EQ NEQ LEQ GEQ
%left PLUS MINUS
%left MUL DIV

%start <contract> contract
%type <cmd> cmd
%type <args> args
%type <expr> expr 
%type <var_decl> var_decl
%type <var_decls> var_decls
%type <fun_decl> fun_decl
%type <fun_decls> fun_decls

%%

contract:
  | CONTRACT; c=ID; LBRACE; vdl = var_decls; fdl = fun_decls; RBRACE; EOF { Contract(c,vdl,fdl) }
;

btype:
  | TBOOL { TBool }
  | TINT { TInt }
  | TUINT { TUint }
  | TADDR { TAddr }
  | TSTRING { TString }

args:
  | a = separated_list(ARGSEP, arg) { a } 
;

arg:
  | t = btype; x = ID { (x,t) }
;

expr:
  | n = CONST { IntConst(int_of_string n) }
  | TADDR; LPAREN; n = CONST; RPAREN; { AddrConst(int_of_string n) }
  | s = STRING { StringConst(s) }
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr { Not e }
  | BALANCE; LPAREN; t=ID; RPAREN; { Bal(t) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; DIV; e2=expr { Div(e1,e2) } 
  | e1=expr; MOD; e2=expr { Mod(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; NEQ; e2=expr { Neq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | e1=expr; LE; e2=expr { Le(e1,e2) }
  | e1=expr; GEQ; e2=expr { Geq(e1,e2) }
  | e1=expr; GE; e2=expr { Ge(e1,e2) }
  | HASH; LPAREN; e = expr; RPAREN { Hash(e) }
  | SUBSTR; LPAREN; e1 = expr; ARGSEP; e2 = expr; ARGSEP; e3 = expr; RPAREN { SubStr(e1,e2,e3) }
  | INTOFSTRING; LPAREN; e = expr; RPAREN { IntOfString(e) }
  | HASH; LPAREN; e = expr; RPAREN { Hash(e) }
  | STRLEN; LPAREN; e = expr; RPAREN { StrLen(e) }
  | LPAREN; e1=expr; RPAREN; QMARK; e2=expr; TOKSEP; e3=expr; { IfE(e1,e2,e3) }
  | x = ID { Var(x) }
  | e1 = expr; LBRACKET; e2 = expr; RBRACKET; { Map(e1,e2) }
  | f = ID; LPAREN; el = separated_list(ARGSEP, expr); RPAREN; { Expand(f,el) }
  | LPAREN; e = expr; RPAREN { e }
;

cmd1: 
  | SKIP; CMDSEP; { Skip }
  | REQ; e = expr; CMDSEP; { Req(e) } 
  | x = ID; TAKES; e=expr; CMDSEP; { VarAssign(x,e) }
  | x = ID; PLUSTAKES; e=expr; CMDSEP; { VarAssign(x,Add(Var(x),e)) }
  | x = ID; MINUSTAKES; e=expr; CMDSEP; { VarAssign(x,Sub(Var(x),e)) }
  | x = ID; LBRACKET; e1 = expr; RBRACKET; TAKES; e2=expr; CMDSEP; { VarAssign(x,MapUpd(Var x,e1,e2)) }
  | x = ID; LBRACKET; e1 = expr; RBRACKET; PLUSTAKES; e2=expr; CMDSEP; { VarAssign(x,MapUpd(Var x,e1,Add(Map(Var x,e1),e2))) }
  | x = ID; LBRACKET; e1 = expr; RBRACKET; MINUSTAKES; e2=expr; CMDSEP; { VarAssign(x,MapUpd(Var x,e1,Sub(Map(Var x,e1),e2))) }
  | x = ID; SENDSEP; SEND; LPAREN; e=expr; TOKSEP; t = ID; RPAREN; CMDSEP; { Xfer(x,e,t) }
  | IF; LPAREN; e = expr; RPAREN; c1 = cmd1; { If(e,c1,Skip) }
  | IF; LPAREN; e = expr; RPAREN; LBRACE; c1 = cmd; RBRACE; { If(e,c1,Skip) }
  | IF; LPAREN; e = expr; RPAREN; c1 = cmd1; ELSE; c2 = cmd1; { If(e,c1,c2) }
  | IF; LPAREN; e = expr; RPAREN; c1 = cmd1; ELSE; LBRACE; c2 = cmd; RBRACE; { If(e,c1,c2) }
  | IF; LPAREN; e = expr; RPAREN; LBRACE; c1 = cmd; RBRACE; ELSE; c2 = cmd1; { If(e,c1,c2) }
  | IF; LPAREN; e = expr; RPAREN; LBRACE; c1 = cmd; RBRACE; ELSE; LBRACE; c2 = cmd; RBRACE; { If(e,c1,c2) }
;

cmd:
  | c = cmd1; { c }
  | c1 = cmd1; c2 = cmd; { Seq(c1,c2) }
  | c1 = cmd; c2 = cmd; { Seq(c1,c2) }
;

var_decl:
  | t = btype; x = ID { VarDecl(t,x) }
  | TMAPPING; LPAREN; t1 = btype; ARROW; t2 = btype; RPAREN; x = ID; { MapDecl(t1,t2,x) }
;

var_decls:
  | vd = var_decl; { VarDeclSeq(vd,EmptyVarDecls) }
  | vd = var_decl; CMDSEP; vdl = var_decls { VarDeclSeq(vd,vdl) }
  | { EmptyVarDecls }
;

fmod:
  | AUTH; LPAREN; x = ID; RPAREN; { AuthFMod(x) }
  | AFTER; LPAREN; e = expr; RPAREN; { AfterFMod(e) }
  | INPUT; LPAREN; e = expr; TOKSEP; t = ID; RPAREN; { InputFMod(e,t) }

fmods: 
  | fm = fmod; { FModSeq(fm,EmptyFMods) }
  | fm = fmod; fml = fmods { FModSeq(fm,fml) }
  | { EmptyFMods }
;

fname:
  | f = ID; { f }
;

nexts:
  | NEXT; LPAREN; nl = separated_list(ARGSEP, fname); RPAREN; { nl } 
;

fun_decl:
  | CONSTR; LPAREN; a = args; RPAREN; fml = fmods; LBRACE; vdl = var_decls; RBRACE; nl=nexts { Constr(a,fml,vdl,Skip,nl) }
  | CONSTR; LPAREN; a = args; RPAREN; fml = fmods; LBRACE; vdl = var_decls; RBRACE { Constr(a,fml,vdl,Skip,[]) }
  | CONSTR; LPAREN; a = args; RPAREN; fml = fmods; LBRACE; vdl = var_decls; c = cmd; RBRACE; nl = nexts; { Constr(a,fml,vdl,c,nl) }
  | CONSTR; LPAREN; a = args; RPAREN; fml = fmods; LBRACE; vdl = var_decls; c = cmd; RBRACE { Constr(a,fml,vdl,c,[]) }
  | FUN; f = ID; LPAREN; a = args; RPAREN; fml = fmods; LBRACE; vdl = var_decls; RBRACE; nl = nexts; { Proc(f,a,fml,vdl,Skip,nl) }
  | FUN; f = ID; LPAREN; a = args; RPAREN; fml = fmods; LBRACE; vdl = var_decls; RBRACE; { Proc(f,a,fml,vdl,Skip,[]) }
  | FUN; f = ID; LPAREN; a = args; RPAREN; fml = fmods; LBRACE; vdl = var_decls; c = cmd; RBRACE; nl = nexts; { Proc(f,a,fml,vdl,c,nl) }
  | FUN; f = ID; LPAREN; a = args; RPAREN; fml = fmods; LBRACE; vdl = var_decls; c = cmd; RBRACE; { Proc(f,a,fml,vdl,c,[]) }
  | FUN; f = ID; LPAREN; a = args; RPAREN; VIEW; LBRACE; e = expr; RBRACE; { View(f,a,e) }
;

fun_decls:
  | fd = fun_decl; { FunDeclSeq(fd,EmptyFunDecls) }
  | fd = fun_decl; fdl = fun_decls { FunDeclSeq(fd,fdl) } 
  | { EmptyFunDecls }
;