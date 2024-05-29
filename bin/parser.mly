%{
    open Syntactic.AST
    let j (a: 'a * rng) (b: 'b * rng) = 
        Lexical.Range.join (snd a) (snd b)

    (* let log i = Printf.eprintf "%i\n" i *)
    let log _ = ()
%}

// All tokens here are actually
// raw_token * range
// and all ast node type are actually
// realtype * range
// so all things cares range

%token <int32 * rng> INT
%token <float * rng> FLOAT
%token <string * rng> ID
%token <unit * rng> SEMI ";"
%token <unit * rng> COMMA ","
%token <unit * rng> ASSIGNOP "="

// RELOP
%token <unit * rng> RELOP_GT ">"
%token <unit * rng> RELOP_LT "<"
%token <unit * rng> RELOP_GEQ ">="
%token <unit * rng> RELOP_LEQ "<="
%token <unit * rng> RELOP_EEQ "=="
%token <unit * rng> RELOP_NEQ "!="

%token <unit * rng> PLUS "+"
%token <unit * rng> MINUS "-"
%token <unit * rng> STAR "*"
%token <unit * rng> DIV "/"
%token <unit * rng> AND "&&"
%token <unit * rng> OR "||"
%token <unit * rng> DOT "."
%token <unit * rng> NOT "!"

// TYPE
%token <unit * rng> TYPE_INT "int"
%token <unit * rng> TYPE_FLOAT "float"

%token <unit * rng> LP "("
%token <unit * rng> RP ")"
%token <unit * rng> LB "["
%token <unit * rng> RB "]"
%token <unit * rng> LC "{"
%token <unit * rng> RC "}"
%token <unit * rng> STRUCT "struct"
%token <unit * rng> RETURN "return"
%token <unit * rng> IF "if"
%token <unit * rng> ELSE "else"
%token <unit * rng> WHILE "while"

%token EOF

%right "="
%left "||"
%left "&&"
%left "!=" "==" ">=" ">" "<=" "<"
%left "+" "-"
%left "*" "/"
%right UMINUS "!"
%nonassoc "["
%left "."

%nonassoc below_ELSE
%nonassoc ELSE

%start <t> program
%type <extdef> ext_def
%type <specifier> specifier
%type <structspecifier> struct_specifier
%type <tag> tag
%type <vardec> var_dec
%type <fundec> fun_dec
%type <paramdec> param_dec
%type <compst> comp_st
%type <stmt> stmt
%type <def> def
%type <dec> dec
%type <expr> exp
%type <identifier> id
%%

program:
| l = ext_def*; EOF { log 0; Program l }

ext_def:
| sp = specifier; l = separated_list(COMMA, var_dec); b = SEMI; 
{ log 1; ExtDefVar (sp, l), j sp b }
| sp = specifier; fd = fun_dec; cps = comp_st;
{ log 2; ExtDefFun (sp, fd, cps), j sp cps }

specifier:
| m = TYPE_INT { log 3; SpecINT, snd m }
| m = TYPE_FLOAT { log 4; SpecFLOAT, snd m }
| s = struct_specifier { log 5; SpecStruct s, snd s }

struct_specifier:
| m = STRUCT; t = tag?; LC; dl = def_list; c = RC; { log 6; StructSpecFull (t, dl), j m c }
| m = STRUCT; t = tag { log 7; StructSpecShort t, j m t }

tag:
| s = id { log 8; Tag s, snd s }

var_dec:
| i = id { log 9; (VarDec i), snd i }
| vd = var_dec; LB; n = INT; m = RB; { log 10; VarDecArr (vd, fst n), j vd m }

fun_dec:
| s = id; LP; vl = var_list; b = RP; { log 11; FunDec (s, vl), j s b }

var_list:
| v = separated_list(COMMA, param_dec) { log 12; VarList v }

param_dec:
| sp = specifier; vd = var_dec { log 13; ParamDec (sp, vd), j sp vd }

comp_st:
| m = LC; dl = def_list; sl = stmt_list; b = RC; { log 14; CompSt (dl, sl), j m b }

stmt_list:
| l = stmt*; { log 15; StmtList l }

def_list:
| l = def*; { log 16; DefList l }

stmt:
| e = exp; b = SEMI; { log 17; StmtExp e, j e b }
| c = comp_st { log 18; StmtComp c, snd c }
| m = RETURN; e = exp; b = SEMI; { log 19; StmtRet e, j m b }
| m = IF; LP; e = exp; RP; s = stmt; %prec below_ELSE { log 20; StmtIf (e, s), j m s }
| m = IF; LP; e = exp; RP; s1 = stmt; ELSE; s2 = stmt; { log 21; StmtIfElse (e, s1, s2), j m s2 }
| m = WHILE; LP; e = exp; RP; s = stmt; { log 22; StmtWhile (e, s), j m s }

def:
| sp = specifier; dl = dec_list; m = SEMI; 
{ log 23; Def (sp, dl), j sp m }

dec_list:
| l = separated_list(COMMA, dec); { log 24; DecList l }

dec:
| vd = var_dec; { log 25; Dec (vd, None), snd vd }
| vd = var_dec; ASSIGNOP; e = exp; { log 26; Dec (vd, Some e), j vd e }

exp:
| e1 = exp; ASSIGNOP; e2 = exp; { log 27; ExprBop (OpEq, e1, e2), j e1 e2 }
| e1 = exp; AND; e2 = exp; { log 28; ExprBop (OpAnd, e1, e2), j e1 e2 }
| e1 = exp; OR; e2 = exp; { log 29; ExprBop (OpOr, e1, e2), j e1 e2 }
| e1 = exp; RELOP_GT; e2 = exp; { log 30; ExprBop (OpGt, e1, e2), j e1 e2 }
| e1 = exp; RELOP_LT; e2 = exp; { log 31; ExprBop (OpLt, e1, e2), j e1 e2 }
| e1 = exp; RELOP_GEQ; e2 = exp; { log 32; ExprBop (OpGeq, e1, e2), j e1 e2 }
| e1 = exp; RELOP_LEQ; e2 = exp; { log 33; ExprBop (OpLeq, e1, e2), j e1 e2 }
| e1 = exp; RELOP_EEQ; e2 = exp; { log 34; ExprBop (OpEeq, e1, e2), j e1 e2 }
| e1 = exp; RELOP_NEQ; e2 = exp; { log 35; ExprBop (OpNeq, e1, e2), j e1 e2 }
| e1 = exp; PLUS; e2 = exp; { log 36; ExprBop (OpAdd, e1, e2), j e1 e2 }
| e1 = exp; MINUS; e2 = exp; { log 37; ExprBop (OpSub, e1, e2), j e1 e2 }
| e1 = exp; STAR; e2 = exp; { log 38; ExprBop (OpMul, e1, e2), j e1 e2 }
| e1 = exp; DIV; e2 = exp; { log 39; ExprBop (OpDiv, e1, e2), j e1 e2 }
| LP; e = exp; RP; { log 40; e }
| m = MINUS; e = exp; %prec UMINUS { log 41; ExprUop (OpNeg, e), j m e }
| m = NOT; e = exp; { log 42; ExprUop (OpNot, e), j m e }
| i = id; LP; a = args; m = RP; 
{ log 43; ExprCall (i, a), j i m }
| e1 = exp; LB; e2 = exp; m = RB; { log 44; ExprIndex (e1, e2), j e1 m }
| e1 = exp; DOT; s = id; { log 45; ExprMem (e1, s), j e1 s }
| s = id { log 46; AtomID s, snd s }
| i = INT { log 47; AtomINT (fst i), snd i }
| f = FLOAT { log 48; AtomFLOAT (fst f), snd f }

args:
| l = separated_list(COMMA, exp); { log 49; Args l }

id: 
| i = ID; { log 50; Id (fst i), snd i }