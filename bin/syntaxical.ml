module AST = struct
  type identifier = string

  module Expr = struct
    (* Expressions *)
    type t =
      | ExprBop of bop * t * t
      | ExprUop of uop * t
      | ExprCall of identifier * args
      | ExprMem of t * identifier
      | ExprIndex of t * t (* array, index *)
      | ExprAtom of atom

    and args = Args of t list

    and atom =
      | AtomID of identifier
      | AtomINT of int32
      | AtomFLOAT of float

    and uop = OpNegative | OpPositive | OpNot

    and bop =
      | OpAdd
      | OpSub
      | OpMul
      | OpDiv
      | OpAnd
      | OpOr
      | OpGt
      | OpLt
      | OpGeq
      | OpLeq
      | OpEeq
      | OpNeq
  end

  type expr = Expr.t

  type t = program

  (* High-level Definitions *)
  and program = Program of extdeflist
  and extdeflist = extdef list

  and extdef =
    | ExtDefVar of specifier * extdeclist (* This list can be empty *)
    | ExtDefFun of specifier * fundec * compst

  and extdeclist = vardec list

  (* Specifiers *)
  and specifier =
    | SpecINT
    | SpecFLOAT
    | SpecStruct of structspecifier

  and structspecifier =
    | StructSpecShort of tag
    | StructSpecFull of opttag * deflist

  and opttag = OptTag of identifier option
  and tag = Tag of identifier

  (* Declarators *)
  and vardec = VarDec of identifier | VarDecArr of identifier * int
  and fundec = FunDec of varlist
  and varlist = Varlist of paramdec list
  and paramdec = ParamDec of specifier * vardec

  (* Statements *)
  and compst = CompSt of deflist * stmtlist
  and stmtlist = StmtList of stmt list

  and stmt =
    | StmtExp of expr
    | StmtComp of compst
    | StmtRet of expr
    | StmtIf of expr * stmt
    | StmtIfElse of expr * stmt * stmt
    | StmtWhile of expr * stmt

  (* Local Definitions *)
  and deflist = DefList of def list
  and def = Def of specifier * declist
  and declist = DecList of dec list
  and dec = Dec of vardec * expr option
end
