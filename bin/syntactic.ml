module AST = struct
  open Sexplib.Conv

  type rng = Lexical.Range.t

  let sexp_of_rng = Lexical.Range.sexp_of_t

  type identifier = identifier' * rng [@@deriving sexp_of]
  and identifier' = Id of string

  module Expr = struct
    (* Expressions *)
    type t = t' * rng [@@deriving sexp_of]

    and t' =
      | ExprBop of bop * t * t
      | ExprUop of uop * t
      | ExprCall of identifier * args
      | ExprMem of t * identifier
      | ExprIndex of t * t (* array, index *)
      | AtomID of identifier
      | AtomINT of int32
      | AtomFLOAT of float

    and args = Args of t list
    and uop = OpNeg | OpNot

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
      | OpEq
  end

  type expr = Expr.t

  let sexp_of_expr = Expr.sexp_of_t

  type t = program [@@deriving sexp_of]

  (* High-level Definitions *)
  and program = Program of extdef list
  and extdef = extdef' * rng

  and extdef' =
    | ExtDefVar of specifier * vardec list
    | ExtDefFun of specifier * fundec * compst

  (* Specifiers *)
  and specifier = specifier' * rng

  and specifier' =
    | SpecINT
    | SpecFLOAT
    | SpecStruct of structspecifier

  and structspecifier = structspecifier' * rng

  and structspecifier' =
    | StructSpecShort of tag
    | StructSpecFull of tag option * deflist

  and tag = tag' * rng
  and tag' = Tag of identifier

  (* Declarators *)
  and vardec = vardec' * rng
  and vardec' = VarDec of identifier | VarDecArr of vardec * int
  and fundec = fundec' * rng
  and fundec' = FunDec of identifier * varlist
  and varlist = VarList of paramdec list
  and paramdec = paramdec' * rng
  and paramdec' = ParamDec of specifier * vardec

  (* Statements *)
  and compst = compst' * rng
  and compst' = CompSt of deflist * stmtlist
  and stmtlist = StmtList of stmt list
  and stmt = stmt' * rng

  and stmt' =
    | StmtExp of expr
    | StmtComp of compst
    | StmtRet of expr
    | StmtIf of expr * stmt
    | StmtIfElse of expr * stmt * stmt
    | StmtWhile of expr * stmt

  (* Local Definitions *)
  and deflist = DefList of def list
  and def = def' * rng
  and def' = Def of specifier * declist
  and declist = DecList of dec list
  and dec = dec' * rng
  and dec' = Dec of vardec * expr option
end

module ASTUtil = struct
  let i' out level =
    let rec i'' level =
      match level with
      | 0 -> ()
      | n ->
          output_string out "  ";
          n |> pred |> i''
    in
    i'' level

  let l' out (l : 'a list) dumper st sp ed =
    st out;
    let rec l'' l =
      match l with
      | [] -> ed out
      | [ a ] ->
          dumper out a;
          ed out
      | hd :: tl ->
          dumper out hd;
          sp out;
          l'' tl
    in
    l'' l
end
