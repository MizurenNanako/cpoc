module Token = struct
  exception BadToken of string

  type token =
    | INT of int32
    | FLOAT of float
    | ID of string
    | SEMI
    | COMMA
    | ASSIGNOP
    | RELOP_GT
    | RELOP_LT
    | RELOP_GEQ
    | RELOP_LEQ
    | RELOP_EEQ
    | RELOP_NEQ
    | PLUS
    | MINUS
    | STAR
    | DIV
    | AND
    | OR
    | DOT
    | NOT
    | TYPE_INT
    | TYPE_FLOAT
    | LP
    | RP
    | LB
    | RB
    | LC
    | RC
    | STRUCT
    | RETURN
    | IF
    | ELSE
    | WHILE
    | EOF

  let dump out (tok : token) : unit =
    let open Printf in
    match tok with
    | INT i -> fprintf out "%ld" i
    | FLOAT f -> fprintf out "%f" f
    | ID s -> fprintf out "%s" s
    | SEMI -> fprintf out "SEMI"
    | COMMA -> fprintf out "COMMA"
    | ASSIGNOP -> fprintf out "ASSIGNOP"
    | RELOP_GT -> fprintf out "RELOP_GT"
    | RELOP_LT -> fprintf out "RELOP_LT"
    | RELOP_GEQ -> fprintf out "RELOP_GEQ"
    | RELOP_LEQ -> fprintf out "RELOP_LEQ"
    | RELOP_EEQ -> fprintf out "RELOP_EEQ"
    | RELOP_NEQ -> fprintf out "RELOP_NEQ"
    | PLUS -> fprintf out "PLUS"
    | MINUS -> fprintf out "MINUS"
    | STAR -> fprintf out "STAR"
    | DIV -> fprintf out "DIV"
    | AND -> fprintf out "AND"
    | OR -> fprintf out "OR"
    | DOT -> fprintf out "DOT"
    | NOT -> fprintf out "NOT"
    | TYPE_INT -> fprintf out "TYPE_INT"
    | TYPE_FLOAT -> fprintf out "TYPE_FLOAT"
    | LP -> fprintf out "LP"
    | RP -> fprintf out "RP"
    | LB -> fprintf out "LB"
    | RB -> fprintf out "RB"
    | LC -> fprintf out "LC"
    | RC -> fprintf out "RC"
    | STRUCT -> fprintf out "STRUCT"
    | RETURN -> fprintf out "RETURN"
    | IF -> fprintf out "IF"
    | ELSE -> fprintf out "ELSE"
    | WHILE -> fprintf out "WHILE"
    | EOF -> fprintf out "[eof]"
end

module Literal : sig
  exception BadLiteral of string

  val dec_of_string : string -> int32
  val oct_of_string : string -> int32
  val hex_of_string : string -> int32
  val bin_of_string : string -> int32
  val float_of_string : string -> float
end = struct
  exception BadLiteral of string

  let ord = function
    | '0' -> 0l
    | '1' -> 1l
    | '2' -> 2l
    | '3' -> 3l
    | '4' -> 4l
    | '5' -> 5l
    | '6' -> 6l
    | '7' -> 7l
    | '8' -> 8l
    | '9' -> 9l
    | 'A' | 'a' -> 10l
    | 'B' | 'b' -> 11l
    | 'C' | 'c' -> 12l
    | 'D' | 'd' -> 13l
    | 'E' | 'e' -> 14l
    | 'F' | 'f' -> 15l
    | _ -> raise (BadLiteral "bad char")

  let make_fold base acc ch =
    let num = ord ch in
    Int32.add (Int32.mul acc base) num

  let dec_of_string s = String.fold_left (make_fold 10l) 0l s
  let bin_of_string s = String.fold_left (make_fold 2l) 0l s
  let oct_of_string s = String.fold_left (make_fold 8l) 0l s
  let hex_of_string s = String.fold_left (make_fold 16l) 0l s
  let float_of_string = float_of_string
end
