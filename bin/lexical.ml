module Range = struct
  type pos = Lexing.position

  let sexp_of_pos (p : pos) =
    let open Sexplib.Pre_sexp in
    let s =
      Printf.sprintf "%s:%i:%i" p.pos_fname p.pos_lnum
        (p.pos_cnum - p.pos_bol + 1)
    in
    Atom s

  type t = pos * pos [@@deriving sexp_of]

  let join (a : t) (b : t) : t = (fst a, snd b)

  let of_lexbuf (lexbuf : Lexing.lexbuf) : t =
    (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

  let dump_pos out (p : pos) =
    let fname, lineno, colno =
      (p.pos_fname, p.pos_lnum, p.pos_cnum - p.pos_bol + 1)
    in
    let open Printf in
    fprintf out "%s:%i:%i" fname lineno colno

  let dump out (r : t) =
    let a, b = r in
    Printf.fprintf out "%a-%a" dump_pos a dump_pos b
end

module Token = struct
  exception BadToken of string

  type token =
    | INT of (int32 * Range.t)
    | FLOAT of (float * Range.t)
    | ID of (string * Range.t)
    | SEMI of (unit * Range.t)
    | COMMA of (unit * Range.t)
    | ASSIGNOP of (unit * Range.t)
    | RELOP_GT of (unit * Range.t)
    | RELOP_LT of (unit * Range.t)
    | RELOP_GEQ of (unit * Range.t)
    | RELOP_LEQ of (unit * Range.t)
    | RELOP_EEQ of (unit * Range.t)
    | RELOP_NEQ of (unit * Range.t)
    | PLUS of (unit * Range.t)
    | MINUS of (unit * Range.t)
    | STAR of (unit * Range.t)
    | DIV of (unit * Range.t)
    | AND of (unit * Range.t)
    | OR of (unit * Range.t)
    | DOT of (unit * Range.t)
    | NOT of (unit * Range.t)
    | TYPE_INT of (unit * Range.t)
    | TYPE_FLOAT of (unit * Range.t)
    | LP of (unit * Range.t)
    | RP of (unit * Range.t)
    | LB of (unit * Range.t)
    | RB of (unit * Range.t)
    | LC of (unit * Range.t)
    | RC of (unit * Range.t)
    | STRUCT of (unit * Range.t)
    | RETURN of (unit * Range.t)
    | IF of (unit * Range.t)
    | ELSE of (unit * Range.t)
    | WHILE of (unit * Range.t)
    | EOF

  let dump out (tok : token) : unit =
    let open Printf in
    match tok with
    | INT (i, r) -> fprintf out "INT<%ld>(%a)" i Range.dump r
    | FLOAT (f, r) -> fprintf out "FLOAT<%f>(%a)" f Range.dump r
    | ID (s, r) -> fprintf out "ID<%s>(%a)" s Range.dump r
    | SEMI (_, r) -> fprintf out "SEMI(%a)" Range.dump r
    | COMMA (_, r) -> fprintf out "COMMA(%a)" Range.dump r
    | ASSIGNOP (_, r) -> fprintf out "ASSIGNOP(%a)" Range.dump r
    | RELOP_GT (_, r) -> fprintf out "RELOP_GT(%a)" Range.dump r
    | RELOP_LT (_, r) -> fprintf out "RELOP_LT(%a)" Range.dump r
    | RELOP_GEQ (_, r) -> fprintf out "RELOP_GEQ(%a)" Range.dump r
    | RELOP_LEQ (_, r) -> fprintf out "RELOP_LEQ(%a)" Range.dump r
    | RELOP_EEQ (_, r) -> fprintf out "RELOP_EEQ(%a)" Range.dump r
    | RELOP_NEQ (_, r) -> fprintf out "RELOP_NEQ(%a)" Range.dump r
    | PLUS (_, r) -> fprintf out "PLUS(%a)" Range.dump r
    | MINUS (_, r) -> fprintf out "MINUS(%a)" Range.dump r
    | STAR (_, r) -> fprintf out "STAR(%a)" Range.dump r
    | DIV (_, r) -> fprintf out "DIV(%a)" Range.dump r
    | AND (_, r) -> fprintf out "AND(%a)" Range.dump r
    | OR (_, r) -> fprintf out "OR(%a)" Range.dump r
    | DOT (_, r) -> fprintf out "DOT(%a)" Range.dump r
    | NOT (_, r) -> fprintf out "NOT(%a)" Range.dump r
    | TYPE_INT (_, r) -> fprintf out "TYPE_INT(%a)" Range.dump r
    | TYPE_FLOAT (_, r) -> fprintf out "TYPE_FLOAT(%a)" Range.dump r
    | LP (_, r) -> fprintf out "LP(%a)" Range.dump r
    | RP (_, r) -> fprintf out "RP(%a)" Range.dump r
    | LB (_, r) -> fprintf out "LB(%a)" Range.dump r
    | RB (_, r) -> fprintf out "RB(%a)" Range.dump r
    | LC (_, r) -> fprintf out "LC(%a)" Range.dump r
    | RC (_, r) -> fprintf out "RC(%a)" Range.dump r
    | STRUCT (_, r) -> fprintf out "STRUCT(%a)" Range.dump r
    | RETURN (_, r) -> fprintf out "RETURN(%a)" Range.dump r
    | IF (_, r) -> fprintf out "IF(%a)" Range.dump r
    | ELSE (_, r) -> fprintf out "ELSE(%a)" Range.dump r
    | WHILE (_, r) -> fprintf out "WHILE(%a)" Range.dump r
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
    | 'X' | 'x' ->
        0l (* 'X' or 'x' only appears in first two of hex num *)
    | c -> raise (BadLiteral (Printf.sprintf "bad char: %c" c))

  let make_fold base acc ch =
    let num = ord ch in
    Int32.add (Int32.mul acc base) num

  let dec_of_string s = String.fold_left (make_fold 10l) 0l s
  let bin_of_string s = String.fold_left (make_fold 2l) 0l s
  let oct_of_string s = String.fold_left (make_fold 8l) 0l s
  let hex_of_string s = String.fold_left (make_fold 16l) 0l s
  let float_of_string = float_of_string
end
