module Range = struct
  type pos = Lexing.position

  let sexp_of_pos (p : pos) =
    let open Sexplib.Pre_sexp in
    let s =
      Printf.sprintf "%s:%i:%i" p.pos_fname p.pos_lnum
        (p.pos_cnum - p.pos_bol + 1)
    in
    Atom s

  type t = pos * pos

  let sexp_of_t (r : t) =
    let open Sexplib.Pre_sexp in
    let a, b = r in
    let s =
      if a.pos_fname = b.pos_fname then
        if a.pos_lnum = b.pos_lnum then
          Printf.sprintf "%s:%i:%i-%i" a.pos_fname a.pos_lnum
            (a.pos_cnum - a.pos_bol + 1)
            (b.pos_cnum - b.pos_bol + 1)
        else
          Printf.sprintf "%s:%i:%i-%i:%i" a.pos_fname a.pos_lnum
            (a.pos_cnum - a.pos_bol + 1)
            b.pos_lnum
            (b.pos_cnum - b.pos_bol + 1)
      else
        Printf.sprintf "%s:%i:%i-%s:%i:%i" a.pos_fname a.pos_lnum
          (a.pos_cnum - a.pos_bol + 1)
          b.pos_fname b.pos_lnum
          (b.pos_cnum - b.pos_bol + 1)
    in
    Atom s

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

  let dumps_pos (p : pos) =
    let fname, lineno, colno =
      (p.pos_fname, p.pos_lnum, p.pos_cnum - p.pos_bol + 1)
    in
    let open Printf in
    sprintf "%s:%i:%i" fname lineno colno

  let dumps (r : t) =
    let a, b = r in
    Printf.sprintf "%s-%s" (dumps_pos a) (dumps_pos b)
end

module Token = struct
  exception BadToken of string

  type rng = Range.t

  type token =
    | INT of (int32 * rng)
    | FLOAT of (float * rng)
    | ID of (string * rng)
    | SEMI of (unit * rng)
    | COMMA of (unit * rng)
    | ASSIGNOP of (unit * rng)
    | RELOP_GT of (unit * rng)
    | RELOP_LT of (unit * rng)
    | RELOP_GEQ of (unit * rng)
    | RELOP_LEQ of (unit * rng)
    | RELOP_EEQ of (unit * rng)
    | RELOP_NEQ of (unit * rng)
    | PLUS of (unit * rng)
    | MINUS of (unit * rng)
    | STAR of (unit * rng)
    | DIV of (unit * rng)
    | AND of (unit * rng)
    | OR of (unit * rng)
    | DOT of (unit * rng)
    | NOT of (unit * rng)
    | TYPE_INT of (unit * rng)
    | TYPE_FLOAT of (unit * rng)
    | LP of (unit * rng)
    | RP of (unit * rng)
    | LB of (unit * rng)
    | RB of (unit * rng)
    | LC of (unit * rng)
    | RC of (unit * rng)
    | STRUCT of (unit * rng)
    | RETURN of (unit * rng)
    | IF of (unit * rng)
    | ELSE of (unit * rng)
    | WHILE of (unit * rng)
    | EOF

  let get_range (tk : token) =
    match tk with
    | INT (_, r)
    | FLOAT (_, r)
    | ID (_, r)
    | SEMI (_, r)
    | COMMA (_, r)
    | ASSIGNOP (_, r)
    | RELOP_GT (_, r)
    | RELOP_LT (_, r)
    | RELOP_GEQ (_, r)
    | RELOP_LEQ (_, r)
    | RELOP_EEQ (_, r)
    | RELOP_NEQ (_, r)
    | PLUS (_, r)
    | MINUS (_, r)
    | STAR (_, r)
    | DIV (_, r)
    | AND (_, r)
    | OR (_, r)
    | DOT (_, r)
    | NOT (_, r)
    | TYPE_INT (_, r)
    | TYPE_FLOAT (_, r)
    | LP (_, r)
    | RP (_, r)
    | LB (_, r)
    | RB (_, r)
    | LC (_, r)
    | RC (_, r)
    | STRUCT (_, r)
    | RETURN (_, r)
    | IF (_, r)
    | ELSE (_, r)
    | WHILE (_, r) ->
        r
    | EOF -> (Lexing.dummy_pos, Lexing.dummy_pos)

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

  module MenhirInterpreter = struct
    (* The indexed type of terminal symbols. *)

    type _ terminal =
      | T_error : unit terminal
      | T_WHILE : (unit * rng) terminal
      | T_TYPE_INT : (unit * rng) terminal
      | T_TYPE_FLOAT : (unit * rng) terminal
      | T_STRUCT : (unit * rng) terminal
      | T_STAR : (unit * rng) terminal
      | T_SEMI : (unit * rng) terminal
      | T_RP : (unit * rng) terminal
      | T_RETURN : (unit * rng) terminal
      | T_RELOP_NEQ : (unit * rng) terminal
      | T_RELOP_LT : (unit * rng) terminal
      | T_RELOP_LEQ : (unit * rng) terminal
      | T_RELOP_GT : (unit * rng) terminal
      | T_RELOP_GEQ : (unit * rng) terminal
      | T_RELOP_EEQ : (unit * rng) terminal
      | T_RC : (unit * rng) terminal
      | T_RB : (unit * rng) terminal
      | T_PLUS : (unit * rng) terminal
      | T_OR : (unit * rng) terminal
      | T_NOT : (unit * rng) terminal
      | T_MINUS : (unit * rng) terminal
      | T_LP : (unit * rng) terminal
      | T_LC : (unit * rng) terminal
      | T_LB : (unit * rng) terminal
      | T_INT : (int32 * rng) terminal
      | T_IF : (unit * rng) terminal
      | T_ID : (string * rng) terminal
      | T_FLOAT : (float * rng) terminal
      | T_EOF : unit terminal
      | T_ELSE : (unit * rng) terminal
      | T_DOT : (unit * rng) terminal
      | T_DIV : (unit * rng) terminal
      | T_COMMA : (unit * rng) terminal
      | T_ASSIGNOP : (unit * rng) terminal
      | T_AND : (unit * rng) terminal
  end
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
