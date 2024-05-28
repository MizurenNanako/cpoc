open Lexical

module Lexer : sig
  type t

  val initialize : Lexing.lexbuf -> unit
  val start : t
  val next : t -> Token.token * t
  val get : t -> Token.token
  val current_range : t -> Range.t
  val skip_until_before : (Token.token -> bool) -> t -> t
end = struct
  let buffer = ref []
  let size = ref 0
  let more = ref (fun () -> assert false)
  let initialize lexbuf = more := fun () -> Raw_lexer.get_token lexbuf

  type t = int (* lexer state, position in token stream. *)

  let start = 0
  let get (pos : t) : Token.token = List.nth !buffer (!size - pos)
  let current_range pos = pos |> get |> Token.get_range

  let next (pos : t) : Token.token * t =
    if pos >= !size - 1 then (
      buffer := !more () :: !buffer;
      incr size);
    let pos = pos + 1 in
    (get pos, pos)

  let skip_until_before (pred : Token.token -> bool) (pos : t) : t =
    let rec aux pos =
      let token = get pos in
      if token = EOF then pos
      else if pred token then pos - 1
      else aux (snd (next pos))
    in
    aux pos
end
