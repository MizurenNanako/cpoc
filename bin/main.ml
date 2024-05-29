let () =
  let filename = Sys.argv.(1) in
  let file = In_channel.open_text filename in
  let lexbuf = Lexing.from_channel file in
  Lexing.set_filename lexbuf filename;
  let ast =
    try Parser.program Raw_lexer.get_token lexbuf with
    | Lexical.Token.BadToken msg ->
        let lcp = lexbuf.lex_curr_p in
        Printf.printf "A_error at %s:%i:%i\tBadToken \"%s\"\n"
          lcp.pos_fname lcp.pos_lnum
          (lcp.pos_cnum - lcp.pos_bol + 1)
          msg;
        exit 0
    | _ ->
        let lcp = lexbuf.lex_curr_p in
        Printf.printf "B_error at %s:%i:%i\n" lcp.pos_fname
          lcp.pos_lnum
          (lcp.pos_cnum - lcp.pos_bol + 1);
        exit 0
  in
  let ast_sexp = Syntactic.AST.sexp_of_t ast in
  Sexplib.Sexp.output_hum stdout ast_sexp;
  print_newline ()
