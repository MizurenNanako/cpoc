let () =
  let filename = Sys.argv.(1) in
  let file = In_channel.open_text filename in
  let lexbuf = Lexing.from_channel file in
  let rec loop () =
    match Raw_lexer.get_token lexbuf with
    | EOF, _ -> ()
    | tok ->
        Printf.printf "%a\n" Lexical.Token.dump tok;
        loop ()
  in
  loop ()
