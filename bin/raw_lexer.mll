{
    open Lexing
    open Lexical.Token
    module L = Lexical.Literal
}

let zero          = '0'
let identifier    = ['A'-'Z' 'a'-'z' '_' '$']+ ['A'-'Z' 'a'-'z' '0'-'9' '_' '$']*
let oct_char      = ['0'-'7']
let hex_char      = ['0'-'9' 'A'-'F' 'a'-'f']
let numbody       = ['0'-'9']
let literial_dec  = ['1'-'9'] numbody*
let literial_oct  = zero ['0'-'9']+
let literial_hex  = zero ['x' 'X'] hex_char*
let literial_bin  = zero ['b' 'B'] ['0' '1']*
let wholenumber   = ['+' '-']? numbody+
let fraction      = numbody+
let significand   = (wholenumber "." fraction) | ("." fraction) | (wholenumber ".")
let exponent      = ['e' 'E' 'p' 'P'] ['+' '-']? ['0'-'9']+
let literial_real = (significand exponent? | wholenumber exponent)

rule get_raw_token = parse
| ' '+ { get_raw_token lexbuf }
| "//" [^ '\n']* '\n' { new_line lexbuf; get_raw_token lexbuf }
| "/*" { skip_comment lexbuf; get_raw_token lexbuf; }
| "\n" { new_line lexbuf; get_raw_token lexbuf }
| zero { INT 0l }
| identifier as s { ID s }
| literial_real as f { FLOAT (L.float_of_string f) }
| literial_dec as a { INT (L.dec_of_string a) }
| literial_oct as a { INT (L.oct_of_string a) }
| literial_bin as a { INT (L.bin_of_string a) }
| literial_hex as a { INT (L.hex_of_string a) }
| ";" { SEMI }
| "," { COMMA }
| "=" { ASSIGNOP }
| ">" { RELOP_GT }
| "<" { RELOP_LT }
| ">=" { RELOP_GEQ }
| "<=" { RELOP_LEQ }
| "==" { RELOP_EEQ }
| "!=" { RELOP_NEQ }
| "+" { PLUS }
| "-" { MINUS }
| "*" { STAR }
| "/" { DIV }
| "&&" { AND }
| "||" { OR }
| "." { DOT }
| "!" { NOT }
| "int" { TYPE_INT }
| "float" { TYPE_FLOAT }
| "(" { LP }
| ")" { RP }
| "[" { LB }
| "]" { RB }
| "{" { LC }
| "}" { RC }
| "struct" { STRUCT }
| "return" { RETURN }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| eof { EOF }
| _ as a { raise (BadToken (Printf.sprintf "%c" a)) }

and skip_comment = parse
| "*/" { () }
| '\n' { new_line lexbuf; skip_comment lexbuf }
| eof { raise (BadToken "unterminated comment") }
| [^ '*']+ { skip_comment lexbuf }
| _ { skip_comment lexbuf }

{
    let get_token lexbuf = 
        let tok = get_raw_token lexbuf in
        tok, (lexbuf.lex_start_p, lexbuf.lex_curr_p)
}