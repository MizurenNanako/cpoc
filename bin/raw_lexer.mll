{
    open Lexing
    open Lexical.Token
    module L = Lexical.Literal
    let r = Lexical.Range.of_lexbuf
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

rule get_token = parse
| ' '+ { get_token lexbuf }
| "//" [^ '\n']* '\n' { new_line lexbuf; get_token lexbuf }
| "/*" { skip_comment lexbuf; get_token lexbuf; }
| "\n" { new_line lexbuf; get_token lexbuf }
| zero { INT (0l, r lexbuf) }
| literial_real as f { FLOAT (L.float_of_string f, r lexbuf) }
| literial_dec as a { INT (L.dec_of_string a, r lexbuf) }
| literial_oct as a { INT (L.oct_of_string a, r lexbuf) }
| literial_bin as a { INT (L.bin_of_string a, r lexbuf) }
| literial_hex as a { INT (L.hex_of_string a, r lexbuf) }
| ";" { SEMI ((), r lexbuf) }
| "," { COMMA ((), r lexbuf) }
| "=" { ASSIGNOP ((), r lexbuf) }
| ">" { RELOP_GT ((), r lexbuf) }
| "<" { RELOP_LT ((), r lexbuf) }
| ">=" { RELOP_GEQ ((), r lexbuf) }
| "<=" { RELOP_LEQ ((), r lexbuf) }
| "==" { RELOP_EEQ ((), r lexbuf) }
| "!=" { RELOP_NEQ ((), r lexbuf) }
| "+" { PLUS ((), r lexbuf) }
| "-" { MINUS ((), r lexbuf) }
| "*" { STAR ((), r lexbuf) }
| "/" { DIV ((), r lexbuf) }
| "&&" { AND ((), r lexbuf) }
| "||" { OR ((), r lexbuf) }
| "." { DOT ((), r lexbuf) }
| "!" { NOT ((), r lexbuf) }
| "int" { TYPE_INT ((), r lexbuf) }
| "float" { TYPE_FLOAT ((), r lexbuf) }
| "(" { LP ((), r lexbuf) }
| ")" { RP ((), r lexbuf) }
| "[" { LB ((), r lexbuf) }
| "]" { RB ((), r lexbuf) }
| "{" { LC ((), r lexbuf) }
| "}" { RC ((), r lexbuf) }
| "struct" { STRUCT ((), r lexbuf) }
| "return" { RETURN ((), r lexbuf) }
| "if" { IF ((), r lexbuf) }
| "else" { ELSE ((), r lexbuf) }
| "while" { WHILE ((), r lexbuf) }
| identifier as s { ID (s, r lexbuf) }
| eof { EOF }
| _ as a { raise (BadToken (Printf.sprintf "%c" a)) }

and skip_comment = parse
| "*/" { () }
| '\n' { new_line lexbuf; skip_comment lexbuf }
| eof { raise (BadToken "unterminated comment") }
| [^ '*']+ { skip_comment lexbuf }
| _ { skip_comment lexbuf }
