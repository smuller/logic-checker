{

open Parser
open Lexing
exception Error of string

}

rule token = parse
| ['\t'] ['\t' ' ']* { TAB }
| ' ' { token lexbuf }
| "//" { comment lexbuf }
| '\n' { Lexing.new_line lexbuf; NEWLINE }
| eof { EOF }
| ['0'-'9']+ as i { INT (int_of_string i) }
| ['a'-'z']+ as s { LID s }
| 'T' { TRUE }
| 'F' { FALSE }
| "/\\" { AND }
| "\\/" { OR }
| "->" { COND }
| "<->" { BICOND }
| "~" { NOT }
| "=>" { IMP }
| "Exists" { EXISTS }
| "Forall" { FORALL }
| '(' { LPAREN }
| ')' { RPAREN }
| ',' { COMMA }
| '.' { DOT }
| ['A'-'Z'] ['a'-'z' 'A'-'Z']* as s { UID s }
| _
    { raise (Error (Printf.sprintf "%d:%d: unexpected character.\n"
    ((lexeme_start_p lexbuf).pos_lnum)
     (((lexeme_start_p lexbuf).pos_cnum) -
         ((lexeme_start_p lexbuf).pos_bol)))) }
and comment = parse
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _ { comment lexbuf }