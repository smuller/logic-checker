open Sys
open Parser
open Check

let parse (f: string) =
  let ch = open_in f in
  let lexbuf = Lexing.from_channel ch in
  try
    Parser.proof Lexer.token lexbuf
  with
  | Lexer.Error msg ->
     Printf.printf "%s\n" msg; exit 1
  | Types.ParseError (msg, (s, e)) ->
     Printf.printf "Syntax Error at %d:%d-%d:%d: %s\n"
       s.pos_lnum
       (s.pos_cnum - s.pos_bol)
       e.pos_lnum
       (e.pos_cnum - e.pos_bol)
       msg;
     exit 1

let proof = parse (Sys.argv.(1))

let _ =
  match check_full_proof proof with
  | Good -> Printf.printf "Proof is correct.\n"
  | Problems errs ->
     List.iter
       (fun (n, r, s) ->
         Printf.printf "Line %d: %s%s\n"
           n
           (match r with
            | Some rule -> Printf.sprintf "Can't apply %s: " rule
            | None -> "")
           s
       )
       errs;
     Printf.printf "\n";
     exit 1
  | Incomplete -> Printf.printf "No errors but proof is incomplete.\n"
