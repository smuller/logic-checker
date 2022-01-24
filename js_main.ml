open Check
open Js_of_ocaml

let parse_string (s: string) =
  let lexbuf = Lexing.from_string s in
    Parser.proof Lexer.token lexbuf

let last_result = ref 0
       
let _ = Js.export "logCheck"
  (object%js
     method check s =
       Js.string (
           try
             (
       match check_full_proof (parse_string (Js.to_string s)) with
       | Good -> last_result := 0; "Proof is correct.\n"
       | Problems errs ->
          last_result := 1;
          (String.concat
            ""
            (List.map
               (fun (n, r, s) ->
                 Printf.sprintf "Line %d: %s%s\n"
                   n
                   (match r with
                    | Some rule -> Printf.sprintf "Can't apply %s: " rule
                    | None -> "")
                   s
               )
               errs))
       | Incomplete -> last_result := 2; "No errors but proof is incomplete.\n"
             )
           with
           | Lexer.Error msg ->
              last_result := 1;
              Printf.sprintf "%s\n" msg
           | Types.ParseError (msg, (s, e)) ->
              last_result := 1;
              Printf.sprintf "Syntax Error at %d:%d-%d:%d: %s\n"
                s.pos_lnum
                (s.pos_cnum - s.pos_bol)
                e.pos_lnum
                (e.pos_cnum - e.pos_bol)
                msg
         )
     method rules =
       Js.string (Rules.rules_HTML ())
     method lastResult =
       !last_result
  end)
