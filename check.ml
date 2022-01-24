open Rules
open Types
   
type check_error = int * string option * string
   
type check_result = Good
                  | Problems of check_error list
                  | Incomplete

module IMap = Map.Make(struct type t = int
                              let compare = compare
                       end)

exception CheckError of string
            
let check_line ctx last p (rule, exps) : string option =
  let lookup_line l =
    match IMap.find_opt l ctx with
    | Some p -> p
    | None -> raise (CheckError (Printf.sprintf "Line %d doesn't exist" l))
  in
  try
    let (nargs, f) =
      match lookup_rule rule with
      | Some r -> r
      | None -> raise (CheckError "Rule not recognized")
    in
    let args =
      if List.length exps = nargs then
        List.map lookup_line exps
      else if nargs = 1 && List.length exps = 0 then [last]
      else
        raise (CheckError "Wrong number of arguments")
    in
    match f args p with
    | OK -> None
    | NotOK s -> Some s
  with CheckError s -> Some s
            
let rec check_proof ctx assumption goal lines last status =
  let add_prob e =
    match status with
    | Good -> Good
    | Problems l -> Problems (e::l)
    | Incomplete -> Problems [e]
  in
  match lines with
  | [] -> status
  | (n, p, Assumption)::lines ->
     if peq p assumption then
       check_proof (IMap.add n p ctx) assumption goal lines p status
     else
       check_proof (IMap.add n p ctx) assumption goal lines p
         (add_prob (n, None, "Does not match the assumption of the proof."))
  | (n, p, App (rule, exps))::lines ->
     (match check_line ctx last p (rule, exps) with
      | None ->
         if peq p goal then
           (match status with
            | Incomplete -> Good
            | _ -> status)
         else
           check_proof (IMap.add n p ctx) assumption goal lines p status
      | Some s ->
         check_proof (IMap.add n p ctx) assumption goal lines p
           (add_prob (n, Some rule, s))
     )

let check_full_proof (assumption, goal, lines) =
  check_proof IMap.empty assumption goal lines assumption Incomplete
