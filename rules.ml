open Types

type result = OK
            | NotOK of string

type rule = prop list -> prop -> result

let rec simplify ps p =
  match ps with
  | [And (p1, p2)] ->
     if peq p1 p || peq p2 p then OK
     else
       (match simplify [p1] p with
        | OK -> OK
        | _ ->
           (match simplify [p2] p with
            | OK -> OK
            | _ -> NotOK (Printf.sprintf "%s not found in %s"
                            (string_of_prop p)
                            (string_of_prop (And (p1, p2))))
           )
       )
  | _ -> NotOK "not a conjunction"

let notse p1 p2 =
  NotOK (Printf.sprintf "%s not syntactically equal to %s"
                  (string_of_prop p1)
                  (string_of_prop p2))
let reqse p1 p2 =
  if peq p1 p2 then OK
  else notse p1 p2
  
let rec modusponens ps p =
  match ps with
  | [Cond (pa, pb); pa'] ->
     if peq pa' pa then
       if peq pb p then
         OK
       else notse p pb
     else notse pa pa'
  | [Bicond (pa, pb); pa'] ->
     if peq pa' pa then
       if peq pb p then
         OK
       else notse p pb
     else
       if peq pb pa' then
         if peq pa p then
           OK
         else notse p pa
       else
         NotOK (Printf.sprintf "%s doesn't match either prop of biconditional"
                  (string_of_prop p))
  | _ -> NotOK "not a conditional or biconditional"

let conj ps p =
  match ps with
  | [p1; p2] ->
     reqse p (And (p1, p2))
  | _ -> NotOK ""

let disj ps p =
  match (p, ps) with
  | (Or (p1, p2), [p]) ->
     if peq p1 p then OK
     else if peq p2 p then OK
     else NotOK (Printf.sprintf "%s not syntactically equal to either disjunct"
                   (string_of_prop p))
  | _ -> NotOK "not a disjunction"

let bind f g =
  match f () with
  | OK -> OK
  | NotOK _ -> g ()

let biimp f ps p =
  match ps with
  | [p1] ->
     bind (fun () -> f p1 p)
       (fun () -> f p p1)
  | _ -> NotOK ""
       
let defcond =
  biimp (fun p1 p2 ->
      match (p1, p2) with
      | (Cond (p1, p2), Or (Not p1', p2'))
           when peq p1 p1' && peq p2 p2' -> OK
      | _ -> NotOK "doesn't apply")

let defbicond =
  biimp
    (fun p1 p2 ->
      match (p1, p2) with
      | (Bicond (p1, p2), And (Cond (p1', p2'), Cond (p2'', p1'')))
           when peq p1 p1' && peq p1 p1'' && peq p2 p2' && peq p2 p2''
        -> OK
      | (Bicond (p1, p2), And (Cond (p2'', p1''), Cond (p1', p2')))
           when peq p1 p1' && peq p1 p1'' && peq p2 p2' && peq p2 p2''
        -> OK
      | _ -> NotOK "doesn't apply"
    )

let dne =
  biimp
    (fun p1 p2 ->
      match (p1, p2) with
      | (Not (Not p1), p1') when peq p1 p1' -> OK
      | _ -> NotOK (Printf.sprintf "doesn't apply to %s and %s"
                      (string_of_prop p1)
                      (string_of_prop p2))
    )

let lem =
  biimp
    (fun p1 p2 ->
      match (p1, p2) with
      | (True, (Or (p, Not p'))) when peq p p' -> OK
      | (True, (Or (Not p', p))) when peq p p' -> OK
      | _ -> NotOK "doesn't apply")

let contra =
  biimp
    (fun p1 p2 ->
      match (p1, p2) with
      | (False, (And (p, Not p'))) when peq p p' -> OK
      | (False, (And (Not p', p))) when peq p p' -> OK
      | _ -> NotOK "doesn't apply"
    )

let id =
  biimp
    (fun p1 p2 ->
      match (p1, p2) with
      | (And (p1, True), p1') when peq p1 p1' -> OK
      | (And (True, p1), p1') when peq p1 p1' -> OK
      | (Or (p1, False), p1') when peq p1 p1' -> OK
      | (Or (False, p1), p1') when peq p1 p1' -> OK
      | _ -> NotOK "doesn't apply")

let demorgan =
  biimp
    (fun p1 p2 ->
      (match (p1, p2) with
       | (Not (And (p1, q1)), Or (Not p2, Not q2))
            when peq p1 p2 && peq q1 q2 -> OK
       | (Not (Or (p1, q1)), And (Not p2, Not q2))
            when peq p1 p2 && peq q1 q2 -> OK
       | (Not (Forall (x, p)), Exists (x', Not p'))
            when x = x' && peq p p' -> OK
       | (Not (Exists (x, p)), Forall (x', Not p'))
            when x = x' && peq p p' -> OK
       | _ -> NotOK "doesn't apply"))

let dist =
  biimp
    (fun p1 p2 ->
      (match (p1, p2) with
       | (And (Or (p1, q1), r1), Or (And (p2, r2), And (q2, r3)))
            when peq p1 p2 && peq q1 q2 && peq r1 r2 && peq r1 r3
         -> OK
       | (Or (And (p1, q1), r1), And (Or (p2, r2), Or (q2, r3)))
            when peq p1 p2 && peq q1 q2 && peq r1 r2 && peq r1 r3
         -> OK
       | _ -> NotOK "doesn't apply"
      )
    )

let comm =
  biimp
    (fun p1 p2 ->
      match (p1, p2) with
      | (And (p1, q1), And (q2, p2))
           when peq p1 p2 && peq q1 q1 -> OK
      | (Or (p1, q1), Or (q2, p2))
           when peq p1 p2 && peq q1 q1 -> OK
      | _ -> NotOK "doesn't apply")

let assoc =
  biimp
    (fun p1 p2 ->
      match p1, p2 with
      | And (p1, And (q1, r1)), And (And (p2, q2), r2)
        | Or (p1, Or (q1, r1)), Or (Or (p2, q2), r2) ->
         if peq p1 p2 && peq q1 q2 && peq r1 r2 then OK
         else NotOK "doesn't apply"
      | _ -> NotOK "doesn't apply"
    )

let idemp =
  biimp
    (fun p1 p2 ->
      match p1, p2 with
      | And (p1, p2), p3 when peq p1 p2 && peq p1 p3 -> OK
      | Or (p1, p2), p3 when peq p1 p2 && peq p1 p3 -> OK
      | _ -> NotOK "doesn't apply"
    )

(*
let comm_assoc =
  let rec in_and big_p small_p  =
    match big_p with
    | And (p1, p2) -> (in_and p1 small_p) || (in_and p2 small_p)
    | _ -> peq big_p small_p 
  in
  let rec in_or big_p small_p =
    match big_p with
    | Or (p1, p2) -> (in_or p1 small_p) || (in_or p2 small_p)
    | _ -> peq big_p small_p
  in
  let rec forall_and f p =
    match p with
    | And (p1, p2) -> (forall_and f p1) && (forall_and f p2)
    | _ -> f p
  in
  let rec forall_or f p =
    match p with
    | Or (p1, p2) -> (forall_or f p1) && (forall_or f p2)
    | _ -> f p
  in
  biimp
  (fun p1 p2 ->
    if
      ((forall_and (in_and p1) p2) && (forall_and (in_and p2) p1))
      || ((forall_or (in_or p1) p2) && (forall_or (in_or p2) p1))
    then OK
    else NotOK "doesn't apply")
 *)

let dom =
  biimp
    (fun p1 p2 ->
      match (p1, p2) with
      | (Or (p, True), True)
        | (Or (True, p), True)
        | (And (p, False), False)
        | (And (False, p), False) ->
         OK
      | _ -> NotOK "doesn't apply"
    )

let rec deep rule ps p =
  match rule ps p with
  | OK -> OK
  | _ ->
     (match (ps, p) with
      | ([Not p1], Not p) -> deep rule [p1] p
      | ([Forall (x1, p1)], Forall (x, p))
        | ([Exists (x1, p1)], Exists (x, p))
        ->
         if x1 = x then deep rule [p1] p
         else
           NotOK "variables don't match"
      | ([And (p1, p2)], And (p1', p2'))
        | ([Or (p1, p2)], Or (p1', p2'))
        | ([Cond (p1, p2)], Cond (p1', p2'))
        | ([Bicond (p1, p2)], Bicond (p1', p2')) ->
         if peq p1 p1' then
           deep rule [p2] p2'
         else if peq p2 p2' then
           deep rule [p1] p1'
         else
           NotOK "shapes don't match"
      | _ -> NotOK "")

let rec partdeep rule ps p =
  match rule ps p with
  | OK -> OK
  | _ ->
     (match (ps, p) with
      | ([And (p1, p2)], And (p1', p2'))
        | ([Or (p1, p2)], Or (p1', p2')) ->
         if peq p1 p1' then
           partdeep rule [p2] p2'
         else if peq p2 p2' then
           partdeep rule [p1] p1'
         else
           NotOK "shapes don't match"
      | _ -> NotOK "")
    
let rules =
  [(["Simplify"; "Simplification"; "S"], 1, partdeep simplify, "p /\\ q => p, q");
   (["ModusPonens"; "MP"], 2, partdeep modusponens, "(p -> q), p => q");
   (["Conjunction"; "Addition"; "Conj"], 2, partdeep conj, "p, q => p /\\ q");
   (["Disjunction"; "Disj"], 1, partdeep disj, "p => p \\/ q, q \\/ p");
   (["DefinitionC"; "DC"], 1, deep defcond, "p -> q <=> ~p \\/ q");
   (["DefinitionBC"; "DBC"], 1, deep defbicond, "p <-> q <=> (p -> q) /\\ (q -> p)");
   (["ExcludedMiddle"; "LEM"], 1, deep lem, "p \\/ ~p <=> T");
   (["DoubleNegationElimination"; "DoubleNegation"; "DNE"], 1, deep dne, "p <=> ~~p");
   (["Contradiction"; "Cont"], 1, deep contra, "p /\\ ~p <=> F");
   (["Identity"; "Id"; "ID"], 1, deep id, "p /\\ T => p<br />p \\/ F => p");
   (["DeMorgan"; "DM"], 1, deep demorgan, "~(p /\\ q) <=> ~p \\/ ~q<br />~(p \\/ q) <=> ~p /\\ ~q<br />~(Forall x. p(x)) <=> Exists x. ~p(x)<br />~(Exists x. p(x)) <=> Forall x. ~p(x)");
   (["Distributivity"; "Dist"], 1, deep dist, "(p /\\ q) \\/ R <=> (p \\/ R) /\\ (q \\/ R)<br />(p \\/ q) /\\ R <=> (p /\\ R) \\/ (q /\\ R)");
   (["Commutativity"; "Comm"], 1, deep comm, "p /\\ q <=> q /\\ p<br />p \\/ q <=> q \\/ p");
   (* (["CommAssocIdem"; "CommAssoc"; "CA"; "CAI"], 1, deep comm_assoc, "Applies commutativity, associativity and<br />idempotence as many times as needed"); *)
   (["Associativity"; "Assoc"], 1, deep assoc, "p /\\ (q /\\ r) <=> (p /\\ q) /\\ r<br />p \\/ (q \\/ r) <=> (p \\/ q) \\/ r");
   (["Idempotency"; "Idem"], 1, deep idemp, "p /\\ p <=> p, p \\/ p <=> p");
   (["Domination"; "Dom"], 1, deep dom, "p \\/ T <=> T<br />p /\\ F <=> F");
  ]
          
let lookup_rule s =
  let rec lr rules =
    match rules with
    | [] -> None
    | (names, args, f, _)::rules ->
       if List.mem s names then Some (args, f) else lr rules
  in lr rules

let rules_HTML () =
  let rule_HTML (names, _, _, desc) =
    let mainname = try List.hd names with _ -> "" in
    let abbrv =
      if List.length names > 1 then
        List.nth names ((List.length names) - 1)
      else ""
    in
    "<tr><td>" ^ mainname ^ "</td><td>" ^ abbrv ^ "</td><td>"
    ^ desc ^ "</td></tr>"
  in
  "<table border=0 cellspacing=10>"
  ^ "<tr><td><b>Name</b></td><td><b>Abbrev</b></td><td><b>Description</b></td></tr>"
  ^ String.concat "\n" (List.map rule_HTML rules)
  ^ "</table>"
