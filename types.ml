exception ParseError of string * (Lexing.position * Lexing.position)

type var = string
type id = string
type exvar = int

type prop = True
          | False
          | Not of prop
          | Atom of string * var list
          | And of prop * prop
          | Or of prop * prop
          | Cond of prop * prop
          | Bicond of prop * prop
          | Forall of var * prop
          | Exists of var * prop

type exp = Assumption
         | App of id * exvar list
                    
type line = int * prop * exp

type proof = prop * prop * line list

let rec leq f l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (a::l1, b::l2) -> f a b && leq f l1 l2
  | _ -> false
           
let rec peq p1 p2 =
  match (p1, p2) with
  | (True, True) -> true
  | (False, False) -> true
  | (Not p1, Not p2) -> peq p1 p2
  | (Atom (a1, vs1), Atom (a2, vs2)) ->
     a1 = a2 && leq (=) vs1 vs2
  | (And (p1a, p1b), And (p2a, p2b)) -> peq p1a p2a && peq p1b p2b
  | (Or (p1a, p1b), Or (p2a, p2b)) -> peq p1a p2a && peq p1b p2b
  | (Cond (p1a, p1b), Cond (p2a, p2b)) -> peq p1a p2a && peq p1b p2b
  | (Bicond (p1a, p1b), Bicond (p2a, p2b)) -> peq p1a p2a && peq p1b p2b
  | (Forall (x1, p1), Forall (x2, p2)) -> x1 = x2 && peq p1 p2
  | (Exists (x1, p1), Exists (x2, p2)) -> x1 = x2 && peq p1 p2
  | _ -> false

let rec string_of_prop p =
  let sp = string_of_prop in
  match p with
  | True -> "T"
  | False -> "F"
  | Not p -> Printf.sprintf "~%s" (sp p)
  | Atom (s, []) -> s
  | Atom (s, vs) -> Printf.sprintf "%s(%s)"
                      s
                      (String.concat ", " vs)
  | And (p1, p2) -> Printf.sprintf "(%s /\\ %s)" (sp p1) (sp p2)
  | Or (p1, p2) -> Printf.sprintf "(%s \\/ %s)" (sp p1) (sp p2)
  | Cond (p1, p2) -> Printf.sprintf "(%s -> %s)" (sp p1) (sp p2)
  | Bicond (p1, p2) -> Printf.sprintf "(%s <-> %s)" (sp p1) (sp p2)
  | Forall (x, p) -> Printf.sprintf "(Forall %s. %s)" x (sp p)
  | Exists (x, p) -> Printf.sprintf "(Exists %s. %s)" x (sp p)      
