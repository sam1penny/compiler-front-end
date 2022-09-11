type regex =
  EmptySet
  |EmptyString
  |Character of char
  |Union of regex * regex
  |Concat of regex * regex
  |Star of regex


let union r1 r2 =
  match r1, r2 with
    |EmptySet, _ -> r2
    |_, EmptySet -> r1
    |_, _ -> Union(r1, r2)


let concat r1 r2 =
  match r1, r2 with
    EmptySet, _ -> r1
    |_, EmptySet -> r2
    |EmptyString, _ -> r2
    |_, EmptyString -> r1
    |_, _ -> Concat(r1, r2)

let rec nullable = function
  EmptySet -> false
  |EmptyString -> true
  |Character(_) -> false
  |Union(r1, r2) -> (nullable r1) || (nullable r2)
  |Concat(r1, r2) -> (nullable r1) && (nullable r2)
  |Star(_) -> true

let rec dead = function
  EmptySet -> true
  |EmptyString -> false
  |Character(_) -> false
  |Union(r1, r2) -> (dead r1) && (dead r2)
  |Concat(r1, r2) -> (dead r1) || (dead r2)
  |Star(_) -> false

let rec derive r c =
  match r with
    EmptySet -> EmptySet
    |EmptyString -> EmptySet
    |Character(c') -> if c = c' then EmptyString else EmptySet
    |Union(r1, r2) -> union (derive r1 c) (derive r2 c)
    |Concat(r1, r2) -> let x = if nullable r1 then EmptyString else EmptySet in
        union (concat (derive r1 c) r2) (concat x (derive r2 c))
    |Star(r) -> concat (derive r c)  (Star(r))


(* extended syntax *)

let zero_or_one = union EmptyString
let one_or_more r = concat r (Star(r))

(* helpers *)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

let union_list s = explode s
  |> List.fold_left (fun acc c -> union (Character(c)) acc) EmptySet

let concat_list s = List.fold_right (fun c acc -> concat (Character(c)) acc) (explode s) EmptyString

(* match function *)

let regexmatch s r =
  explode s
  |> List.fold_left derive r
  |> nullable