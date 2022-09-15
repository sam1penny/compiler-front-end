type symbol =
  |Terminal of string
  |Nonterminal of string

(* first symbol should be a Nonterminal *)
type production = symbol * symbol list

(* Terminals, Nonterminals, Root, Productions *)
type grammar = symbol list * symbol list * symbol * production list


let string_of_symbol = function
    |Terminal(t) -> t
    |Nonterminal(nt) -> nt

(* the integer is to the left of a symbol in the production. E.g 0, E, [E; +; E] represents E -> .E+E*)
type item = Item of int * production

let print_item (Item(dot, (nt, tlist))) =
  string_of_symbol nt
  |> Printf.printf "%s";
  Printf.printf " -> ";
  Utils.take dot tlist
  |> List.map string_of_symbol
  |> List.iter (Printf.printf "%s ");
  Printf.printf ". ";
  Utils.drop dot tlist
  |> List.map string_of_symbol
  |> List.iter (Printf.printf "%s ");
  print_endline ""


let test_grammar : grammar =
  [Terminal("+"); Terminal("*"); Terminal("num"); Terminal("("); Terminal(")")],
  [Nonterminal("E"); Nonterminal("T"); Nonterminal("F")],
  Nonterminal("S"),
  [
  (Nonterminal("S"), [Nonterminal("E")]);
  (Nonterminal("E"), [Nonterminal("E"); Terminal("+"); Nonterminal("T")]);
  (Nonterminal("E"), [Nonterminal("T")]);
  (Nonterminal("T"), [Nonterminal("T"); Terminal("*"); Nonterminal("F")]);
  (Nonterminal("T"), [Nonterminal("F")]);
  (Nonterminal("F"), [Terminal("("); Nonterminal("E"); Terminal(")")]);
  (Nonterminal("F"), [Terminal("num")]);
  ]

let grammar_productions (_, _, _, p) = p


let get_next_symbol (Item(dot, (_, symbols))) = List.nth_opt symbols dot


let increment_item_dot (Item(dot, prod)) = Item(dot + 1, prod)

(* old list union
let rec union i1 i2 = match i1 with
  [] -> i2
  |i::is -> if List.mem i i2 then union is i2
            else union is (i::i2)
*)

let union i1 i2 =
  let module S = Set.Make(struct type t = item let compare = compare end) in
  List.fold_left (fun set item ->
    if S.mem item set then set
    else S.add item set
  ) (S.of_list i2) i1
  |> S.to_seq
  |> List.of_seq

let equal_item_sets i1 i2 =
  let rec is_subset = function
    |[], _ -> true
    |i::is, item_set -> List.mem i i2 && is_subset (is,item_set)
  in
  is_subset (i1, i2) && List.length i1 = List.length i2

let closure (g : grammar) items =
  let single_item_closure_step item =
    match get_next_symbol item with
      None | Some(Terminal(_)) -> []
      |Some(nt) -> grammar_productions g
        |> List.filter (fun (prod_head, _) -> prod_head = nt)
        |> List.map (fun prod -> Item(0, prod))

  in

  let next_closure items = List.fold_left (fun acc item -> match get_next_symbol item with
                      None | Some(Terminal(_)) -> acc
                      |Some(Nonterminal(_)) -> single_item_closure_step item
                        |> union acc
                        )
                        items items
  in

  let rec repeat_until_unchanged items =
    let next_item_set = next_closure items in
    if equal_item_sets items next_item_set then next_item_set
    else repeat_until_unchanged next_item_set
  in

  repeat_until_unchanged items


let goto items symbol grammar =
  List.filter (
    fun i -> match get_next_symbol i with
      |None -> false
      |Some(nt) -> nt = symbol
  ) items
  |> List.map increment_item_dot
  |> closure grammar