type symbol =
  |Terminal of string
  |Nonterminal of string

(* first symbol should be a Nonterminal *)
type production = symbol * symbol list

(* Terminals, Nonterminals, Root, Productions *)
type grammar = Grammar of symbol list * symbol list * symbol * production list


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

let print_production (head, body) =
  string_of_symbol head
  |> Printf.printf "%s";
  Printf.printf " -> ";
  List.map string_of_symbol body
  |> List.iter (Printf.printf "%s ");
  print_endline ""


let test_grammar = Grammar (
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
  ])

let production_lhs (head, _) = head
let production_rhs (_, body) = body

let grammar_terminals (Grammar (t, _, _, _)) = t
let grammar_nonterminals (Grammar (_, nt, _, _)) = nt
let grammar_root (Grammar(_, _, r, _)) = r
let grammar_productions (Grammar (_, _, _, p)) = p


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

let equal_collections c1 c2 =
  let rec is_subset = function
    |[], _ -> true
    |c::cs, item_set -> List.exists (fun c' -> equal_item_sets c c') c2 && is_subset (cs, item_set)
  in
  is_subset (c1, c2) && List.length c1 = List.length c2

let get_productions grammar nt =
  grammar_productions grammar
  |> List.filter(fun (prod_head, _) -> prod_head = nt)

let closure g items =
  let single_item_closure_step item =
    match get_next_symbol item with
      None | Some(Terminal(_)) -> []
      |Some(nt) -> get_productions g nt
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

let canonical_collection grammar =
  let initial_items = [Item(0, (Nonterminal("S"), [Nonterminal("E")]))] |> closure grammar in
  let c = [initial_items] in
  let symbols = grammar_nonterminals grammar @ grammar_terminals grammar in
  (* acc is collection *)
  let goto_from_item_set collection item_set = List.fold_left (fun acc symbol ->
    match goto item_set symbol grammar with
      [] -> acc
      |goto_item_set -> if List.exists (fun iset -> equal_item_sets iset goto_item_set) acc then acc else (goto_item_set::acc)
  ) collection symbols
  in

  let update_collection_loop collection = List.fold_left goto_from_item_set collection collection
  in

  let rec repeat_until_unchanged collection =
    let next_collection = update_collection_loop collection in
    if equal_collections next_collection collection then next_collection
    else repeat_until_unchanged next_collection
  in

  repeat_until_unchanged c


module SymbolSet = Set.Make(struct type t = symbol let compare = compare end)
let rec first_set grammar symbol =
  let prod_first acc prod =
    let rec inner acc = function
      [] -> SymbolSet.add (Terminal "epsilon") acc
      |s::ss -> if s = (production_lhs prod) then acc
      else
      let s_first = first_set grammar s in
      if SymbolSet.mem (Terminal "epsilon") s_first then inner (SymbolSet.union acc s_first) ss
      else (SymbolSet.union acc s_first)
    in
    inner acc (production_rhs prod)
  in

  match symbol with
    Terminal(_) -> SymbolSet.add symbol SymbolSet.empty
    |Nonterminal(_) -> List.fold_left(fun acc prod ->
      prod_first acc prod
      |> SymbolSet.union acc
    ) SymbolSet.empty (get_productions grammar symbol)

let first_list grammar symbol =
  first_set grammar symbol
  |> SymbolSet.to_seq
  |> List.of_seq
