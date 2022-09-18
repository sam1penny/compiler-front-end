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
module SymbolMap = Map.Make(struct type t = symbol let compare = compare end)

let first_map_to_list map = SymbolMap.to_seq map
|> List.of_seq
|> List.map (fun (symbol, first_set) -> (symbol, SymbolSet.to_seq first_set |> List.of_seq))

let print_first_map map =
  first_map_to_list map
  |> List.iter (fun (s, slist) ->
      string_of_symbol s
      |> Printf.printf "%s : [ ";
      List.map (fun s -> string_of_symbol s) slist
      |> List.iter (Printf.printf "%s ");
      print_endline "]"
    )

let equal_first_maps m1 m2 = SymbolMap.equal (fun s1 s2 -> SymbolSet.equal s1 s2) m1 m2

let compute_first_sets grammar =

  let first_set_of_symbol symbol first_map =
    let first_set_from_production first_map prod =
      let rec helper first_map = function
        [] -> SymbolMap.update (production_lhs prod) (
          function
            None -> let set_with_epsilon = SymbolSet.add (Terminal "epsilon") SymbolSet.empty  in
                    Some(set_with_epsilon)
            |Some(set) -> Some(SymbolSet.add (Terminal "epsilon") set)
        ) first_map

        |s::ss ->
        let s_first_set = (
          match SymbolMap.find_opt s first_map with
            None -> SymbolSet.empty
            |Some(set) -> set
        )
        in
        let contains_epsilon = SymbolSet.mem (Terminal "epsilon") s_first_set in
        let s_first_set = SymbolSet.remove (Terminal "epsilon") s_first_set in
        let new_first_map = SymbolMap.update (production_lhs prod) (
          fun opt_set -> match opt_set with
            None -> Some(s_first_set)
            |Some(set) -> Some(SymbolSet.union set s_first_set)
        ) first_map
        in
        if contains_epsilon then helper new_first_map ss
        else new_first_map
      in
      helper first_map (production_rhs prod)
    in

    match symbol with
      Terminal(_) -> SymbolMap.update symbol (function
        None -> Some(SymbolSet.add symbol SymbolSet.empty)
        |Some(s) -> Some(s)
      ) first_map
      |Nonterminal(_) -> List.fold_left(fun acc prod ->
        first_set_from_production acc prod
        |> SymbolMap.union (fun _ s1 s2 -> Some(SymbolSet.union s1 s2)) acc
      ) first_map (get_productions grammar symbol)

  in

  let update_first_sets first_map =
    List.fold_left (fun first_map symbol ->
      first_set_of_symbol symbol first_map
      |> SymbolMap.union (fun _ s1 s2 -> Some(SymbolSet.union s1 s2)) first_map
    ) first_map (grammar_terminals grammar @ grammar_nonterminals grammar)
  in


  let rec repeat_until_unchanged first_map =
    let next_first_map = update_first_sets first_map in
    if equal_first_maps first_map next_first_map then next_first_map
    else repeat_until_unchanged next_first_map

  in

  repeat_until_unchanged SymbolMap.empty

let epsilon_grammar = Grammar (
  [Terminal("+"); Terminal("epsilon")],
  [Nonterminal("E"); Nonterminal("T")],
  Nonterminal("S"),
  [
    (Nonterminal "S", [Nonterminal "E"]);
    (Nonterminal "T", [Nonterminal "E"; Terminal "+"]);
    (Nonterminal "E", [Nonterminal "E"; Terminal "+"; Nonterminal "E"]);
    (Nonterminal "E", [Terminal "epsilon"]);
  ]
)