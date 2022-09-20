type symbol =
  |Terminal of string
  |Nonterminal of string
  |Epsilon
  |Dollar (* end marker *)

(* first symbol should be a Nonterminal *)
type production = symbol * symbol list

(* Terminals, Nonterminals, Root, Productions *)
type grammar = Grammar of symbol list * symbol list * symbol * production list


let string_of_symbol = function
    |Terminal(t) -> t
    |Nonterminal(nt) -> nt
    |Epsilon -> "epsilon"
    |Dollar -> "$"

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
      |Some(Nonterminal(_) as nt) -> get_productions g nt
        |> List.map (fun prod -> Item(0, prod))
      |None | Some(_) -> []

  in

  let next_closure items = List.fold_left (fun acc item -> match get_next_symbol item with
                      Some(Nonterminal(_)) -> single_item_closure_step item
                        |> union acc
                      |_ -> acc
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
            None -> let set_with_epsilon = SymbolSet.add (Epsilon) SymbolSet.empty in
                    Some(set_with_epsilon)
            |Some(set) -> Some(SymbolSet.add (Epsilon) set)
        ) first_map

        |s::ss ->
        let s_first_set = (
          match SymbolMap.find_opt s first_map with
            None -> SymbolSet.empty
            |Some(set) -> set
        )
        in
        let contains_epsilon = SymbolSet.mem (Epsilon) s_first_set in
        let s_first_set = SymbolSet.remove (Epsilon) s_first_set in
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
      Terminal(_) | Epsilon | Dollar -> SymbolMap.update symbol (function
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

let is_nonterminal = function
  Nonterminal(_) -> true
  |_ -> false

let compute_follow_sets grammar first_map =
  (* Returns the first set for a given string of grammar symbols.
     Returns {epsilon} if the string is empty*)
  let first_of_string first_map symbols =
    let rec helper first_map first_set = function
      [] -> SymbolSet.add (Epsilon) first_set
      |s::ss -> let set = SymbolMap.find s first_map in
                let contains_epsilon = SymbolSet.mem (Epsilon) set in
                let set = SymbolSet.remove (Epsilon) set in
                if contains_epsilon then helper first_map (SymbolSet.union first_set set) ss
                else SymbolSet.union first_set set
    in
    helper first_map SymbolSet.empty symbols
  in

  let follow_set_from_production follow_map prod =
    let rec helper follow_map = function
      |[] -> follow_map
      |s::ss when is_nonterminal s -> let set = first_of_string first_map ss in
      let contains_epsilon = SymbolSet.mem (Epsilon) set in
      let set_without_epsilon = SymbolSet.remove (Epsilon) set in

      let follow_lhs = (
        match SymbolMap.find_opt (production_lhs prod) follow_map with
          None -> SymbolSet.empty
          |Some(set) -> set
        )
      in

      let final_set = if contains_epsilon then SymbolSet.union follow_lhs set_without_epsilon else set_without_epsilon
      in

      helper (SymbolMap.update s (function
       None -> Some(final_set)
       |Some(existing_set) -> Some(SymbolSet.union final_set existing_set)
      ) follow_map) ss

      |_::ss -> helper follow_map ss

    in
    helper follow_map (production_rhs prod)
  in

  let update_follow_sets follow_map =
    List.fold_left (fun map prod ->
      follow_set_from_production map prod
      |> SymbolMap.union (fun _ s1 s2 -> Some(SymbolSet.union s1 s2)) map
    ) follow_map (grammar_productions grammar)

  in

  let rec repeat_until_unchanged follow_map =
    let next_follow_map = update_follow_sets follow_map in
    if equal_first_maps follow_map next_follow_map then next_follow_map
    else repeat_until_unchanged next_follow_map
  in

  let initial_map = SymbolMap.add (grammar_root grammar) (SymbolSet.add Dollar SymbolSet.empty) SymbolMap.empty
  in

  repeat_until_unchanged initial_map

type action =
  |Shift of int (* int represents state - position in canonical collection *)
  |Reduce of int (* int represents the position of the production in the list of productions *)
  (* TODO: change representation of productions to a functional array for fast lookup,
     or store the production in the action *)
  |Accept
  |Error

let string_of_action = function
  Shift(i) -> Printf.sprintf "shift %d" i
  |Reduce(i) -> Printf.sprintf "reduce %d" i
  |Accept -> "accept"
  |Error -> "error"

module ActionMap = Map.Make(struct type t = int * symbol let compare = compare end)

let print_action_map map =
  ActionMap.to_seq map
  |> List.of_seq
  |> List.map (fun ((state, symbol), action ) -> string_of_int state, string_of_symbol symbol, string_of_action action)
  |> List.iter (fun (state, symbol, action) -> Printf.printf "ACTION[%s, %s] = %s\n" state symbol action)

exception Grammar_Not_SLR

let set_with_error_on_conflict map key action =
  ActionMap.update key (
    function
      None -> Some(action)
      |Some(action') when action = action' -> Some(action)
      |_ -> raise Grammar_Not_SLR
  ) map

let get_with_error_default map key =
  match ActionMap.find_opt key map with
    None -> Error
    |Some(action) -> action

let index_of element list =
  List.mapi (fun i ele -> (i, ele)) list
  |> List.find (fun (_, ele) -> ele = element)
  |> fun (i, _) -> i


let compute_action_table grammar collection follow_map =

  let add_shifts action_map =
    List.fold_left(
      fun (i, action_map) item_set ->
        List.fold_left (fun action_map item ->
          match get_next_symbol item with
            Some(Terminal(s)) -> let next_state = index_of (goto item_set (Terminal s) grammar) collection in
            set_with_error_on_conflict action_map (i, (Terminal s)) (Shift next_state)
            |_ -> action_map
        ) action_map item_set
        |> fun map -> (i+1, map)
    ) (0, action_map) collection
    |> fun (_, map) -> map
  in

  let add_reduces action_map =
    List.fold_left(
      fun (i, action_map) item_set ->
        List.fold_left(
          fun action_map (Item(dot, prod)) ->
            match production_lhs prod with
              Nonterminal "S" -> action_map
              |_ -> if dot != List.length (production_rhs prod) then action_map
              else List.fold_left (fun action_map symbol ->
                let index = index_of prod (grammar_productions grammar) in
                set_with_error_on_conflict action_map (i, symbol) (Reduce index)
              ) action_map (SymbolMap.find (production_lhs prod) follow_map |> SymbolSet.to_seq |> List.of_seq)

        ) action_map item_set
        |> fun map -> (i+1, map)
    ) (0, action_map) collection
    |> fun (_, map) -> map
  in

  let end_item = Item(1, (Nonterminal("S"), [Nonterminal("E")]))
  in
  let add_accept action_map =
    List.fold_left (
    fun (i, action_map) item_set ->
      match List.find_opt ((=)end_item) item_set with
        None -> (i+1, action_map)
        |Some(_) -> (i+1, set_with_error_on_conflict action_map (i, Dollar) Accept)
      ) (0, action_map) collection
    |> fun (_, map) -> map
  in

  add_accept ActionMap.empty
  |> add_shifts
  |> add_reduces

let goto_state state symbol grammar collection =
  let current_set = List.nth collection state in
  index_of (goto current_set symbol grammar) collection

exception Parsing_Error
let parse grammar input =
  let collection = canonical_collection grammar |> List.sort compare in
  let follow_map = compute_first_sets grammar
  |> compute_follow_sets grammar in
  let action_table = compute_action_table grammar collection follow_map in


  let rec loop stack input =
    Printf.printf "stack: ";
    List.iter (fun s -> Printf.printf "%d " s) stack;
    print_endline "";
    Printf.printf "input: ";
    List.iter (fun s -> Printf.printf "%s " (string_of_symbol s)) input;
    print_endline "";
    match input with
      [] -> raise Parsing_Error
      |s::ss -> print_endline (string_of_action (get_with_error_default action_table (List.hd stack, s)));
      match get_with_error_default action_table (List.hd stack, s) with
                  Shift(t) -> loop (t::stack) ss
                  |Reduce(t) -> let prod = List.nth (grammar_productions grammar) t in
                                let new_stack = Utils.drop (production_rhs prod |> List.length) stack in
                                let next_state = goto_state (List.hd new_stack) (production_lhs prod) grammar collection in
                                print_production prod;
                                loop (next_state::new_stack) (s::ss)
                  |Accept -> true
                  |Error -> false
  in

  (List.map (fun s -> Terminal s) input) @ [Dollar]
  |> loop [0]