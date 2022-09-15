open Comp


(* Lexer testing
let () =
  Input.read ()
  |> Lexer.get_all_tokens
  |> List.map (fun (line, t) -> sprintf "%d, %s" line (Lexer.string_of_token t))
  |> List.iter (printf "[%s]")
*)



(* Parser testing *)

let root_prod = (Parser.Nonterminal("S"), [Parser.Nonterminal("E")])
let initial_item = Parser.Item(0, root_prod)

let add_prod = (Parser.Nonterminal("E"), [Parser.Nonterminal("E"); Parser.Terminal("+"); Parser.Nonterminal("T")])
let test_items = [Parser.Item(1, root_prod); Parser.Item(1, add_prod)]

let () =
  print_endline "--- Closure of root production ---";
  let start_state = Parser.closure Parser.test_grammar [initial_item] in
  List.iter Parser.print_item start_state;
  print_endline "--- Goto ---";
  Parser.goto test_items (Terminal "+") Parser.test_grammar
  |> List.iter Parser.print_item;
  print_endline "--- Closure of Set ---";
  Parser.closure Parser.test_grammar [Parser.Item(2, add_prod)]
  |> List.iter Parser.print_item;
  print_endline "--- Canonical Collection ---";
  Parser.canonical_collection Parser.test_grammar
  |> List.iter (fun l -> print_endline "- item set: -"; List.iter Parser.print_item l;);
