open Compiler


(* Lexer testing
let () =
  Input.read "./resources/test.kl"
  |> Lexer.get_all_tokens
  |> List.map (fun (line, t) -> Printf.sprintf "%d, %s" line (Lexer.string_of_token t))
  |> List.iter (Printf.printf "[%s]");
  print_endline ""
*)

let () =
  print_endline "--- Grammar --- ";
  Parser.grammar_productions Parser.test_grammar
  |> List.iter Parser.print_production;
  print_endline "--- Canonical Collection";
  Parser.canonical_collection Parser.test_grammar
  |> List.sort compare
  |> List.iteri (fun i l -> Printf.printf "- item set %d: -\n" i; List.iter Parser.print_item l;);
  print_endline "";
  print_endline "--- Action ---";
  let follow_set = Parser.compute_first_sets Parser.test_grammar
  |> Parser.compute_follow_sets Parser.test_grammar in
  let col = Parser.canonical_collection Parser.test_grammar
  |> List.sort compare in
  Parser.compute_action_table Parser.test_grammar col follow_set
  |> Parser.print_action_map;
  print_endline "--- PARSING ---";
  Input.read "./resources/test.kl"
  |> Lexer.get_all_tokens
  |> Parser.parse Parser.test_grammar
  |> Parser.tree_to_string
  |> print_endline;
  print_endline "-- END --"