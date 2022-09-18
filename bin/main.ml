open Comp

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
