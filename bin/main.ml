open Comp


(* Lexer testing
let () =
  Input.read ()
  |> Lexer.get_all_tokens
  |> List.map (fun (line, t) -> sprintf "%d, %s" line (Lexer.string_of_token t))
  |> List.iter (printf "[%s]")
*)

let () =
  print_endline "--- Grammar --- ";
  Parser.grammar_productions Parser.test_grammar
  |> List.iter Parser.print_production;
