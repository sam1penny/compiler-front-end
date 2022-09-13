open Printf
open Comp


let () =
  Input.read ()
  |> Lexer.get_all_tokens
  |> List.map (fun (line, t) -> sprintf "%d, %s" line (Lexer.string_of_token t))
  |> List.iter (printf "[%s]")


