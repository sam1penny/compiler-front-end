open Printf
open Comp


let () =
  Input.read ()
  |> Lexer.get_all_tokens
  |> List.map (fun t -> Lexer.string_of_op t)
  |> List.iter (printf "%s")


