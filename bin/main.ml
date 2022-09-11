open Printf

let () = Comp.Input.read ()
        |> List.map (fun (_, y) -> y)
        |> List.iter (printf "%c,");

