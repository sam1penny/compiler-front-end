let read () =
  let f = open_in "./test/test.kl" in
  let rec get_input line acc =
    try
      match input_char f with
        | '\n' -> let new_line = line + 1 in
                  get_input new_line ((new_line, '\n') :: acc)
        | c -> get_input line ((line, c) :: acc)
    with
    | End_of_file -> List.rev acc
  in
  get_input 1 []