let rec take i = function
  | [] -> []
  | x::xs ->
    if i > 0 then x :: take (i - 1) xs
    else []

let rec drop i = function
  | [] -> []
  | x::xs ->
    if i > 0 then drop (i-1) xs
    else x::xs

let chars_to_string clist = List.to_seq clist |> String.of_seq;;