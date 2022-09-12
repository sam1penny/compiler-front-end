type state = LexerState of (Regex.regex * (char list -> state)) list

type token =
  | PLUS
  | MINUS
  | CARAT
  | COS
  | EXCLAMATION_MARK
  | NUM of string
  | EOF

let string_of_op = function
  | PLUS -> "[+]"
  | MINUS -> "[-]"
  | CARAT -> "^"
  | COS -> "[cos]"
  | EXCLAMATION_MARK -> "[!]"
  | NUM(s) -> Printf.sprintf "[%s]" s
  | EOF -> "EOF"

type rule = Regex.regex * (char list -> token)

let digit = Regex.union_list "0123456789"
let plusdigit = Regex.union_list "123456789"
let integer = Regex.concat plusdigit (Regex.Star digit)

let prefix = Regex.union (Regex.Character('0')) integer

let optplus = Regex.zero_or_one (Regex.union_list "+-")
let optexp = Regex.zero_or_one (Regex.concat (Regex.Character('e')) (Regex.concat optplus integer))

let floating = Regex.union integer (Regex.union
      (Regex.concat (Regex.Character('.')) (Regex.one_or_more digit))
      (Regex.concat prefix (Regex.concat (Regex.Character('.')) (Regex.Star digit)))
      )

let number = Regex.concat optplus (Regex.concat floating optexp)

let to_string clist = List.to_seq clist |> String.of_seq;;

let rules : rule list = [
  Regex.Character('+'), (fun _ -> PLUS) ;
  Regex.Character('-'), (fun _ -> MINUS) ;
  Regex.Character('^'), (fun _ -> CARAT) ;
  Regex.concat_list "cos", (fun _ -> COS) ;
  Regex.Character('!'), (fun _ -> EXCLAMATION_MARK) ;
  number, (fun x -> NUM(to_string x)) ;
  Regex.Character('\004'), ( fun _ -> EOF) ;
]

(* get all regular expressions that are not in the dead state -
   i.e they can still be matched *)
let next_state state c =
  List.map (fun (r, a) -> (Regex.derive r c, a)) state
  |> List.filter (fun (r, _) -> not (Regex.dead r))

(* get all currently matched regular expressions *)
let matched_rules = List.filter (fun (r, _) -> Regex.nullable r)

exception No_Match

(*
let get_token stream =
  let rec loop state stream lexeme last_match =
    match stream with
      [] -> last_match, stream
      |(_, c)::tail ->
        let state = next_state state c in
        match state with
          [] -> last_match, stream
          |_ -> match matched_rules state with
            [] -> loop state tail (c::lexeme) last_match
            |(_, action)::_ -> loop state tail (c::lexeme) (Some(action (c::lexeme)))
      in
  loop rules stream [] None
*)

let get_token stream =
  let rec get_token_inner state stream lexeme last_match =
    match stream with
      [] -> last_match
      |(_, c)::rest ->
        let state = next_state state c in
        match state with
          [] -> last_match
          |_ -> match matched_rules state with
            [] -> get_token_inner state rest (c::lexeme) last_match
            |(_, action)::_ -> get_token_inner state rest (c::lexeme) (Some(action, c::lexeme, stream))
      in
  match get_token_inner rules stream [] None with
      None -> raise No_Match
      |Some(action, lexeme, stream) -> (action (List.rev lexeme)), stream

let rec get_all_tokens stream =
  match stream with
    [] -> []
    |_ -> match get_token stream with
      |t, [] -> [t]
      |t, _::tail -> t :: get_all_tokens tail

(*
let rec get_all_tokens stream =
  match get_token stream with
    (Some(t), []) -> [t]
    |(None, []) -> []
    |(Some(t), hd::tl) -> t :: get_all_tokens (hd::tl)
    |(None, _::tl) -> get_all_tokens (tl)
*)
