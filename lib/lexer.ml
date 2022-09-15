type state = LexerState of (Regex.regex * (char list -> state)) list

type token =
  | PLUS
  | MINUS
  | CARAT
  | COS
  | EXCLAMATION_MARK
  | NUM of string
  | WS
  | EOF

let string_of_token = function
  | PLUS -> "+"
  | MINUS -> "-"
  | CARAT -> "^"
  | COS -> "cos"
  | EXCLAMATION_MARK -> "!"
  | NUM(s) -> Printf.sprintf "%s" s
  | WS -> "WS"
  | EOF -> "EOF"

type rule = Regex.regex * (char list -> token)

let digit = Regex.union_string "0123456789"
let plusdigit = Regex.union_string "123456789"
let integer = Regex.concat plusdigit (Regex.Star digit)

let prefix = Regex.union (Regex.Character('0')) integer

let optplus = Regex.zero_or_one (Regex.union_string "+-")
let optexp = Regex.zero_or_one (Regex.concat_list [Regex.Character('e'); optplus; integer])

let floating = Regex.union_list [
  integer;
  Regex.concat (Regex.Character('.')) (Regex.one_or_more digit);
  Regex.concat_list [ prefix; Regex.Character('.'); (Regex.Star(digit))]
]

let number =  Regex.concat_list [ optplus; floating; optexp]


let whitespace = Regex.one_or_more(Regex.union_string "\n \t")

let rules : rule list = [
  Regex.Character('+'), (fun _ -> PLUS) ;
  Regex.Character('-'), (fun _ -> MINUS) ;
  Regex.Character('^'), (fun _ -> CARAT) ;
  Regex.concat_string "cos", (fun _ -> COS) ;
  Regex.Character('!'), (fun _ -> EXCLAMATION_MARK) ;
  number, (fun x -> NUM(Utils.chars_to_string x)) ;
  Regex.Character('\004'), ( fun _ -> EOF) ;
  whitespace, (fun _ -> WS) ;
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

let rec get_token stream =
  let rec get_token_inner state stream lexeme last_match =
    match stream with
      [] -> last_match
      |(line, c)::rest ->
        let state = next_state state c in
        match state with
          [] -> last_match
          |_ -> match matched_rules state with
            [] -> get_token_inner state rest (c::lexeme) last_match
            |(_, action)::_ -> get_token_inner state rest (c::lexeme) (Some(action, c::lexeme, line, rest))
      in
  match get_token_inner rules stream [] None with
      None -> raise No_Match
      |Some(action, lexeme, line, stream) -> match action (List.rev lexeme) with
                                        (* If whitespace is matched, call get_token again to get a non-ws token*)
                                        WS -> get_token stream
                                        |token -> (line, token), stream

let rec get_all_tokens stream =
  match stream with
    [] -> []
    |_ -> match get_token stream with
      |(line, token), [] -> [(line, token)]
      |(line, token), new_stream -> (line, token) :: get_all_tokens new_stream
