
(* testing productions / items *)
let root_prod = (Parser.Nonterminal("S"), [Parser.Nonterminal("E")])
let initial_item = Parser.Item(0, root_prod)
let add_prod = (Parser.Nonterminal("E"), [Parser.Nonterminal("E"); Parser.Terminal(PLUS); Parser.Nonterminal("T")])
let test_items = [Parser.Item(1, root_prod); Parser.Item(1, add_prod)]

(* closure of the root production *)

let%expect_test _ =
  Parser.closure Parser.test_grammar [initial_item]
  |> List.sort compare
  |> List.iter Parser.print_item;
  [%expect{|
E -> . E + T
E -> . T
F -> . num
F -> . ( E )
S -> . E
T -> . F
T -> . T * F
  |}]

(* goto testing (example from the 'dragon' compilers book) *)
let %expect_test _ =
  Parser.goto test_items (Terminal PLUS) Parser.test_grammar
  |> List.sort compare
  |> List.iter Parser.print_item;
  [%expect{|
F -> . num
F -> . ( E )
T -> . F
T -> . T * F
E -> E + . T
  |}]

(* canonical collection testing *)
let %expect_test _ =
  Parser.canonical_collection Parser.test_grammar
  |> List.sort compare
  |> List.iteri (fun i l -> Printf.printf "- item set %d: -\n" i; List.iter Parser.print_item l;);
  [%expect {|
- item set 0: -
E -> . E + T
E -> . T
F -> . num
F -> . ( E )
S -> . E
T -> . F
T -> . T * F
- item set 1: -
E -> . E + T
E -> . T
F -> . num
F -> . ( E )
T -> . F
T -> . T * F
F -> ( . E )
- item set 2: -
F -> . num
F -> . ( E )
T -> . F
T -> . T * F
E -> E + . T
- item set 3: -
F -> . num
F -> . ( E )
T -> T * . F
- item set 4: -
E -> E . + T
S -> E .
- item set 5: -
E -> E . + T
F -> ( E . )
- item set 6: -
E -> T .
T -> T . * F
- item set 7: -
F -> num .
- item set 8: -
T -> F .
- item set 9: -
T -> T . * F
E -> E + T .
- item set 10: -
F -> ( E ) .
- item set 11: -
T -> T * F .
  |}]


(* first set testing *)

let epsilon_grammar = Parser.Grammar (
  [Parser.Terminal(PLUS); Parser.Epsilon],
  [Parser.Nonterminal("E"); Parser.Nonterminal("T")],
  Parser.Nonterminal("S"),
  [
    (Parser.Nonterminal "S", [Parser.Nonterminal "E"]);
    (Parser.Nonterminal "T", [Parser.Nonterminal "E"; Parser.Terminal PLUS]);
    (Parser.Nonterminal "E", [Parser.Nonterminal "E"; Parser.Terminal PLUS; Parser.Nonterminal "E"]);
    (Parser.Nonterminal "E", [Parser.Epsilon]);
  ]
)

let %expect_test _ =
  Parser.compute_first_sets epsilon_grammar
  |> Parser.print_first_map;
  [%expect{|
epsilon : [ epsilon ]
+ : [ + ]
E : [ epsilon + ]
T : [ + ]
  |}]

let %expect_test _ =
  Parser.compute_first_sets Parser.test_grammar
    |> Parser.print_first_map;
    [%expect{|
+ : [ + ]
* : [ * ]
num : [ num ]
( : [ ( ]
) : [ ) ]
E : [ num ( ]
F : [ num ( ]
T : [ num ( ]
    |}]

(* follow set testing *)
let%expect_test _ =
Parser.compute_first_sets Parser.test_grammar
|> Parser.compute_follow_sets Parser.test_grammar
|> Parser.print_first_map;
[%expect{|
E : [ $ + ) ]
F : [ $ + * ) ]
S : [ $ ]
T : [ $ + * ) ]
|}]