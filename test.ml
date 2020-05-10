open OUnit2
open Command
open Card
open Deck

let first_money_card = List.hd(get_money())

let first_action_card = List.hd(get_actions())

let first_rent_card = List.hd(get_rents())

let first_wild_card = List.hd(get_wildcards())

let first_property_card = List.hd(get_properties())

let deck_v1 = initialize_deck()

(* Test suite for getters and setters implemented in card.ml*)
(** [parse_test name input expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with [parse input]*) 
let card_test name expected_output input_card input_function = 
  name >:: (fun _ -> assert_equal expected_output (input_function input_card))

(* Test suite for functions implemented in command.ml*)
(** [parse_test name input expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with [parse input]*) 
let parse_test
    (name: string)
    (input: string)
    (expected_output: command) : test = 
  name >:: (fun _ -> assert_equal expected_output (parse input))

let card_tests = [
  card_test "Money value getter" 10 (first_money_card) get_money_value;
  card_test "Money count getter" 1 (first_money_card) get_money_count;
  card_test "prop name getter" "Baltic Avenue" (first_property_card) get_property_name;
  card_test "prop value getter" 1 (first_property_card) get_property_value;
  card_test "prop color getter" "brown" (first_property_card) get_property_color;
  card_test "prop rents getter" [|1; 2|] (first_property_card) get_property_rents;
  card_test "action name getter" "Deal Breaker" (first_action_card) get_action_name;
  card_test "action desc getter" "Steal a complete set of properties from every \
                                  player (includes any buildings)"
    (first_action_card) get_action_description;
  card_test "action value getter" 5 (first_action_card) get_action_value;
  card_test "action count getter" 2 (first_action_card) get_action_count;
  card_test "wildcard color getter" ["blue"; "green"] (first_wild_card) 
    get_wildcard_colors;
  card_test "wildcard rents getter" [|[|3; 8|]; [|2; 4; 7|]|] (first_wild_card) 
    get_wildcard_rents;
  card_test "wildcard value getter" 4 (first_wild_card) get_wildcard_value;
  card_test "wildcard count getter" 1 (first_wild_card) get_wildcard_count; 
  card_test "rent color getter" ["green"; "blue"] (first_rent_card) 
    get_rent_colors; 
  card_test "rent value getter" 1 (first_rent_card) get_rent_value;
  card_test "rent count getter" 2 (first_rent_card) get_rent_count;
  card_test "money id getter" 1 (Money first_money_card) get_id; 
]

let command_tests = 
  [
    parse_test "Parse string into command" "  draw   " (Draw);
    parse_test "Parse string into command" " play       2  " (Play 2);
    parse_test "Parse string into command" "view pile" (ViewPile);
    parse_test "Parse string into command" "view hand     " (ViewHand);
    parse_test "Parse string into command" "  pile ag pg    as" (Pile "ag pg as");
    parse_test "Parse string into command" " discard       5" (Discard 5);
    parse_test "Parse string into command" "pass" (Pass);
    parse_test "Parse string into command" "  quit " (Quit);
    "Check for malformed command" >:: 
    (fun _ -> assert_raises (Malformed "the id should be an integer.") 
        (fun () -> parse "play crazy"));
    "Check for malformed command" >:: 
    (fun _ -> assert_raises (Malformed "please enter the discard keyword followed \
                                        by the id of the card you want to play.") 
        (fun () -> parse "discard"));
    "Check for malformed command" >:: 
    (fun _ -> assert_raises (Malformed "draw does not have any keywords after it.") 
        (fun () -> parse "draw pile "));
    "Check for malformed command" >:: 
    (fun _ -> assert_raises (Malformed "Incorrect view command. You can either call \
                                        view pile or view hand") 
        (fun () -> parse "  view  draw"));
    "Check for malformed command" >:: 
    (fun _ -> assert_raises (Malformed "Incorrect command. Did you mean to call \
                                        view hand or view pile?") 
        (fun () -> parse "  view"));
    "Check for malformed command" >:: 
    (fun _ -> assert_raises (Malformed "pass does not have any keywords after it.") 
        (fun () -> parse "pass pile "));
    "Check for malformed command" >:: 
    (fun _ -> assert_raises (Malformed "quit does not have any keywords after it.") 
        (fun () -> parse "quit    game"));
    "Check for malformed command" >:: 
    (fun _ -> assert_raises (Malformed "Unknown keyword") 
        (fun () -> parse "what"));            
    "Check for empty command" >:: 
    (fun _ -> assert_raises (Empty) (fun () -> parse ""));  
  ]

let deck_tests = [

]

let player_tests = []

let suite =
  "test suite for A2"  >::: List.flatten [
    card_tests;
    command_tests;
    deck_tests;
    player_tests;
  ]

let () = run_test_tt_main suite