open OUnit2
open Command
open Card
open Deck
open Player

(** TEST PLAN:- 

    Since our system involves users being able to play a game, most of the tests
    we have defined deal with the getter functions for our cards, parsing commands
    and checking initial setup configurations for our board and players. Our main
    file, game.ml, is primarily focused on combining all the different models in 
    the system together and generating the terminal text interface, hence we 
    do not need to test it with OUnit (it can be tested by playing the game). 
    We primarily employed glass-box testing to walk through the possible execution
    trace of our code and achieve as much coverage as possible - due to there being
    a large variety of different cards, we deemed it impractical to test all of 
    the cards we had created and instead focus on the functionality of the getter
    functions. We believe that the OUnit test suite adequately checks the system for 
    potential bugs - in combination with our rigorous manual testing on the terminal
    we were able to identify and resolve small bugs that prevented the correct
    execution of our code.

*)


let first_money_card = List.hd(get_money())

let first_action_card = List.hd(get_actions())

let first_rent_card = List.hd(get_rents())

let first_wild_card = List.hd(get_wildcards())

let first_property_card = List.hd(get_properties())

(* Test suite for getters and setters implemented in card.ml*)
(** [card_test name expected_output input_card input_function] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [input_function input_card]*) 
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

let deck_v1 = initialize_deck()
let card1, deck1, discarded1 = remove_top_card deck_v1 []
let cards2, deck2, discarded2 = remove_top_n_cards deck_v1 5 []
let cards3, deck3, discarded3 = remove_top_card [] deck_v1
let deck_tests = [
  "remove_top_card 1" >:: (fun _ -> 
      assert_equal (List.length deck1) (107) (* deck is shuffled so cannot pinpoint specific id of the card*)
    );

  "remove_top_card 2" >:: (fun _ -> 
      assert_equal (discarded1) ([])
    );

  "remove_top_card 3" >:: (fun _ -> 
      assert_equal (List.length deck3) (107) 
    );

  "remove_top_card 4" >:: (fun _ -> 
      assert_equal (List.length discarded3) (0) 
    );

  "remove_top_n_cards 1" >:: (fun _ -> 
      assert_equal (List.length cards2) (5) 
    );

  "remove_top_n_cards 2" >:: (fun _ -> 
      assert_equal (List.length deck2) (103) 
    );



]

let p1 = initialize_player "player 1"
let p2 = initialize_player "player 2"
let p3 = initialize_player "player 3"
let p4 = initialize_player "player 4"
let cards1, deck1, discarded1 = remove_top_n_cards deck_v1 5 [] 
let cards2, deck2, discarded2 = remove_top_n_cards deck1 5 [] 
let cards3, deck3, discarded3 = remove_top_n_cards deck_v1 108 []
let card4, deck4, discarded4 = remove_top_card deck_v1 []
let () = add_cards_to_hand cards1 p1
let () = add_cards_to_personal_pile cards2 p2
let () = add_cards_to_personal_pile cards3 p3
let () = add_cards_to_hand [card4] p4
let () = add_card_to_personal_pile card4 p4

let player_tests = [

  "player name getter" >:: (fun _ -> 
      assert_equal (get_player_name p1) ("player 1")
    ); 

  "player hand getter" >:: (fun _ -> 
      assert_equal (get_cards_in_hand p1) (cards1)
    );

  "player hand getter 2" >:: (fun _ -> 
      assert_equal (discarded1) ([])
    ); 

  "player hand getter 3" >:: (fun _ -> 
      assert_equal (get_cards_in_hand p2) ([])
    ); 

  "player pile getter 1" >:: (fun _ -> 
      assert_equal (get_played_personal_cards p2) (cards2)
    ); 

  "player pile getter 2" >:: (fun _ -> 
      assert_equal (get_played_personal_cards p1) ([])
    );

  "player pile getter 3" >:: (fun _ -> 
      assert_equal (deck3) ([])
    ); 

  "check if set made false" >:: (fun _ -> 
      assert_equal (check_if_set_made p1 "blue") (false)
    ); 

  "check if set made true" >:: (fun _ -> 
      assert_equal (check_if_set_made p3 "blue") (true)
    ); 

  "remove card from hand" >:: (fun _ -> 
      assert_equal (remove_card_from_hand (get_id card4) p4) (card4)
    );

  "remove card from pile" >:: (fun _ -> 
      assert_equal (remove_card_from_personal_pile (get_id card4) p4) (card4)
    );

]

let suite =
  "test suite for A2"  >::: List.flatten [
    card_tests;
    command_tests;
    deck_tests;
    player_tests;
  ]

let () = run_test_tt_main suite