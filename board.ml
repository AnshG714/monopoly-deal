(* I'm going to try using board as a controller itself. *)

open Card
open Deck
open Player
open Util

exception InvalidCard

type board = 
  {
    players: player list;
    mutable deck: deck;
    mutable turn: int;
    discarded: card list
  }

(* [init_mult_players] is a list of n players, with names given in [names].
    Precondition: n = List.length names
*)
let rec init_mult_players n names = 
  if (n = 0) then []
  else initialize_player(List.hd names) :: (init_mult_players (n-1) (List.tl names))

let initialize_board (n: int) (player_names: string list): board = 
  {
    players = init_mult_players n player_names;
    deck = shuffle (initialize_deck ());
    turn = 0;
    discarded = []
  }

(* These two methods can be migrated to whatebver our controller will be. *)
let increment_turn (board: board) = 
  board.turn <- (board.turn + 1) mod (List.length board.players)

let get_current_turn board = 
  board.turn

let distribute_cards_to_players board = 
  let players = board.players in
  let deck = board.deck in 
  let rec helper players deck = 
    if players = [] then () else
      let n, r = remove_top_n_cards deck 5 in
      add_cards_to_hand n (List.hd players); board.deck <- r;
      helper (List.tl players) r; in
  helper players deck

let draw_new_cards (board: board) =
  let player = List.nth board.players board.turn in 
  let cards = get_cards_in_hand player in
  if List.length cards >= 7 then ()
  else if List.length cards = 6 then 
    let c, d= remove_top_card board.deck in 
    add_cards_to_hand ([c]) player; board.deck <- d
  else if List.length cards = 0 then 
    let c, d = remove_top_n_cards board.deck 5 in 
    add_cards_to_hand c player; board.deck <- d
  else 
    let c, d = remove_top_n_cards board.deck 2 in 
    add_cards_to_hand c player; board.deck <- d


let check_card_in_hand player id = 
  let cards = get_cards_in_hand player in
  List.exists (fun card -> get_id card = id) cards

let check_card_in_pile player id = 
  let cards = get_played_personal_cards player in
  List.exists (fun card -> get_id card = id) cards

let get_players board =
  board.players

let get_current_player board = 
  let p = List.nth board.players board.turn in
  get_player_name p

(* print wildcards *)
let print_current_player_hand board = 
  let p = List.nth board.players board.turn in 
  let rec helper lst = 
    match lst with 
    | [] -> ()
    | h::t -> 
      (match h with 
       | Money m -> print_money_cards [m]; helper t
       | Action a -> print_action_cards [a]; helper t
       | Rent r -> print_rent_card r; helper t
       | Property p -> print_property_cards [p]; helper t
       | _ -> ()) in 
  helper (get_cards_in_hand p)

(* print wildcards *)
let print_current_player_pile board = 
  let p = List.nth board.players board.turn in 
  let rec helper lst = 
    match lst with 
    | [] -> ()
    | h::t -> 
      (match h with 
       | Money m -> print_money_cards [m]; helper t
       | Action a -> print_action_cards [a]; helper t
       | Rent r -> print_rent_card r; helper t
       | Property p -> print_property_cards [p]; helper t
       | _ -> ()) in 
  helper (get_played_personal_cards p)
