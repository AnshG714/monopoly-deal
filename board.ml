(* I'm going to try using board as a controller itself. *)

open Card
open Deck
open Player
open Util

exception InvalidCard

type board = 
  {
    players: player list;
    deck: deck;
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
      add_cards_to_hand n (List.hd players);
      helper (List.tl players) r; in
  helper players deck

let check_card_in_hand player id = 
  let cards = get_cards_in_hand player in
  List.exists (fun card -> get_id card = id) cards

let check_card_in_pile player id = 
  let cards = get_played_personal_cards player in
  List.exists (fun card -> get_id card = id) cards

