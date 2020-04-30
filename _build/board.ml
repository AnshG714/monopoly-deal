(* I'm going to try using board as a controller itself. *)

open Card
open Deck
open Player
open Util

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
  board.turn <- board.turn + 1

let get_current_turn board = 
  board.turn

