open Card
open Deck
open Player

type board = 
  {
    players: player list;
    deck: deck;
    mutable turn: int
  }

let rec init_mult_players n = 
  if (n = 0) then []
  else initialize_player() :: (init_mult_players (n-1))

let initialize_board (n: int): board = 
  {
    players = init_mult_players n;
    deck = initialize_deck();
    turn = 0;
  }

(* This can be migrated to whatebver our controller will be. *)
let increment_turn (board: board) = 
  board.turn <- board.turn + 1

let get_current_turn board = 
  board.turn
