open Card
open Deck
open Player

type board = 
  {
    players: player list;
    deck: deck
  }

let rec init_mult_players n = 
  if (n = 0) then []
  else initialize_player() :: (init_mult_players (n-1))

let board_init (n: int): board = 
  {
    players = init_mult_players n;
    deck = initialize_deck();
  }

