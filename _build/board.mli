open Card
open Deck
open Player

type board

val init_mult_players: int -> string list -> player list 

val initialize_board: int -> string list -> board

val get_players: board -> player list

val get_current_player: board -> string

val increment_turn: board -> unit

val distribute_cards_to_players : board -> unit

val draw_new_cards : board -> unit

val print_current_player_hand : board -> unit

val print_current_player_pile : board -> unit
