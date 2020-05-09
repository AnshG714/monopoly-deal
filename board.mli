open Card
open Deck
open Player

exception InvalidCard

type board

val init_mult_players: int -> string list -> player list 

val initialize_board: int -> string list -> board

val get_players: board -> player list

val get_current_player: board -> string

val get_player_names: board -> string list

val increment_turn: board -> unit

val distribute_cards_to_players : board -> unit

val draw_new_cards : board -> bool -> unit

val print_current_player_hand : board -> unit

val print_current_player_pile : board -> unit

val print_pile_of_player : board -> string -> unit

val add_card_to_pile : board -> int -> unit

val discard_card_from_hand : board -> int -> unit

val get_current_turn : board -> int

val get_card_value: int -> card list -> int

val transfer_card : int -> player -> player -> unit