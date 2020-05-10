open Card
open Deck
open Player

exception InvalidCard

type board

(* [init_mult_players] is a list of n players, with names given in [names].
    Requires: n = List.length names. *)
val init_mult_players: int -> string list -> player list 

(** [initialize_board n player_names] creates a board of [n] players with 
    names [names]. *)
val initialize_board: int -> string list -> board

(** [get_players board] is a list of all the players in [board]. *)
val get_players: board -> player list

(** [get_current_player board] is the name of the player whose turn it is. *)
val get_current_player: board -> string

(** [get_player_names board] is a list of all the names of the players in [board]. *)
val get_player_names: board -> string list

(** [increment_turn board] switches the turn to the next person in [board]. *)
val increment_turn: board -> unit

(** [distribute_cards_to_players board] distributes 5 cards to all the players
    and an additional 2 cards to the first player in [board]. *)
val distribute_cards_to_players : board -> unit

(** [draw_new_cards board mode] adds up to 2 cards to the current player's hand
    such that they have no more than 7 cards, and adds 5 cards to the current 
    player's hand if they have 0 cards and [mode] is true. *)
val draw_new_cards : board -> bool -> unit

(** [print_current_player_hand board] prints out the cards in the current 
    player's hand in [board]. *)
val print_current_player_hand : board -> unit

(** [print_current_player_pile board] prints out the cards in the current 
    player's pile in [board]. *)
val print_current_player_pile : board -> unit

(** [print_pile_of_player board player_name] prints out the pile of the player
    whose name is [player_name] in [board]. *)
val print_pile_of_player : board -> string -> unit

(** [add_card_to_pile board id] adds the card with id [id] from the current
    player's hand to their pile, and raises InvalidCard if that card is not in 
    their hand. *)
val add_card_to_pile : board -> int -> unit

(** [discard_card_from_hand board id] adds the card with id [id] from the current
    player's hand to the discarded list, and raises InvalidCard if that card is
    not in their hand. *)
val discard_card_from_hand : board -> int -> unit

(** [get_current_turn board] returns the current turn of [board]. *)
val get_current_turn : board -> int

(** [get_card_value id card_list] is the value of the card with id [id] in
    [card_list]. *)
val get_card_value: int -> card list -> int

(** [transfer_card id player1 player1] transfers card with id [id] 
    from [player1] pile to [player2] pile. Raises InvalidCard if [player1] does
    not have that card in their pile. *)
val transfer_card : int -> player -> player -> unit