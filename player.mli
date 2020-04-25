open Card

type player
val initialize_player : unit -> player 
val add_cards_to_hand : card list -> player -> card list 
val get_cards_in_hand: player -> (card list)
val get_played_personal_cards: player -> (card list)
val play_cards_to_personal_pile : card list -> player -> unit
val remove_cards_from_personal_pile : card list -> player -> unit
val add_cards_to_personal_pile : card list -> player -> unit
