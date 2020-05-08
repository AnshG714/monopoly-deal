open Card

(* The type representing a player in an instance of the game *)
type player

(* [intialize_player] creates a new player with name [name]. *)
val initialize_player : string -> player 

(* [add_cards_to_hand] adds the list of cards [add_list] to the hand of the 
   player [player] *)
val add_cards_to_hand : card list -> player -> unit

(* [get_cards_in_hand] is the list of cards currently held by player [player]. *)
val get_cards_in_hand: player -> (card list)

(* [get_played_personal_cards] is the list of cards in the pile of player [player]. *)
val get_played_personal_cards: player -> (card list)

(* [get_player_name] is the name of the player [player] *)
val get_player_name: player -> string

(* [play_card_]to_personal_pile] transfers the card with id [id] to to the hand
   of the player [player]. Throws Failure if [id] isn't an id of a card in the hand
   of [player] *)
val play_card_to_personal_pile : int -> player -> unit

val remove_cards_from_personal_pile : card list -> player -> unit

val add_cards_to_personal_pile : card list -> player -> unit
