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

(* [get_sorted_properties_of_color] is the sorted list of properties for player
   [player], for color [color]. The resulting list is sorted in DESCENDING order
   of ids.*)
val get_sorted_properties_of_color: player -> color -> card list

(* [get_rent_earnings] is the amount that would be earned if [player] were to 
   charge rent on a property with color [color]*)
val get_rent_earnings: player -> color -> rent

(* [play_card_]to_personal_pile] transfers the card with id [id] to to the hand
   of the player [player]. Throws Failure if [id] isn't an id of a card in the hand
   of [player] *)
val play_card_to_personal_pile : int -> player -> unit

(* [remove_card_from_personal_pile] removes card wth id [id] from player [player]
   pile and returns it. *)
val remove_card_from_personal_pile : int -> player -> card

(* [remove_card_from hand] removes card with id [id] from player [player] hand
   and returns it. *)
val remove_card_from_hand : int -> player -> card

(* [remove_cards_from_personal_pile] removes the list of cards [cards] from
   the personal pile of [player]. *)
val remove_cards_from_personal_pile : card list -> player -> unit

(* [add_card_to_personal_pile] adds [cards] to the pile of player [player]. *)
val add_card_to_personal_pile : card -> player -> unit

(* [add_cards_to_personal_pile] adds the list [cards] to the pile of player 
   [player]. *)
val add_cards_to_personal_pile : card list -> player -> unit

(* [check_if_set_made] is true if the player [player] owns all the cards in the
   set with color [color], false otherwise. *)
val check_if_set_made: player -> color -> bool
