open ANSITerminal

(* value types *)
type venue_name = string
type card_value = int
type color = string
type rent = int

(* card types *)
type action_name
type property_card
type money_card
type rent_card
type action_card
type wildcard

(* the type representing all cards *)
type card = 
  | Property of property_card
  | Money of money_card
  | Rent of rent_card
  | Wildcard of wildcard
  | Action of action_card

(** [get_money ()] returns a list of all the money cards from the deck. *)
val get_money: unit -> money_card list

(** [get_properties ()] returns a list of all the property cards from the deck. *)
val get_properties: unit -> property_card list

(** [get_actions ()] returns a list of all the action cards from the deck. *)
val get_actions: unit -> action_card list

(** [get_wildcards ()] returns a list of all the wildcards from the deck. *)
val get_wildcards: unit -> wildcard list

(** [get_rents ()] returns a list of all the rent cards from the deck. *)
val get_rents: unit -> rent_card list

(* getters for individual card types *)
(** [get_money_value card] is the value of the money card [card]. *)
val get_money_value: money_card -> card_value

(** [get_money_count card] is the number of cards in the deck with the type of 
    money card [card]. *)
val get_money_count: money_card -> int

(** [get_property_name card] is the name of the property of the property
    card [card]. *)
val get_property_name: property_card -> venue_name

(** [get_property_value card] is the value of the property card [card]. *)
val get_property_value: property_card -> card_value

(** [get_property_color card] is the color of the property card [card]. *)
val get_property_color: property_card -> color

(** [get_property_rents card] is a list of the rents such that having one card 
    of the color of property card [card] corresponds to the first element of the 
    list, having two cards of that color corresponds to the second element of 
    the list, and so on. *)
val get_property_rents: property_card -> rent array

(** [get_action_name card] is the name of the action card [card]. *)
val get_action_name: action_card -> action_name

(** [get_action_description card] is the description of what the player can do 
    by playing action card [card]. *)
val get_action_description: action_card -> string

(** [get_action_value card] is the value of the action card [card]. *)
val get_action_value: action_card -> card_value

(** [get_action_count card] is the number of cards in the deck of the type
    of action card [card]. *)
val get_action_count: action_card -> int

(** [get_wildcard_colors card] is a list of property colors that the wildcard 
    [card] can be played as. *)
val get_wildcard_colors: wildcard -> color list

(** [get_wildcard_rents card] is a list of list of rents of [card] such that the 
    first element corresponds to a list of rents for the first color, where the 
    first element of this inner list corresponds to the rent of having one 
    property of that color, and so on. *)
val get_wildcard_rents: wildcard -> rent array array

(** [get_wildcard_count card] is the number of cards in the deck of the type
    of wildcard [card].  *)
val get_wildcard_count: wildcard -> int

(** [get_wildcard_value card] is the value of the wildcard [card]. *)
val get_wildcard_value: wildcard -> card_value

(** [get_rent_colors card] is a list of colors that the rent card [card] can 
    request rent for. *)
val get_rent_colors: rent_card -> color list

(** [get_rent_value card] is the value of the rent card [card]. *)
val get_rent_value: rent_card -> card_value

(** [get_rent_count card] is the number of cards in the deck of the type
    of rent card [card]. *)
val get_rent_count: rent_card -> int

(** [get_id card] returns the unique id of the kind of card of [card]. *)
val get_id: card -> int

(** [print_money_cards cards] prints out the elements of a list of money cards
    [cards] with its values and ids. *)
val print_money_cards: money_card list -> unit

(** [print_action_cards cards] prints out the elements of a list of action cards
    [cards] with its names, values, and ids. *)
val print_action_cards: action_card list -> unit

(** [print_property_cards cards] prints out the elements of a list of property
    cards [cards] with its names, rent values, and ids. *)
val print_property_cards: property_card list -> unit

(** [print_wildcards cards] prints out the elements of a list of wildcards 
    [cards] with its colors, rents, and ids *)
val print_wildcards : wildcard list -> unit

(** [print_rent_cards cards] prints out the elements of a lis of rent cards 
    [cards] with its colors, value, and id. *)
val print_rent_cards : rent_card list -> unit

