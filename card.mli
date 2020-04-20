(* value types *)
type venue_name
type card_value
type color
type rent

(* card types *)
type action_name
type property_card
type money_card
type rent_card
type action_card
type wildcard

(* the type representing all cards *)
type card

val get_card_from_file: string -> (Yojson.Basic.t -> 'a) -> 'a list

val get_money: unit -> money_card list

val get_properties: unit -> property_card list

val get_actions: unit -> action_card list

val get_wildcards: unit -> wildcard list

val get_rents: unit -> rent_card list

(* getters for individual card types *)
val get_money_value: money_card -> card_value

val get_money_count: money_card -> int

val get_property_name: property_card -> venue_name

val get_property_value: property_card -> card_value

val get_property_color: property_card -> color

val get_property_rents: property_card -> rent array

val get_action_name: action_card -> action_name

val get_action_description: action_card -> string

val get_action_value: action_card -> card_value

val get_action_count: action_card -> int

val get_wildcard_colors: wildcard -> color list

val get_wildcard_rents: wildcard -> rent array array

val get_wildcard_count: wildcard -> int

val get_wildcard_value: wildcard -> card_value

val get_rent_colors: rent_card -> color list

val get_rent_value: rent_card -> card_value

val get_rent_count: rent_card -> int


