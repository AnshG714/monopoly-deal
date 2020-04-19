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

