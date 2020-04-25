open Yojson.Basic.Util

(* Type Definitions *)
type venue_name = string
type card_value = int
type color = string
type rent = int
type action_name = string

let id_count = ref 0

type property_card = {
  id: int;
  venue: venue_name;
  value: card_value;
  color: color;
  rents: rent array
}

type money_card = {
  id: int;
  value: card_value;
  count: int
}

type action_card = {
  id: int;
  name: action_name;
  desc: string;
  value: card_value;
  count: int;
}

type wildcard = {
  id: int;
  colors: color list;
  rents: rent array array;
  count: int;
  value: card_value
}

type rent_card = {
  id: int;
  colors: color list;
  value: card_value;
  count: int
}

type card = 
  | Property of property_card
  | Money of money_card
  | Rent of rent_card
  | Wildcard of wildcard
  | Action of action_card

let json_list_to_alpha_list conversion (j: Yojson.Basic.t list) = 
  List.map (fun x -> conversion x) j

let list_to_array lst= 
  let num_entries = List.length lst in
  let temp_arr = Array.make num_entries (Array.make num_entries 0) in
  for i = 0 to (num_entries - 1) do
    temp_arr.(i) <- Array.of_list (List.nth lst i)
  done 
  ;
  temp_arr 


let assign_id (): int = 
  id_count := !id_count + 1;
  !id_count

(* Conversions from json *)
let money_from_json j: money_card = {
  id = assign_id ();
  value = j |> member "value" |> to_int;
  count = j |> member "count" |> to_int
}

let property_from_json j: property_card = {
  id = assign_id ();
  venue = j |> member "venue" |> to_string;
  value = j |> member "value" |> to_int;
  color = j |> member "color" |> to_string;
  rents = j |> member "rents" |> to_list |> json_list_to_alpha_list to_int |> Array.of_list
}

let action_from_json j: action_card = {
  id = assign_id ();
  name = j |> member "name" |> to_string;
  desc = j |> member "desc" |> to_string;
  value = j |> member "value" |> to_int;
  count = j |> member "count" |> to_int;
}

let wildcard_from_json j: wildcard = {
  id = assign_id ();
  colors = j |> member "colors" |> to_list |> json_list_to_alpha_list to_string;
  rents = j |> member "rents" |> to_list |> 
          List.map (fun x-> x |> to_list |> json_list_to_alpha_list to_int) 
          |> list_to_array;
  count = j |> member "count" |> to_int;
  value = j |> member "value" |> to_int;
}

let rent_from_json j : rent_card = {
  id = assign_id ();
  colors = j |> member "colors" |> to_list |> json_list_to_alpha_list to_string;
  value = j |>  member "value" |> to_int;
  count = j |> member "count" |> to_int;
}


let get_card_from_file card_type card_method: 'a list =
  let json = Yojson.Basic.from_file "card_data.json" in
  json |> member card_type |> to_list |> List.map (fun x -> card_method x)

let get_money () : money_card list =
  get_card_from_file "money cards" money_from_json

let get_properties (): property_card list =
  get_card_from_file "property cards" property_from_json

let get_actions (): action_card list =
  get_card_from_file "action cards" action_from_json 

let get_wildcards (): wildcard list = 
  get_card_from_file "wildcards" wildcard_from_json 

let get_rents (): rent_card list =
  get_card_from_file "rent cards" rent_from_json

(* Define all getters to interface with other models *)

(* Money Card getters *)
let get_money_value (card: money_card): card_value =
  card.value

let get_money_count (card: money_card): int =
  card.count

(* Property Card getters *)
let get_property_name (card: property_card): venue_name =
  card.venue

let get_property_value (card: property_card): card_value =
  card.value

let get_property_color (card: property_card): color = 
  card.color

let get_property_rents (card: property_card): rent array = 
  card.rents


(* action card getters *)
let get_action_name (card: action_card): action_name =
  card.name

let get_action_description (card: action_card): string = 
  card.desc

let get_action_value (card: action_card): card_value = 
  card.value

let get_action_count (card: action_card): int =
  card.count

(* wildcard getters *)
let get_wildcard_colors (card: wildcard): color list =
  card.colors

let get_wildcard_rents (card: wildcard): rent array array =
  card.rents

let get_wildcard_count (card: wildcard): int = 
  card.count

let get_wildcard_value (card: wildcard): card_value = 
  card.value

(* rent card getters *)
let get_rent_colors (card: rent_card): color list = 
  card.colors

let get_rent_value (card: rent_card): card_value = 
  card.value

let get_rent_count (card: rent_card): int =
  card.count

let get_id (card: card) = 
  match card with
  | Property c -> c.id
  | Wildcard c -> c.id
  | Rent c -> c.id
  | Action c -> c.id
  | Money c -> c.id
