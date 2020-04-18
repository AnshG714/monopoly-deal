open Yojson.Basic.Util

(* Type Definitions *)
type venue_name = string
type card_value = int
type color = string
type rent = int
type action_name = string

type property_card = {
  venue: venue_name;
  value: card_value;
  color: color;
  rents: rent array
}

type money_card = {
  value: card_value;
  count: int
}

type action_card = {
  name: action_name;
  desc: string;
  value: card_value;
  count: int;
}

type wildcard = {
  colors: color list;
  rents: rent array array;
  count: int;
  value: card_value
}

type rent_card = {
  colors: color list;
  value: card_value;
  count: int
}

type card = 
  | Property of property_card
  | Money of money_card
  | Action of action_card
  | Wildcard of wildcard
  | Rent of rent_card

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


(* Conversions from json *)
let money_from_json j: money_card = {
  value = j |> member "value" |> to_int;
  count = j |> member "count" |> to_int
}

let property_from_json j: property_card = {
  venue = j |> member "venue" |> to_string;
  value = j |> member "value" |> to_int;
  color = j |> member "color" |> to_string;
  rents = j |> member "rents" |> to_list |> json_list_to_alpha_list to_int |> Array.of_list
}

let action_from_json j: action_card = {
  name = j |> member "name" |> to_string;
  desc = j |> member "desc" |> to_string;
  value = j |> member "value" |> to_int;
  count = j |> member "count" |> to_int;
}

let wildcard_from_json j: wildcard = {
  colors = j |> member "colors" |> to_list |> json_list_to_alpha_list to_string;
  rents = j |> member "rents" |> to_list |> 
          List.map (fun x-> x |> to_list |> json_list_to_alpha_list to_int) 
          |> list_to_array;
  count = j |> member "count" |> to_int;
  value = j |> member "value" |> to_int;
}

let rent_from_json j : rent_card = {
  colors = j |> member "colors" |> to_list |> json_list_to_alpha_list to_string;
  value = j |>  member "value" |> to_int;
  count = j |> member "count" |> to_int;
}


let get_card_from_file card_type card_method j: 'a list =
  let json = Yojson.Basic.from_file "card_data.json" in
  json |> member card_type |> to_list |> List.map (fun x -> card_method x)

(* Make MLI *)

let get_money j: money_card list =
  get_card_from_file "money cards" money_from_json j

let get_properties j: property_card list =
  get_card_from_file "property cards" property_from_json j

(* Apu *)
let get_actions j: action_card list =
  get_card_from_file "action cards" action_from_json j

(* Pooja *)
let get_wildcards j: wildcard list = 
  get_card_from_file "wildcards" wildcard_from_json j

(* Apu*)
let get_rent j: rent_card list =
  get_card_from_file "rent cards" rent_from_json j 

(* Money Card getters *)
let get_money_value (card: money_card): card_value =
  card.value

let get_money_count (card: money_card): int =
  card.count

(* Define all getters to interface with other models *)

(* Property Card getters *)
let get_property_name (card: property_card): venue_name =
  card.venue

let get_property_value (card: property_card): card_value =
  card.value

let get_property_color (card: property_card): color = 
  card.color

let get_property_rents (card: property_card): rent array = 
  card.rents