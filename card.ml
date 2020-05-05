open Yojson.Basic.Util
open ANSITerminal
open Util
(* Type Definitions *)
type venue_name = string
type card_value = int
type color = string
type rent = int
type action_name = string

let color_map = 
  [("brown", "\027[38;5;94m"); ("blue", "\027[38;5;19m"); ("green", "\027[38;5;28m");
   ("light blue", "\027[38;5;45m"); ("orange", "\027[38;5;208m"); ("pink", "\027[38;5;200m");
   ("black", "\027[38;5;15m"); ("red", "\027[38;5;9m"); ("light green", "\027[38;5;194m");
   ("yellow", "\027[38;5;226m")]

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

let rec print_contents (sl: string list) (color: ANSITerminal.style) = 
  match sl with
  | [] -> print_string [] "\n"
  | h :: t -> print_string [color] h; print_string [] "    "; print_contents t color

let splice_first_n_list lst n =
  let rec helper lst n acc count = 
    match lst with
    | [] -> (acc, [])
    | h :: t -> if count = n then (acc, h :: t) else helper t n (acc @ [h]) (count + 1) in
  helper lst n [] 0

let rec batch_and_print (batch_size: int) (card_list) (print_fun) = 
  match card_list with
  | [] -> ()
  | lst -> let b, rem = splice_first_n_list lst batch_size in
    print_fun b; batch_and_print batch_size rem print_fun

let print_money_cards_helper (cards: money_card list) =
  let l = List.length cards in
  let underline_list = make_recurring_list "\027[38;5;88m-------------" l in
  let money_header_list = make_recurring_list "\027[38;5;88m|   Money   |" l in
  let sidebar_list = make_recurring_list "\027[38;5;88m|           |" l in
  let money_val_list = List.map (fun card -> 
      (let money_value = get_money_value card in 
       if money_value = 10 then 
         "|    $"  ^ string_of_int money_value ^   "    |"
       else 
         "|    $"  ^ string_of_int money_value ^   "     |")
    ) cards in

  print_contents underline_list magenta;
  print_contents money_header_list white;
  print_contents underline_list magenta;
  print_contents sidebar_list magenta;
  print_contents money_val_list magenta;
  print_contents sidebar_list magenta;
  print_contents underline_list magenta

let print_money_cards (cards: money_card list) = 
  batch_and_print 5 cards print_money_cards_helper

let print_action_cards_helper (cards: action_card list) = 
  let l = List.length cards in
  let underline_list = make_recurring_list "\027[38;5;22m--------------------" l in 
  let action_header_list = make_recurring_list "\027[38;5;115m|      Action      |" l in
  let sidebar_list = make_recurring_list "\027[38;5;47m|                  |" l in
  let action_val_list = List.map (fun card -> 
      (let action_value = get_action_value card in 
       if action_value = 10 then 
         "|       $"  ^ string_of_int action_value ^   "        |"
       else 
         "|        $"  ^ string_of_int action_value ^   "        |")
    ) cards in

  let action_type_list = List.map (fun card -> 
      let action_name = get_action_name card in
      let name_length = String.length action_name in
      let space = 18 - name_length in
      let padding_left = if (space mod 2 = 0) then space / 2 else (space / 2 - 1) in
      let padding_right = space - padding_left in
      "|" ^ (String.make (padding_left) ' ') ^ action_name ^ (String.make (padding_right) ' ') ^ "|"
    ) cards in

  print_contents underline_list green;
  print_contents action_header_list green;
  print_contents underline_list green;
  print_contents sidebar_list green;
  print_contents action_val_list green;
  print_contents action_type_list green;
  print_contents underline_list green

let print_action_cards (cards: action_card list) =
  batch_and_print 5 cards print_action_cards_helper

let print_property_cards_helper (cards: property_card list) = 
  let rec make_dataless_helper cards acc st = 
    match cards with
    | [] -> acc
    | h :: t -> make_dataless_helper t 
                  (((List.assoc h.color color_map) ^ st) :: acc) st in


  let rec make_property_name_helper (cards: property_card list) acc = 
    match cards with
    | [] -> acc
    | h :: t -> let p_name = h.venue in
      let p_size = String.length p_name in 
      let space = 22 - p_size in
      let padding_left = if (space mod 2 = 0) then space / 2 else ((space - 1)/ 2) in
      let padding_right = space - padding_left in
      let s = "|" ^ (String.make padding_left ' ') ^ p_name ^ (String.make padding_right ' ') ^ "|" in
      make_property_name_helper t (((List.assoc h.color color_map) ^ s) :: acc) in

  let rec make_rent_info_helper (cards: property_card list) rent_number acc = 
    match cards with
    | [] -> acc
    | h :: t -> let rents = h.rents in
      let num_in_set = Array.length rents in
      let s = 
        if rent_number >= num_in_set then "|                      |"
        else let rent_info = (string_of_int rent_number) ^ "---$" ^ string_of_int(rents.(rent_number)) ^ "M" in
          "|       " ^ rent_info ^ "        |" in
      make_rent_info_helper t rent_number (((List.assoc h.color color_map ^ s) :: acc)) in

  print_contents (make_dataless_helper cards [] (String.make 24 '-')) white;
  print_contents (make_property_name_helper cards []) white;
  print_contents (make_dataless_helper cards [] (String.make 24 '-')) white;
  print_contents (make_dataless_helper cards [] ("|" ^ (String.make 22 ' ') ^ "|")) white;
  for i = 0 to 3 do
    print_contents (make_rent_info_helper cards i []) white;
  done;
  print_contents (make_dataless_helper cards [] (String.make 24 '-')) white;
  ()

let print_property_cards cards = 
  batch_and_print 4 cards print_property_cards_helper

let print_rent_card (card: rent_card) : unit = 
  let card_colors = card.colors in
  let card_value = card.value in
  match card_colors with 
  | ["green"; "blue"] -> ANSITerminal.(print_string [green; on_blue; Underlined] ("\n$" ^ string_of_int card_value ^"M     " ^ "Rent\n"));
  | ["brown"; "light blue"] -> ANSITerminal.(print_string [magenta; on_white; Underlined] ("\n$" ^ string_of_int card_value ^"M     " ^ "Rent\n"));
  | ["orange"; "pink"] -> ANSITerminal.(print_string [magenta; on_red; Underlined] ("\n$" ^ string_of_int card_value ^"M     " ^ "Rent\n"));
  | ["black"; "light green"] -> ANSITerminal.(print_string [black; on_green; Underlined] ("\n$" ^ string_of_int card_value ^"M     " ^ "Rent\n"));
  | ["red"; "yellow"] -> ANSITerminal.(print_string [red; on_yellow; Underlined] ("\n$" ^ string_of_int card_value ^"M     " ^ "Rent\n"));
  | _ -> ();
    print_string [] "\n"  
