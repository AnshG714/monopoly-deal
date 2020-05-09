open Yojson.Basic.Util
open ANSITerminal
open Util
(* Type Definitions *)
type venue_name = string
type card_value = int
type color = string
type rent = int
type action_name = string

(* list of printing colors *)
let color_map = 
  [("brown", "\027[38;5;94m"); ("blue", "\027[38;5;4m"); ("green", "\027[38;5;28m");
   ("light blue", "\027[38;5;45m"); ("orange", "\027[38;5;208m"); ("pink", "\027[38;5;200m");
   ("black", "\027[38;5;15m"); ("red", "\027[38;5;9m"); ("light green", "\027[38;5;194m");
   ("yellow", "\027[38;5;226m")]

(* keeps track of which id number has just been assigned *)
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

(** [json_list_to_alpha_list conversion j] converts a json list [j] to a list 
    of the type specified in [conversion].  *)
let json_list_to_alpha_list conversion (j: Yojson.Basic.t list) = 
  List.map (fun x -> conversion x) j

(** [list_to_array] converts a list [lst] to type array with the same elements. *)
let list_to_array lst= 
  let num_entries = List.length lst in
  let temp_arr = Array.make num_entries (Array.make num_entries 0) in
  for i = 0 to (num_entries - 1) do
    temp_arr.(i) <- Array.of_list (List.nth lst i)
  done 
  ;
  temp_arr 


(** [assign id ()] is the next id number to assign to a card, one higher than 
    the previous id number. *)
let assign_id (): int = 
  id_count := !id_count + 1;
  !id_count 

(* Conversions from json *)
(** [money_from_json j] creates a money card with an id and parses the json [j] 
    to get the value and count of the card. *)
let money_from_json j: money_card = {
  id = assign_id();
  value = j |> member "value" |> to_int;
  count = j |> member "count" |> to_int
}

(** [property_from_json j] creates a property card with an id and parses the 
    json [j] to get the venue, value, color, and rents of the card. *)
let property_from_json j: property_card = {
  id = assign_id();
  venue = j |> member "venue" |> to_string;
  value = j |> member "value" |> to_int;
  color = j |> member "color" |> to_string;
  rents = j |> member "rents" |> to_list |> json_list_to_alpha_list to_int |> Array.of_list
}

(** [action_from_json j] creates an action card with an id and parses the json
    [j] to get the name, description, value, and count of the card. *)
let action_from_json j: action_card = {
  id = assign_id();
  name = j |> member "name" |> to_string;
  desc = j |> member "desc" |> to_string;
  value = j |> member "value" |> to_int;
  count = j |> member "count" |> to_int;
}

(** [wildcard_from_json j] creates a wildcard with an id and parses the json [j]
    to get the colors, rents, count, and value of the card.*)
let wildcard_from_json j: wildcard = {
  id = assign_id();
  colors = j |> member "colors" |> to_list |> json_list_to_alpha_list to_string;
  rents = j |> member "rents" |> to_list |> 
          List.map (fun x-> x |> to_list |> json_list_to_alpha_list to_int) 
          |> list_to_array;
  count = j |> member "count" |> to_int;
  value = j |> member "value" |> to_int;
}

(** [rent_from_json j] creates a rent card with an id and parses the json [j]
    to get the colors, value, and count of the card. *)
let rent_from_json j : rent_card = {
  id = assign_id();
  colors = j |> member "colors" |> to_list |> json_list_to_alpha_list to_string;
  value = j |>  member "value" |> to_int;
  count = j |> member "count" |> to_int;
}

(** [get_card_from_file card_type card_method] gets a list of all the types of
    [card_type] in card_data.json and applies method [card_method] to parse it. *)
let get_card_from_file card_type card_method: 'a list =
  let json = Yojson.Basic.from_file "card_data.json" in
  json |> member card_type |> to_list |> List.map (fun x -> card_method x)

(* Getters for lists of each type of card *)
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


(* Action card getters *)
let get_action_name (card: action_card): action_name =
  card.name

let get_action_description (card: action_card): string = 
  card.desc

let get_action_value (card: action_card): card_value = 
  card.value

let get_action_count (card: action_card): int =
  card.count

(* Wildcard getters *)
let get_wildcard_colors (card: wildcard): color list =
  card.colors

let get_wildcard_rents (card: wildcard): rent array array =
  card.rents

let get_wildcard_count (card: wildcard): int = 
  card.count

let get_wildcard_value (card: wildcard): card_value = 
  card.value

(* Rent card getters *)
let get_rent_colors (card: rent_card): color list = 
  card.colors

let get_rent_value (card: rent_card): card_value = 
  card.value

let get_rent_count (card: rent_card): int =
  card.count

(* id getter *)
let get_id (card: card) = 
  match card with
  | Property c -> c.id
  | Wildcard c -> c.id
  | Rent c -> c.id
  | Action c -> c.id
  | Money c -> c.id

(* General printing functions *)
(** [print_contents sl color] prints the elemts of the list of string [sl] 
    using the color [color]. *)
let rec print_contents (sl: string list) (color: ANSITerminal.style) = 
  match sl with
  | [] -> print_string [] "\n"
  | h :: t -> print_string [color] h; print_string [] "    "; print_contents t color

(** [splice_first_n_list lst n] is a tuple of the first [n] elements of [lst]
    followed by the remaining elements of [lst].*)
let splice_first_n_list lst n =
  let rec helper lst n acc count = 
    match lst with
    | [] -> (acc, [])
    | h :: t -> if count = n then (acc, h :: t) else helper t n (acc @ [h]) (count + 1) in
  helper lst n [] 0

(** [batch_and_print batch_size card_list print_fun] prints the elements in 
    [card_list] as specified using [print_fun] with [batch_size] elements per 
    row. *)
let rec batch_and_print (batch_size: int) (card_list) (print_fun) = 
  match card_list with
  | [] -> ()
  | lst -> let b, rem = splice_first_n_list lst batch_size in
    print_fun b; batch_and_print batch_size rem print_fun

(* money card printing *)
(** [print_money_cards_helper cards] formats how to print out all the money cards
    in [cards].  *)
let print_money_cards_helper (cards: money_card list) =
  let l = List.length cards in
  let underline_list = make_recurring_list "\027[38;5;88m-------------" l in
  let money_header_list = make_recurring_list "\027[38;5;88m|   Money   |" l in
  let sidebar_list = make_recurring_list "\027[38;5;88m|           |" l in
  let money_id_list = List.map (fun (card: money_card) -> 
      if card.id >= 10 then
        "|   id:"  ^ string_of_int (card.id) ^   "   |"
      else "|    id:"  ^ string_of_int (card.id) ^   "    |"
    ) cards in
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
  print_contents (List.rev money_id_list) magenta;
  print_contents money_val_list magenta;
  print_contents sidebar_list magenta;
  print_contents underline_list magenta

let print_money_cards (cards: money_card list) = 
  batch_and_print 5 cards print_money_cards_helper


(* Action card printing*)
(** [print_action_cards_helper cards] formats how to print out all the action 
    cards in [cards].  *)
let print_action_cards_helper (cards: action_card list) = 
  let l = List.length cards in
  let underline_list = make_recurring_list "--------------------" l in 
  (* let action_header_list = make_recurring_list "\027[|      Action      |" l in *)
  let sidebar_list = make_recurring_list "|                  |" l in
  let action_val_list = List.map (fun (card: action_card) -> 
      if card.id >= 10 then
        "|      id:"  ^ string_of_int (card.id) ^   "       |"
      else "|       id:"  ^ string_of_int (card.id) ^   "       |"
    ) cards in

  let rec make_action_header (cards: action_card list) acc = 
    match cards with
    | [] -> acc
    | h :: t -> let s = "|  $" ^ (string_of_int (get_action_value h)) ^ "M      Action |" in
      make_action_header t (s :: acc) in

  let action_type_list = List.map (fun card -> 
      let action_name = get_action_name card in
      let name_length = String.length action_name in
      let space = 18 - name_length in
      let padding_left = if (space mod 2 = 0) then space / 2 else ((space - 1) / 2) in
      let padding_right = space - padding_left in
      "|" ^ (String.make (padding_left) ' ') ^ action_name ^ (String.make (padding_right) ' ') ^ "|"
    ) cards in

  print_contents underline_list green;
  print_contents (make_action_header cards []) green;
  print_contents underline_list green;
  print_contents sidebar_list green;
  print_contents (List.rev action_val_list) green;
  print_contents (List.rev action_type_list) green;
  print_contents underline_list green

let print_action_cards (cards: action_card list) =
  batch_and_print 5 cards print_action_cards_helper


(* Property card printing *)
(** [print_property_cards_helper cards] formats how to print out all the 
    property cards in [cards].  *)
let print_property_cards_helper (cards: property_card list) = 
  let rec make_dataless_helper cards acc st = 
    match cards with
    | [] -> acc
    | h :: t -> make_dataless_helper t 
                  (((List.assoc h.color color_map) ^ st) :: acc) st in
  let property_id_list = List.map (fun (card: property_card) -> 
      if card.id >= 10 then
        "|        id:"  ^ string_of_int (card.id) ^   "         |"
      else "|         id:"  ^ string_of_int (card.id) ^   "         |"
    ) cards in


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
  print_contents (List.rev property_id_list) white;
  for i = 0 to 3 do
    print_contents (make_rent_info_helper cards i []) white;
  done;
  print_contents (make_dataless_helper cards [] (String.make 24 '-')) white;
  ()

let print_property_cards cards = 
  batch_and_print 4 cards print_property_cards_helper


(* Wildcard printing *)
(** [print_wildcard_helper cards] formats how to print out all the wildcards
    in [cards].  *)
let print_wildcard_helper (cards: wildcard list) = 
  let l = List.length cards in
  let underline = make_recurring_list "\027[38;5;140m------------------------" l in
  let header = make_recurring_list "\027[38;5;140m|  Property Wildcard   |" l in

  let universal_pink = "\027[38;5;200m|      Universal       |" in
  let universal_orange = "\027[38;5;208m|      Universal       |" in
  let universal_yellow = "\027[38;5;226m|      Universal       |" in
  let universal_red = "\027[38;5;9m|      Universal       |" in
  let ultra_card_list = [universal_pink;universal_orange;universal_yellow;universal_red] in

  let rec wildcard_helper cards acc rent_number = 
    match cards with
    | [] -> acc
    | h :: t -> let rents = h.rents in
      if Array.length rents = 0 then wildcard_helper t ((List.nth ultra_card_list rent_number) :: acc) rent_number
      else let l1 = Array.length rents.(0) in
        let l2 = Array.length rents.(1) in
        let s1 = 
          if rent_number >= l1 then "     " 
          else (List.assoc (List.hd (h.colors)) color_map) 
               ^ string_of_int(rent_number) ^ "--$" ^ string_of_int(rents.(0).(rent_number)) in

        let s2 = 
          if rent_number >= l2 then "     "
          else (List.assoc (h.colors |> List.tl |> List.hd) color_map) 
               ^ string_of_int(rent_number) ^ "--$" ^ string_of_int(rents.(1).(rent_number)) in

        let finalst = "\027[38;5;140m|    " ^ s1 ^ "    " ^ s2 ^ "    " ^ "\027[38;5;140m|" in
        wildcard_helper t (finalst :: acc) rent_number in
  let wildcard_id_list = List.map (fun (card: wildcard) -> 
      if card.id >= 10 then
        "|        id:"  ^ string_of_int (card.id) ^   "         |"
      else "|         id:"  ^ string_of_int (card.id) ^   "         |"
    ) cards in


  print_contents underline white;
  print_contents header white;
  print_contents underline white;
  print_contents (List.rev wildcard_id_list) white;
  for i = 0 to 3 do
    print_contents (wildcard_helper cards [] i) white;
  done;
  print_contents underline white;
  ()

let print_wildcards (cards: wildcard list) = 
  batch_and_print 4 cards print_wildcard_helper

(* Rent card printing*)
(** [print_rent_cards_helper cards] formats how to print out all the rent cards
    in [cards].  *)
let print_rent_cards_helper (cards: rent_card list) =
  let l = List.length cards in 
  let underline_list = make_recurring_list "\027[38;5;360m--------------------" l in 
  let sidebar = "\027[38;5;360m|                  |" in

  let universal_pink = "\027[38;5;200m|    Universal     |" in
  let universal_orange = "\027[38;5;208m|    Universal     |" in
  let universal_yellow = "\027[38;5;226m|    Universal     |" in
  let ultra_card_list = [universal_pink;universal_orange;universal_yellow;] in

  let rent_id_list = List.map (fun (card: rent_card) -> 
      if card.id >= 10 then
        "|        id:"  ^ string_of_int (card.id) ^   "         |"
      else "|       id:"  ^ string_of_int (card.id) ^   "       |"
    ) cards in

  let rec make_rent_header cards acc = 
    match cards with
    | [] -> acc
    | h :: t -> let s = "\027[38;5;360m" ^ "|  $" ^ (string_of_int h.value) ^ "M        Rent |" in
      make_rent_header t (s::acc) in

  let rec make_color_info cards color_number acc = 
    match cards with
    | [] -> acc
    | h :: t -> let colors = h.colors in
      if List.length colors = 0 
      then (make_color_info t color_number ((List.nth ultra_card_list color_number) :: acc)) else 
        let s = if color_number >= (List.length colors) then sidebar else
            let required_color = List.nth colors color_number in
            let len = String.length required_color in
            let space = 18 - len in
            let padding_left = if (space mod 2 = 0) then space / 2 else ((space - 1)/ 2) in
            let padding_right = space - padding_left in
            ((List.assoc required_color color_map) ^ "|" ^ 
             (String.make padding_left ' ') ^ required_color ^ 
             (String.make padding_right ' ') ^ "|") in

        make_color_info t color_number 
          (s :: acc) in

  print_contents underline_list white;
  print_contents (make_rent_header cards []) white;
  print_contents underline_list white;
  print_contents (List.rev rent_id_list) white;

  for i = 0 to 2 do
    print_contents (make_color_info cards i []) white;
  done;
  print_contents underline_list white

let print_rent_cards (cards: rent_card list) =
  batch_and_print 5 cards print_rent_cards_helper
