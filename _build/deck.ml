open Card
open Util

type deck = card list

let id = ref 1
(* [get_dup_cards] is a list of the correct count of the card [card_instance]. 
   This function produces a list of [m] instances of [card_instance] as a list, 
   where [m] is the count stored in the [count] field of [card_instance]. *)

let get_dup_cards card_instance = 
  let count = match card_instance with
    | Property _ -> 1
    | Money m -> get_money_count m 
    | Rent r -> get_rent_count r 
    | Wildcard w -> get_wildcard_count w
    | Action a -> get_action_count a 
  in
  make_recurring_list card_instance count

(* [get_money_cards] is the Card.card list of all money cards, including duplicates for
   each unique card. *)
let get_money_cards () = 
  let card_list = get_money () in
  List.flatten (List.map (fun x -> get_dup_cards (Money x)) card_list)

(* [get_rent_cards] is the Card.card list of all rent cards, including duplicates for
   each unique card. *)
let get_rent_cards () = 
  let card_list = get_rents () in
  List.flatten (List.map (fun x -> get_dup_cards (Rent x)) card_list)

(* [get_property_cards] is the Card.card list of all property cards *)
let get_property_cards () =
  let card_list = get_properties () in
  List.flatten (List.map (fun x -> get_dup_cards (Property x)) card_list)

(* [get_rent_cards] is the Card.card list of all rent cards, including duplicates for
   each unique card. *)
let get_wildcards () =
  let card_list = get_wildcards () in
  List.flatten (List.map (fun x -> get_dup_cards (Wildcard x)) card_list)

(* [get_action_cards] is the Card.card list of all action cards, including 
   duplicates for each unique card. *)
let get_action_cards () =
  let card_list = get_actions () in
  List.flatten (List.map (fun x -> get_dup_cards (Action x)) card_list)

let initialize_deck (): deck = 
  (*
    ids:
    Money: [53, 58]
    Properties: [25, 52]
    Wildcards: [17, 24]
    Action: [7, 16]
    Rent: [1, 6]
  *)
  List.flatten [get_money_cards (); get_property_cards (); get_wildcards ();
                get_action_cards (); get_rent_cards ()] 

let remove_top_card (deck: deck) (discard : card list): card * deck * card list =
  match deck with
  | [] -> let d = shuffle(discard) in (List.hd d, List.tl d, [])
  | h :: t -> (h, t, discard)

let remove_top_n_cards (deck: deck) (n: int) (discard: card list): (card list * deck * card list) = 
  if (List.length deck) < n then failwith "not enough cards" 
  else
    let rec helper deck n acc discard = 
      if n = 0 then (acc, deck, discard) else
        match deck with
        | [] -> helper (shuffle(discard)) n acc []
        | h :: t -> helper t (n-1) (h :: acc) discard in

    helper deck n [] discard