open Card
open Deck

type player = {
  name: string;
  mutable cards_in_hand: card list;
  mutable played_personal_cards: card list
}

let initialize_player name: player =
  {
    name = name;
    cards_in_hand = [];
    played_personal_cards = []
  }

let add_cards_to_hand add_list player = 
  player.cards_in_hand <- add_list @ player.cards_in_hand

let get_cards_in_hand player = 
  player.cards_in_hand

let get_played_personal_cards player = 
  player.played_personal_cards

let get_player_name player = 
  player.name

(* moves from hand to personal pile *)
let play_card_to_personal_pile (id: int) (player: player) =

  (* get remaining cards in hand after playing *)
  let hand_cards_after_removal = List.filter (fun x -> 
      ((get_id x) <> id)) (get_cards_in_hand player) in

  let removed_cards = List.filter (fun x -> 
      ((get_id x) = id)) (get_cards_in_hand player) in

  if List.length removed_cards = 0 then failwith "id error"
  else
    (* update fields - this essentially transfers cards from the hand to the board. *)
    (player.cards_in_hand <- List.tl removed_cards @ hand_cards_after_removal;
     player.played_personal_cards <- List.hd removed_cards :: player.played_personal_cards;);
  ()


let remove_card_helper (id: int) (player: player) (removal_from_pile: bool) : card = 
  let pile_after_removal = 
    List.filter (fun x -> (get_id x) <> id) 
      (if removal_from_pile then get_played_personal_cards player else get_cards_in_hand player) in 

  let removed_cards =  List.filter (fun x -> (get_id x) = id)
      (if removal_from_pile then get_played_personal_cards player else get_cards_in_hand player) in 

  if List.length removed_cards = 0 then failwith "id error"
  else
  if removal_from_pile then 
    player.played_personal_cards <- List.tl removed_cards @ pile_after_removal
  else
    player.cards_in_hand <- List.tl removed_cards @ pile_after_removal;
  List.hd removed_cards

let remove_card_from_personal_pile (id : int) (player) : card =
  remove_card_helper id player true

let remove_card_from_hand (id: int) (player: player) : card = 
  remove_card_helper id player false

let add_card_to_personal_pile (card : card) (player: player) = 
  player.played_personal_cards <- card :: player.played_personal_cards

(* if the player has more than 7 cards in their hand, it discards cards such 
   that there are only 7 cards*)
let rec discard_until_seven player = 
  if List.length player.cards_in_hand > 7 
  then match player.cards_in_hand with
    | [] -> failwith "impossible"
    | h::t -> player.cards_in_hand <- t; discard_until_seven player
  else player

(* [check_if_set_made] is true if the player [player] owns all the cards in the
   set with color [color], false otherwise. *)
let check_if_set_made (player: player) (color: color): bool =

  (* get cards from pile *)
  let l = get_played_personal_cards player in

  (* Get all the property cards with the same color *)
  let color_filter = List.filter (fun x -> match x with 
      |Property v -> get_property_color v = color
      | _ -> false ) l in

  (* If length of color_filter is 0, then this have no cards for that color set*)
  if List.length color_filter = 0 then false

  (* Only possess all colors in a set of the length of the list containing 
     all the cards of the same color = the length list of rents for any one property
     in the list of cards of the same color. *)
  else if 
    (let h = List.hd color_filter in 
     let rent_list = match h with 
       | Property v -> get_property_rents v 
       | _ -> failwith "invariant violated" in
     Array.length rent_list <> List.length color_filter) then false
  else true

