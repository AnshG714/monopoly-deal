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
let play_cards_to_personal_pile (card_list: card list) (player: player) =

  (* get ids of the cards to be played *)
  let card_ids_in_playing_list = List.map get_id card_list in

  (* get remaining cards in hand after playing *)
  let hand_cards_after_removal = List.filter (fun x -> 
      not (List.mem (get_id x) card_ids_in_playing_list)) (get_cards_in_hand player) in

  (* update fields - this essentially transfers cards from the hand to the board. *)
  player.cards_in_hand <- hand_cards_after_removal;
  player.played_personal_cards <- card_list @ player.played_personal_cards

let remove_cards_from_personal_pile (card_list: card list) (player) =
  let card_ids_in_playing_list = List.map get_id card_list in

  player.played_personal_cards <- 
    List.filter (fun x -> not (List.mem (get_id x) card_ids_in_playing_list)) 
      (get_played_personal_cards player)

let add_cards_to_personal_pile (card_list: card list) (player: player) = 
  player.played_personal_cards <- card_list @ player.played_personal_cards

(* if the player has more than 7 cards in their hand, it discards cards such 
   that there are only 7 cards*)
let rec discard_until_seven player = 
  if List.length player.cards_in_hand > 7 
  then match player.cards_in_hand with
    | [] -> failwith "impossible"
    | h::t -> player.cards_in_hand <- t; discard_until_seven player
  else player

(* discards all the cards from [cards] in the hand of [player]*)
let discard_cards player cards= 
  let rec helper lst card acc = 
    match lst with 
    | [] -> acc
    | h::t -> if List.mem h cards then helper t card acc else helper t card (h::acc) in 
  player.cards_in_hand <- helper player.cards_in_hand cards []



