open Card

type player = {
  mutable cards_in_hand: card list;
  mutable played_personal_cards: card list
}

let initialize_player (): player =
  {
    cards_in_hand = [];
    played_personal_cards = []
  }

let add_cards_to_hand add_list player = 
  player.cards_in_hand <- add_list @ player.cards_in_hand

let get_cards_in_hand player = 
  player.cards_in_hand

let get_played_personal_cards player = 
  player.played_personal_cards

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