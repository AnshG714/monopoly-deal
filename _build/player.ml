open Card

type player = {
  cards_in_hand: card list;
  played_personal_cards: card list
}

let initialize_player (): player =
  {
    cards_in_hand = [];
    played_personal_cards = []
  }

let add_cards_to_hand add_list player = 
  add_list @ player.cards_in_hand

let play_cards_to_personal_pile (card_list: card list) player = 
  (* let card_ids_in_playing_list = List.map (fun x -> x.id) card_list
     let hand_cards_after_removal = List.filter (fun x -> List.mem) *)
  ()