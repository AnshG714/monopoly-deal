open Card

type player = {
  cards_in_hand: card list;
  played_personal_cards: card list
}

let init_player (): player =
  {
    cards_in_hand = [];
    played_personal_cards = []
  }

let add_cards_to_hand add_list player = 
  add_list @ player.cards_in_hand

let play_cards_to_personal_pile card_list player = 
  ()