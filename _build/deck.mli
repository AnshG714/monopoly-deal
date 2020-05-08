open Card

(* The type representing the list of all cards in the game - 
   the deck not only accounts for unique cards, but also contains duplicates for 
   similar types of cards in the Monopoly Deal deck. *)
type deck = card list

(* [initialize_deck] makes a new. shuffled deck of all the cards. *)
val initialize_deck : unit -> deck 

(* [remove_top_card] removes the first card from deck [deck], and returns that card
   along with the remaining deck, as the tuple (c, r), where [c] is the card 
   that was removed and [r] is the rest of the deck. *)
val remove_top_card : deck -> card * deck 

(* [remove_top_n_cards] removes the first [n] cards from deck [deck], and returns
   the list of cards that were removed with the remaining deck, as the tuple (cl, r) 
   where [cl] is the list of all cards, and [r] is the remaining deck after the 
   removal. *)
val remove_top_n_cards : deck -> int -> card list * deck 
