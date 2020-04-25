open Card
(* type suite *)
type deck
(* Initial game deck - count value for each card already in json file, need to 
   count/ include all property cards as well. *)
val initialize_deck : unit -> deck 
val shuffle : deck -> deck 
val remove_top_card : deck -> card * deck 
val remove_top_n_cards : deck -> int -> card list * deck 

(* val nth : suite -> int -> card
   val lngth : suite -> int 
   val remove_index : 'a list -> int -> 'a list
   val random_card : suite list -> card *)