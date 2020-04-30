open Card
open Util

type suite =
  | PropertyList of property_card list
  | MoneyList of money_card list
  | ActionList of action_card list
  | WildcardList of wildcard list
  | RentList of rent_card list
  | Empty 
  (** ----------- Ansh's additions ------------- *)
type deck = card list

let get_dup_cards card_instance = 
  let count = match card_instance with
    | Property _ -> 1
    | Money m -> get_money_count m 
    | Rent r -> get_rent_count r 
    | Wildcard w -> get_wildcard_count w
    | Action a -> get_action_count a 
  in
  make_recurring_list card_instance count

let get_money_cards () = 
  let card_list = get_money () in
  List.flatten (List.map (fun x -> get_dup_cards (Money x)) card_list)

let get_rent_cards () = 
  let card_list = get_rents () in
  List.flatten (List.map (fun x -> get_dup_cards (Rent x)) card_list)

let get_property_cards () =
  let card_list = get_properties () in
  List.flatten (List.map (fun x -> get_dup_cards (Property x)) card_list)

let get_wildcards () =
  let card_list = get_wildcards () in
  List.flatten (List.map (fun x -> get_dup_cards (Wildcard x)) card_list)

let get_action_cards () =
  let card_list = get_actions () in
  List.flatten (List.map (fun x -> get_dup_cards (Action x)) card_list)

let initialize_deck (): deck = 
  List.flatten [get_money_cards (); get_property_cards (); get_wildcards ();
                get_action_cards (); get_rent_cards ()]

let remove_top_card (deck: deck): card * deck =
  match deck with
  | [] -> failwith "Deck is empty."
  | h :: t -> (h, t)

let remove_top_n_cards (deck: deck) (n: int): (card list * deck) = 
  if (List.length deck) < n then failwith "not enough cards" 
  else
    let rec helper deck n acc deck_acc = 
      if n = 0 then (acc, deck_acc) else
        match deck with
        | [] -> failwith "impossible"
        | h :: t -> helper t (n-1) (h :: acc) t in

    helper deck n [] deck 

(** ----------- Ansh's additions ------------- *)
(* 
let initialize_deck (): suite list = 
  let moneys = MoneyList (get_money()) in 
  let properties = PropertyList (get_properties()) in 
  let actions = ActionList (get_actions()) in 
  let wilds = WildcardList (get_wildcards()) in 
  let rents = RentList (get_rents()) in 
  [moneys; properties; actions; wilds; rents] *)

let nth (d:suite) i : card = 
  match d with 
  | PropertyList t -> Property (List.nth t i)
  | MoneyList t -> Money (List.nth t i)
  | ActionList t -> Action (List.nth t i)
  | WildcardList t -> Wildcard (List.nth t i)
  | RentList t -> Rent (List.nth t i)
  | Empty -> failwith "no more cards"

let lngth (d:suite) : int = 
  match d with 
  | PropertyList t -> List.length t
  | MoneyList t -> List.length t
  | ActionList t -> List.length t
  | WildcardList t -> List.length t
  | RentList t ->List.length t
  | Empty -> 0 
(* Use for removing property cards, for all other cards, decrease count till0
   then use this.*)
let rec remove_index lst n = 
  match lst with 
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_index t (n-1) 

(*this garbage removes the card from the suite and updates it *)
(* let rmv (d: suite) i = 
   match d with
   | PropertyList t -> PropertyList (remove_index t i)
   | MoneyList t -> List.length t
   | ActionList t -> List.length t
   | WildcardList t -> List.length t
   | RentList t ->List.length t
   | Empty -> 0  *)


let random_card (deck : suite list) : card = 
  let ste = List.nth deck (Random.int (List.length deck)) in 
  let cd = nth ste (Random.int (lngth ste)) in cd