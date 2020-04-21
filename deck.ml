open Card

type card = 
  | Property of property_card
  | Money of money_card
  | Action of action_card 
  | Wildcard of wildcard
  | Rent of rent_card

type suite =
  | PropertyList of property_card list
  | MoneyList of money_card list
  | ActionList of action_card list
  | WildcardList of wildcard list
  | RentList of rent_card list
  | Empty 


let initialize_deck (): suite list = 
  let moneys = MoneyList (get_money()) in 
  let properties = PropertyList (get_properties()) in 
  let actions = ActionList (get_actions()) in 
  let wilds = WildcardList (get_wildcards()) in 
  let rents = RentList (get_rents()) in 
  [moneys; properties; actions; wilds; rents]

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