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

let get_sorted_properties_of_color player color = 
  let l = List.filter (fun (card: card) -> 
      match card with
      | Property p -> (get_property_color p) = color
      | Wildcard w -> List.mem color (get_wildcard_colors w)
      | _ -> false
    ) (get_played_personal_cards player) in

  List.sort (fun card1 card2 -> (get_id card2) - (get_id card1)) l

let get_rent_earnings (player: player) (color: color) = 
  let player_props = get_sorted_properties_of_color player color in 
  match player_props with
  | [] -> failwith "this player has no properties of this color"
  | h :: _ -> (match h with 
      | Property p -> let rents = get_property_rents p in 
        rents.(List.length player_props - 1)

      | Wildcard w -> let rents = get_wildcard_rents w in
        let colors = get_wildcard_colors w in
        if colors = [] then failwith "can't demand from only universal"
        else if (List.nth colors 0) = color then rents.(0).(List.length player_props - 1)
        else rents.(1).(List.length player_props - 1)

      | _ -> failwith "precondition violated"          
    )

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


(* [remove_card_helper] removes card with id [id] from player [player] pile if
   [removal_from_pile] is true, and from player [player] hand if 
   [removal_from_pile] is false. *)
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

let remove_cards_from_personal_pile (cards: card list) (player: player) = 
  let removal_ids = List.map (fun card -> get_id card) cards in
  let l = List.filter (fun c -> not (List.mem (get_id c) removal_ids))
      player.played_personal_cards in
  player.played_personal_cards <- l

let add_card_to_personal_pile (card : card) (player: player) = 
  player.played_personal_cards <- card :: player.played_personal_cards

let add_cards_to_personal_pile (cards: card list) (player: player) = 
  player.played_personal_cards <- cards @ player.played_personal_cards

let check_if_set_made (player: player) (color: color): bool =

  (* get cards from pile *)
  let l = get_played_personal_cards player in

  (* Get all the property cards/wilcards with the same color as [color] *)
  let color_filter = List.filter (fun x -> match x with 
      |Property v -> get_property_color v = color
      |Wildcard w -> 
        List.mem color (get_wildcard_colors w) || get_wildcard_colors w = []
      | _ -> false ) l in

  (* If length of color_filter is 0, then this have no cards for that color set*)
  if color_filter = [] then false

  (* Only possess all colors in a set of the length of the list containing 
     all the cards of the same color = the length list of rents for any one property
     in the list of cards of the same color. *)
  else if 
    (let rec extract_rents lst = 
       let h = List.hd lst in 
       match h with 
       | Property v -> get_property_rents v
       | Wildcard w ->  extract_rents (List.tl lst) (* There has to be one non-wildcard to
                                                       make a valid set. *)
       | _ -> failwith "invariant violated" in

     Array.length (extract_rents color_filter) > List.length color_filter) then false
  else true

