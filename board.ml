open Card
open Deck
open Player
open Util
open Map

exception InvalidCard

type board = 
  {
    players: player list;
    mutable deck: deck;
    mutable turn: int;
    discarded: card list
  }

module Mapping = Make(String)

(* [init_mult_players] is a list of n players, with names given in [names].
    Precondition: n = List.length names
*)
let rec init_mult_players n names = 
  if (n = 0) then []
  else initialize_player(List.hd names) :: (init_mult_players (n-1) (List.tl names))

let initialize_board (n: int) (player_names: string list): board = 
  {
    players = init_mult_players n player_names;
    deck = shuffle(Random.self_init (); initialize_deck ());
    turn = 0;
    discarded = []
  }

(* These two methods can be migrated to whatebver our controller will be. *)
let increment_turn (board: board) = 
  board.turn <- (board.turn + 1) mod (List.length board.players)

let get_current_turn board = 
  board.turn

let distribute_cards_to_players board = 
  let players = board.players in
  let deck = board.deck in 
  let rec helper players deck = 
    if players = [] then () else
      let n, r = remove_top_n_cards deck 5 in
      add_cards_to_hand n (List.hd players); board.deck <- r;
      helper (List.tl players) r; in
  helper players deck

let draw_new_cards (board: board) =
  let player = List.nth board.players board.turn in 
  let cards = get_cards_in_hand player in
  if List.length cards >= 7 then ()
  else if List.length cards = 6 then 
    let c, d= remove_top_card board.deck in 
    add_cards_to_hand ([c]) player; board.deck <- d
  else if List.length cards = 0 then 
    let c, d = remove_top_n_cards board.deck 5 in 
    add_cards_to_hand c player; board.deck <- d
  else 
    let c, d = remove_top_n_cards board.deck 2 in 
    add_cards_to_hand c player; board.deck <- d


let check_card_in_hand player id = 
  let cards = get_cards_in_hand player in
  List.exists (fun card -> get_id card = id) cards

let check_card_in_pile player id = 
  let cards = get_played_personal_cards player in
  List.exists (fun card -> get_id card = id) cards

let get_players board =
  board.players

let get_current_player board = 
  let p = List.nth board.players board.turn in
  get_player_name p

let collect_cards card_list = 
  let l = Mapping.empty in
  let money = Mapping.add "money" [] l in
  let action = Mapping.add "action" [] money in
  let wildcard = Mapping.add "wildcard" [] action in
  let property = Mapping.add "property" [] wildcard in
  let final = Mapping.add "rents" [] property in

  let update_dictionary (key: string) (new_element) (dictionary) = 
    let l = Mapping.find key dictionary in
    Mapping.add key (new_element :: l) dictionary  in

  let rec helper (cards: card list) (dictionary) =
    match cards with
    | [] -> dictionary
    | h :: t -> (match h with 
        | Money m as m'-> helper t (update_dictionary "money" m' dictionary)
        | Action a as a' -> helper t (update_dictionary "action" a' dictionary)
        | Property p as p' -> helper t (update_dictionary "property" p' dictionary)
        | Wildcard w as w' -> helper t (update_dictionary "wildcard" w' dictionary)
        | Rent r as r' -> helper t (update_dictionary "rents" r' dictionary)
      ) in

  helper card_list final

let print_card_list card_list = 
  let map = collect_cards card_list in
  print_action_cards (List.map (fun card ->
      match card with
      | Action a -> a
      | _ -> failwith "impossible"
    ) (Mapping.find "action" map));

  print_money_cards (List.map (fun card ->
      match card with
      | Money m -> m
      | _ -> failwith "impossible"
    ) (Mapping.find "money" map));

  print_property_cards (List.map (fun card ->
      match card with
      | Property p -> p
      | _ -> failwith "impossible"
    ) (Mapping.find "property" map));

  print_wildcards (List.map (fun card ->
      match card with
      | Wildcard w -> w
      | _ -> failwith "impossible"
    ) (Mapping.find "wildcard" map));

  print_rent_cards (List.map (fun card ->
      match card with
      | Rent r -> r
      | _ -> failwith "impossible"
    ) (Mapping.find "rents" map))


let print_current_player_hand board = 
  let p = List.nth board.players board.turn in 
  let hand = get_cards_in_hand p in 
  print_card_list hand

let print_current_player_pile board = 
  let p = List.nth board.players board.turn in 
  let pile = get_played_personal_cards p in 
  print_card_list pile

let add_card_to_pile board id = 
  try
    let p = List.nth board.players board.turn in 
    play_cards_to_personal_pile id p
  with _ ->
    raise InvalidCard
