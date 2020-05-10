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
    mutable discarded: card list
  }

module Mapping = Make(String)

(* ------------------- Functions to initialize a board --------------------- *)

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

let distribute_cards_to_players board = 
  let players = board.players in
  let deck = board.deck in 
  let discard = board.discarded in

  let n, r, d = remove_top_n_cards deck 2 discard in
  add_cards_to_hand n (List.hd board.players); board.deck <- r; board.discarded <- d;

  let deck2 = board.deck in 
  let discard2 = board.discarded in

  let rec helper players deck discard = 
    match players with
    | [] -> ()
    | h :: t -> let n, r, d = remove_top_n_cards deck 53 discard in
      add_cards_to_hand n h; board.deck <- r; board.discarded <- d;
      helper t r d; in

  helper players deck2 discard2

(* --------------------- getters for a board instance ---------------------- *)

let get_current_turn board = 
  board.turn

let get_players board =
  board.players

let get_current_player board = 
  let p = List.nth board.players board.turn in
  get_player_name p

let get_player_names board = 
  List.map (fun player -> get_player_name player) board.players

let get_player_from_name board name = 
  List.find (fun x -> get_player_name x = name) 
    (get_players board)

let get_card_value (id: int) (card_list: card list): int = 
  let l = List.filter (fun c -> (get_id c) = id) card_list in
  if l = [] then raise InvalidCard
  else match List.hd l with
    | Property p -> get_property_value p
    | Wildcard w -> get_wildcard_value w
    | Rent r -> get_rent_value r
    | Money m -> get_money_value m
    | Action a -> get_action_value a

(* ------------------------ Functions for Gameplay ------------------------- *)

let draw_new_cards (board: board) (mode: bool)=
  let player = List.nth board.players board.turn in 
  let cards = get_cards_in_hand player in
  let discard = board.discarded in
  if List.length cards >= 7 then ()
  else if List.length cards = 6 then 
    let n, r, d = remove_top_card board.deck discard in 
    add_cards_to_hand ([n]) player; board.deck <- r; board.discarded <- d
  else if List.length cards = 0 && mode then 
    let n, r, d = remove_top_n_cards board.deck 5 discard in 
    add_cards_to_hand n player; board.deck <- r; board.discarded <- d
  else 
    let n, r, d = remove_top_n_cards board.deck 2 discard in 
    add_cards_to_hand n player; board.deck <- r; board.discarded <- d

let increment_turn (board: board) = 
  board.turn <- (board.turn + 1) mod (List.length board.players);
  draw_new_cards board true

let add_card_to_pile board id = 
  try
    let p = List.nth board.players board.turn in 
    play_card_to_personal_pile id p
  with _ ->
    raise InvalidCard

let discard_card_from_hand (board : board) (id : int): unit = 
  let p = List.nth board.players board.turn in 
  let card = remove_card_from_hand id p in
  board.discarded <- card :: board.discarded

let transfer_card id player1 player2 = 
  try
    let c = remove_card_from_personal_pile id player1 in
    add_card_to_personal_pile c player2
  with _ -> 
    raise InvalidCard

(* ------------------------- printing functions -----------------------------*)

(** [collect_cards card_list] is a mapping of the cards in card_list to
    their card type. *)
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

(** [print_card_list card_list] prints the sorted [card_list]. *)
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

  let extract_color (card: card) = 
    match card with
    | Property v -> get_property_color v 
    | _ -> failwith "precondition violated" in
  let plist = Mapping.find "property" map in

  let sorted = List.sort (fun card1 card2 -> 
      if (extract_color card1 > extract_color card2) then 1 
      else if (extract_color card1 < extract_color card2) then -1
      else 0) plist in  

  print_property_cards (List.map (fun card ->
      match card with
      | Property p -> p
      | _ -> failwith "impossible"
    ) sorted );

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

let print_pile_of_player (board: board) (player_name: string) = 
  (* Precondition: player_name is a valid name. *)
  let player = List.find (fun player -> get_player_name player = player_name) (board.players) in
  print_card_list (get_played_personal_cards player)