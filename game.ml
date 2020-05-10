open Board
open Command
open Player
open Unix

(** [get_n_names n count acc] gets a list of [n] names *)
let rec get_n_names n count acc = 
  if count = n then acc
  else  (print_string ("> Name " ^ string_of_int (count + 1) ^ ": ");
         match read_line () with
         | name when String.length name > 0 -> get_n_names n (count + 1) (name::acc)
         | _ -> print_endline "Invalid entry, a name has to have at least one char"; get_n_names n count acc)

(** [make_board] initializes a board for the game with a list of player names. *)
let rec make_board () = 
  print_endline "\027[38;5;47mPlease enter an integer between 2 and 5 \027[0m"; 
  print_string "> ";
  match read_int_opt () with
  | Some i -> print_endline "\027[38;5;47m\nGreat! Now, please enter the names of the players that will be playing. \027[0m"; 
    get_n_names i 0 [] |> List.rev |> initialize_board i
  | None -> print_endline "\027[38;5;9mIncorrect entry. Please enter an integer between 2 and 5 \027[0m"; make_board () 


(*  -------------------------- Action card helpers ---------------------------*)
(** [get_player_name_input board] is the name inputted. *)
let rec get_player_name_input board =
  let names = get_player_names board in
  print_string (List.fold_left (fun acc name -> acc ^ name ^ "\n") "" names);
  match read_line () with
  | name when String.length name > 0 && (List.mem name names || name = "cancel") -> name 
  | _ -> print_endline 
           "Invalid entry. A name must have more than one character and should be in the list of names.";
    get_player_name_input board

(** [ask_for_money] asks [from_player] which money cards they want to give to
    [current_player] such that the total value is at least [total_value] or they
    are out of cards, and transfers it from their piles. *)
let rec ask_for_money board current_player from_player total_value acc_value =

  print_endline "Please enter a valid card id to play";
  match read_int_opt () with
  | None -> print_endline "Please enter a valid card id to play";
    ask_for_money board current_player from_player total_value acc_value
  | Some id -> (match get_card_value id (get_played_personal_cards from_player) with
      | i -> transfer_card id from_player current_player; 
        if (acc_value + i) < total_value 
        then ask_for_money board current_player from_player total_value (acc_value + i) 
      | exception InvalidCard -> print_endline "You do not possess this card. Please enter the id of a card that you have.";
        ask_for_money board current_player from_player total_value acc_value
    )

(** [pass_go board] distributes up to 2 cards to the current player. *)
let pass_go (board: board) = 
  draw_new_cards board false;
  true

(** [its_my_bday board] gets $2M from every other player and transfers it to
    the current player's pile. *)
let its_my_bday (board: board) = 
  let currpl = List.nth (get_players board) (get_current_turn board) in
  let player_list = get_players board in
  let others = List.filter
      (fun p -> get_player_name p <> get_current_player board) player_list in

  let rec helper total_value plist =
    match plist with
    | [] -> ()
    | h :: t -> let pile = get_played_personal_cards h in
      if List.length pile = 0 then ()
      else print_endline ((get_player_name h) ^ "'s pile"); 
      print_pile_of_player board (get_player_name h); 
      ask_for_money board currpl h total_value 0; helper total_value t in

  helper 2 others;
  true

(** [move_property] moves a property from player with name [name] to the current
    player's pile. *)
let rec move_property board f name = 
  let rec transfer_helper i = 
    match 
      transfer_card i 
        (List.find (fun x -> get_player_name x = name) 
           (get_players board))
        (List.nth (get_players board) 
           (get_current_turn board)) 
    with
    | exception InvalidCard -> print_endline "wrong"; loop ()
    | _ -> true

  and loop () = 
    match read_line () with
    | entry -> (match int_of_string_opt entry with
        | Some i -> (if i < 25 || i > 52 then (print_endline "this isn't a property card!"; loop ())
                     else transfer_helper i)
        | None -> if entry = "back" then (if f = sly_deal then sly_deal board else forced_deal board)
          else (print_endline "You need to either enter a valid id for the property card you want to take, or type 'back'."; 
                loop ())) 
    | exception Failure _ -> false in

  loop ()


(** [sly_deal board] allows the current player to steal any property of their 
    choice from any other player of their choice. *)
and  sly_deal (board: board) = 
  print_endline "\027[38;5;190mYou have chosen to play a sly deal card. To do this, first enter enter the name of the person you want to perform the sly deal with. If you want to cancel, type 'cancel'. The players are: \027[0m";
  let name = get_player_name_input board in
  if name = "cancel" then false 
  else
    (print_endline ("Here is " ^ name ^ "'s pile:");
     print_pile_of_player board name;
     print_endline "Either enter an id, or enter 'back' if you want to choose another player. ";

     move_property board sly_deal name;)

(** [forced_deal board] allows the current player to switch any of their properties
    with any other player's property of their choice. *)
and  forced_deal board =
  let currpl = List.nth (get_players board) (get_current_turn board) in
  print_endline "\027[38;5;190mYou have chosen to play a forced deal card.";
  print_current_player_pile board;
  print_endline "Enter the id of the card you want to swap out or enter 'cancel' to cancel";
  let id = read_line () in
  if id = "cancel" then false 
  else
    let rec swap_out p id = 
      (match int_of_string_opt id with
       | Some i -> (try remove_card_from_personal_pile i p with _ -> print_endline "please enter a valid id"; let id = read_line () in swap_out p id)
       | None -> print_endline "please enter a valid id"; let id = read_line () in swap_out p id;) in

    let card_out = swap_out currpl id in 

    print_endline "Enter enter the name of the person you want to swap properties with. The players are: \027[0m";
    let name = get_player_name_input board in
    let player = List.find (fun x -> get_player_name x = name) (get_players board) in
    print_endline ("Here is " ^ name ^ "'s pile:");
    print_pile_of_player board name;
    print_endline "Enter the id of the card you want to swap in";

    if move_property board forced_deal name then add_card_to_personal_pile card_out player; true

(** [debt_collector board] allows the current player to request $5M from any 
    other player of their choice. *)
let debt_collector board = 
  print_endline "\027[38;5;190mYou have chosen to play a debt collector card. To do this, enter enter the name of the person you want to collect debt from. The players are: \027[0m";
  let name = get_player_name_input board in
  if name = "cancel" then false else
    (let player = List.find (fun x -> get_player_name x = name) (get_players board) in
     print_pile_of_player board name;
     ask_for_money board (List.nth (get_players board) (get_current_turn board)) player 5 0;
     true)

(** [action_card_helper] performs the corresponding functionality of the action
    card with id [id]. *)
let action_card_helper board id =
  if id = 8 then debt_collector board
  else if id = 13 then its_my_bday board
  else if id = 15 then pass_go board
  else if id = 16 then sly_deal board
  else if id = 10 then forced_deal board
  else false

(** [main_player board num] represents a pass of each command input. A player
    can only play 3 cards per turn. *)
let rec main_helper (board: board) (num: int) = 
  print_endline ("It is now " ^ (get_current_player board) ^ "'s turn\n\n\n");
  let command = read_line () in
  match (command |> parse) with
  | Pass -> increment_turn board; 
    print_endline ("it is now turn " ^ (get_current_player board)); main_helper board 0

  | ViewPile -> print_current_player_pile board; main_helper board num

  | ViewHand -> print_current_player_hand board; main_helper board num

  | Play id -> if num >= 3 then (print_endline "You cannot play more than 3 cards per turn"; 
                                 main_helper board num) 
    else (try
            if id >= 7 && id <= 16 then 
              (if (action_card_helper board id) then discard_card_from_hand board id else ())
            else add_card_to_pile board id; main_helper board (num+1)
          with InvalidCard ->
            print_endline "Enter a valid card ID.";
            main_helper board (num));

  | Quit -> print_endline "Hope you enjoyed playing :)"

  | exception Malformed msg -> print_endline msg; main_helper board num

  | _ -> failwith "other cases unimplemented."

(** [main] starts the game. *)
let rec main () = 
  print_endline "\027[38;5;11mWelcome! You are about to start a game of Monopoly Deal. To get started, enter the number of players, followed by their names. \027[0m";
  Unix.sleep 1;
  let board = make_board () in
  distribute_cards_to_players board;
  main_helper board 0

let _ = main ()