open Board
open Command
open Player
open Unix

let rec get_n_names n count acc = 
  if count = n then acc
  else  (print_string ("> Name " ^ string_of_int (count + 1) ^ ": ");
         match read_line () with
         | name when String.length name > 0 -> get_n_names n (count + 1) (name::acc)
         | _ -> print_endline "Invalid entry, a name has to have at least one char"; get_n_names n count acc)

let rec make_board () = 
  print_endline "\027[38;5;47mPlease enter an integer between 2 and 5 \027[0m"; 
  print_string "> ";
  match read_int_opt () with
  | Some i -> print_endline "\027[38;5;47m\nGreat! Now, please enter the names of the players that will be playing. \027[0m"; 
    get_n_names i 0 [] |> List.rev |> initialize_board i
  | None -> print_endline "\027[38;5;9mIncorrect entry. Please enter an integer between 2 and 5 \027[0m"; make_board () 

let rec main_helper (board: board) = 
  let command = read_line () in
  match (command |> parse) with
  | Pass -> increment_turn board; print_endline ("it is now turn " ^ (get_current_player board)); main_helper board
  | ViewPile -> print_current_player_pile board; main_helper board
  | ViewHand -> print_current_player_hand board; main_helper board
  | Play id -> 
    (try
       add_card_to_pile board id;
     with InvalidCard ->
       print_endline "Enter a valid card ID.";); 
    main_helper board
  | exception Malformed msg -> print_endline msg; main_helper board
  | Quit -> print_endline "Hope you enjoyed playing :)"
  | _ -> failwith "other cases unimplemented."

let rec main () = 
  print_endline "\027[38;5;11mWelcome! You are about to start a game of Monopoly Deal. To get started, enter the number of players, followed by their names. \027[0m";
  Unix.sleep 1;
  let board = make_board () in
  distribute_cards_to_players board;
  main_helper board

(* Action card helpers *)
let pass_go (board: board) = 
  draw_new_cards board false

let its_my_bday (board: board) = 
  let currpl = List.nth (get_players board) (get_current_turn board) in
  let player_list = get_players board in
  let others = List.filter
      (fun p -> get_player_name p <> get_current_player board) player_list in

  let rec ask_for_money from_player total_value acc_value =

    (* ask which id they want to play *)
    print_endline "Please enter a valid card id to play";
    match read_int_opt () with
    | None -> print_endline "Please enter a valid card id to play"; ask_for_money from_player total_value acc_value
    | Some id -> (match get_card_value id board with
        | i -> transfer_card id from_player currpl; if (acc_value + i) < total_value then ask_for_money from_player total_value (acc_value + i) 
        | exception InvalidCard -> print_endline "You do not possess this card. Please enter the id of a card that you have.";
          ask_for_money from_player total_value acc_value
      ) in

  let rec helper total_value plist =
    match plist with
    | [] -> ()
    | h :: t -> let pile = get_played_personal_cards h in
      if List.length pile = 0 then ()
      else ask_for_money h total_value 0; helper total_value plist in

  helper 2 others

let action_card_helper board id =
  if id = 15 then pass_go board
  else failwith "unimplemented"

let _ = main ()