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


(*  -------------------------- Action card helpers ---------------------------*)
let rec get_player_name_input board =
  let names = get_player_names board in
  print_string (List.fold_left (fun acc name -> acc ^ name ^ "\n") "" names);
  match read_line () with
  | name when String.length name > 0 && List.mem name names -> name 
  | _ -> print_endline 
           "Invalid entry. A name must have more than one character and should be in the list of names.";
    get_player_name_input board

let pass_go (board: board) = 
  draw_new_cards board false

let its_my_bday (board: board) = 
  let currpl = List.nth (get_players board) (get_current_turn board) in
  let player_list = get_players board in
  let others = List.filter
      (fun p -> get_player_name p <> get_current_player board) player_list in

  let rec ask_for_money from_player total_value acc_value =

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

let rec sly_deal (board: board) = 
  print_endline "\027[38;5;190mYou have chosen to play a sly deal card. To do this, first enter enter the name of the person you want to perform the sly deal with. The players are: \027[0m";
  let name = get_player_name_input board in
  print_endline ("Here is " ^ name ^ "'s pile:");
  print_pile_of_player board name;
  print_endline "Either enter an id, or enter 'back' if you want to choose another player. ";

  let rec loop () = 
    match read_line () with
    | entry -> (match int_of_string_opt entry with
        | Some i -> (if i < 25 || i > 52 then (print_endline "this isn't a property card!"; loop ())
                     else match 
                         transfer_card i 
                           (List.find (fun x -> get_player_name x = name) 
                              (get_players board))
                           (List.nth (get_players board) 
                              (get_current_turn board)) 
                       with
                       | exception InvalidCard -> print_endline "wrong"; loop ()
                       | _ -> ())
        | None -> if entry = "back" then sly_deal board 
          else print_endline "You need to either enter a valid id for the property card you want to take, or type 'back'."; 
          loop ()) in

  loop ()


let action_card_helper board id =
  if id = 15 then pass_go board
  else if id = 16 then sly_deal board
  else ()

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
            if id >= 7 && id <= 16 then (action_card_helper board id; discard_card_from_hand board id)
            else add_card_to_pile board id; main_helper board (num+1)
          with InvalidCard ->
            print_endline "Enter a valid card ID.";
            main_helper board (num));

  | Quit -> print_endline "Hope you enjoyed playing :)"

  | exception Malformed msg -> print_endline msg; main_helper board num

  | _ -> failwith "other cases unimplemented."

let rec main () = 
  print_endline "\027[38;5;11mWelcome! You are about to start a game of Monopoly Deal. To get started, enter the number of players, followed by their names. \027[0m";
  Unix.sleep 1;
  let board = make_board () in
  distribute_cards_to_players board;
  main_helper board 0

let _ = main ()