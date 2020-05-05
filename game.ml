open Board
open Command

let rec get_n_names n acc = 
  if n = 0 then acc
  else match read_line () with
    | name when String.length name > 0 -> get_n_names (n-1) (name::acc)
    | _ -> print_endline "Invalid entry, a name has to have at least one char"; get_n_names n acc

let rec make_board () = 
  print_endline "Welcome! You're about to start a game of Monopoly Deal. To get started, enter the number of players playing (should be between 2 and 5).";
  match read_int_opt () with
  | Some i -> get_n_names i [] |> initialize_board i
  | None -> print_endline "Please enter an integer between 2 and 5"; make_board ()

let rec play_helper (board: board) = 
  let current_player = get_current_player board in
  let command = read_line () in
  match (command |> parse) with
  | Pass -> increment_turn board; print_endline ("it is now turn " ^ (get_current_player board)); draw_at_start_of_turn board ;play_helper board
  | exception Malformed msg -> print_endline msg; play_helper board
  | _ -> failwith "other cases unimplemented."

let rec play () = 
  let board = make_board () in
  distribute_cards_to_players board;
  play_helper board

let _ = play ()