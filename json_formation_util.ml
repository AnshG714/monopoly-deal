open Printf
let file = "card_data.json"

let string_input_helper () = 
  match read_line () with
  | exception End_of_file -> print_endline "nothing given, defaulting to empty string"; ""
  | s -> s 

let int_input_helper () =
  match read_int_opt () with
  | None -> print_endline "setting to 0"; 0
  | Some s -> s

let continue_check_helper f = 
  match read_line () with
  | "y" | "Y" -> f ()
  | _ -> ()

let rec money_card_input () =
  let _ = print_string "Enter the value of this money card\n" in
  let money_value = int_input_helper () in

  let _ = print_string "Enter the count of this money card\n" in
  let count_value = int_input_helper () in

  let json = "{\"value\": " ^ string_of_int money_value ^ ", \"count\": " ^ string_of_int count_value ^ "}," in
  let oc = open_out_gen [Open_append] 0o666 file in (* The octal specifies file permissions *)
  fprintf oc "%s\n" json;
  close_out oc;
  print_string("Enter another one? y/n \n");
  let _= continue_check_helper money_card_input in
  ()

let rec action_card_input () =
  print_string "Enter the name of this action card\n"; 
  let card_name = string_input_helper () in

  print_string "Enter the description of this action card\n";
  let card_description = string_input_helper () in

  print_string "Enter the value of this action card\n";
  let card_value = int_input_helper () in 

  print_string "Enter the count of this action card\n";
  let count_value = int_input_helper () in 

  let json = "{\"name\": " ^ card_name ^ ", \"desc\": " ^ card_description ^ ", \"value\": " ^ string_of_int card_value ^ ", \"count\: " ^ string_of_int count_value ^ "}," in

  let oc = open_out_gen [Open_append] 0o666 file in (* The octal specifies file permissions *)
  fprintf oc "%s\n" json;
  close_out oc;
  print_string("Enter another one? y/n \n");
  let _= continue_check_helper action_card_input in
  ()

let main () =
  ANSITerminal.(print_string [cyan]
                  "Hey there! To enter a JSON type, enter a card type. Please enter all card types together, i.e, don't enter one money card and then a rent card and then a money card again.\n");
  money_card_input
    ()

let () = main ()