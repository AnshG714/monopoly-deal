open Printf
let file = "card_data.json"

(** string_input_helper is the string that the user inputs in the terminal. 
    An important thing to remember is that if you press enter by mistake, nothing 
    will be taken in, and the function will default to an empty string. This is
    intentional. If this was a mistake and you want to change this, you have to 
    do so manually in card_data.json *)
let string_input_helper () = 
  match read_line () with
  | exception End_of_file -> print_endline "nothing given, defaulting to empty string"; ""
  | s -> "\"" ^ s ^ "\""

(** int_input_helper is the integer that the user inputs in the terminal. 
    An important thing to remember is that if you press enter by mistake, nothing 
    will be taken in, and the function will default to 0. This is
    intentional. If this was a mistake and you want to change this, you have to 
    do so manually in card_data.json *)
let int_input_helper () =
  match read_int_opt () with
  | None -> print_endline "setting to 0"; 0
  | Some s -> s

let list_input_helper () = 
  match read_line () with
  | exception End_of_file -> print_endline "setting to []"; "[]"
  | s -> "[" ^ s ^ "]"

(** continue_check_helper [f] will call the function f if the user input is 
    'y' or 'Y', or it will simply return unit (thus exiting) *)
let continue_check_helper (f: unit -> unit) = 
  match read_line () with
  | "y" | "Y" -> f ()
  | _ -> ()

(* Get the user input to record a money card *)
let rec money_card_input () =
  let _ = print_string "Enter the value of this money card\n" in
  let money_value = int_input_helper () in

  let _ = print_string "Enter the count of this money card\n" in
  let count_value = int_input_helper () in

  let json = "{\"value\": " ^ string_of_int money_value ^ ", \"count\": " 
             ^ string_of_int count_value ^ "}," in

  (* This is the mechanism to read to files. No need to understand this in detail, but 
     if you're curious, this is similar to how filestreams work in Java, JavaScript and Python. *)
  let oc = open_out_gen [Open_append] 0o666 file in (* Open out_channel, the octal specifies file permissions *)
  fprintf oc "%s\n" json;
  close_out oc; (* Close out_channel *)
  print_string("Enter another one? y/n \n");
  let _ = continue_check_helper money_card_input in
  ()

(* Get the user input to record an action card *)
let rec action_card_input () =
  print_string "Enter the name of this action card\n"; 
  let card_name = string_input_helper () in

  print_string "Enter the description of this action card\n";
  let card_description = string_input_helper () in

  print_string "Enter the value of this action card\n";
  let card_value = int_input_helper () in 

  print_string "Enter the count of this action card\n";
  let count_value = int_input_helper () in 

  let json = "{\"name\": " ^ card_name ^ ", \"desc\": " ^ card_description ^
             ", \"value\": " ^ string_of_int card_value ^ ", \"count\": " ^ 
             string_of_int count_value ^ "}," in

  let oc = open_out_gen [Open_append] 0o666 file in (* The octal specifies file permissions *)
  fprintf oc "%s\n" json;
  close_out oc;
  print_string("Enter another one? y/n \n");
  let _ = continue_check_helper action_card_input in
  ()

(* Get the user input to record a property card *)
let rec property_card_input () = 

  print_string "Enter the color of this property card set\n";
  let property_color = string_input_helper () in

  print_string "Enter the number of cards in this set\n";
  let set_count = int_input_helper () in

  print_string "Enter the value per card of this property card set\n";
  let property_value = int_input_helper () in

  print_string "Enter the list of rents. IMPORTANT: The input should be in the form\n\t\t [x1, x2, ..., xn], 
  \nwhere the xi'th entry denotes the entry when i members of this color set are possesed by the player.\n
  For example, since the light blue color set has 3 properties, and Connecticut Avenue is a light blue
  property card, this input when the venue is Connecticut Avenue will be [1,2,3]. Of course, this means that
  the length of the input list = number of property cards in the set. \n";
  let property_rents = list_input_helper () in

  let oc = open_out_gen [Open_append] 0o666 file in (* The octal specifies file permissions *)

  for x = 1 to set_count do
    print_string "Enter the name of a venue in this set\n";
    let venue = string_input_helper () in

    let json = "{\"venue\": " ^ venue ^ ", \"value\": " ^ 
               string_of_int property_value ^ ", \"color\": " ^ property_color ^ 
               ", \"rents\": " ^ property_rents ^ "}," in


    fprintf oc "%s\n" json;
  done ;
  close_out oc;
  print_string("Enter another set? y/n \n");
  let _ = continue_check_helper property_card_input in
  ()

(* Get the user input to record a  wildcard *)
let rec wildcard_input () =
  print_string "Enter the list of colors represented by this wildcard\n";
  let color_list = list_input_helper () in

  print_string "Enter the 2D list representing the rents for each color, in the order
  that you entered the color list. Inner [] needed. \n";
  let rent_list = list_input_helper () in 

  print_string "Enter the count of such cards\n";
  let wildcard_count = int_input_helper () in 

  print_string "Enter the value of this type of wildcard\n";
  let wildcard_value = int_input_helper () in 

  let json = "{\"colors\": " ^ color_list ^ ", \"rents\": " ^ rent_list 
             ^ ", \"count\": " ^ string_of_int wildcard_count ^ ", \"value\": " 
             ^ string_of_int wildcard_value ^ "}," in

  let oc = open_out_gen [Open_append] 0o666 file in (* The octal specifies file permissions *)
  fprintf oc "%s\n" json;
  close_out oc;
  print_string("Enter another one? y/n \n");
  let _ = continue_check_helper wildcard_input in
  ()

let rec rent_card_input () = 
  print_string "Enter the colors of this rent card as a list\n";
  let color_list = list_input_helper () in

  print_string "Enter the value of this rent card\n";
  let card_value = int_input_helper () in

  print_string "Enter the count of such cards\n";
  let card_count = int_input_helper () in

  let json = "\"colors\": " ^ color_list ^ ", \"value\": " ^ 
             string_of_int card_value  ^ ", \"count\": " ^ 
             string_of_int card_count ^ "}," in

  let oc = open_out_gen [Open_append] 0o666 file in (* The octal specifies file permissions *)
  fprintf oc "%s\n" json;
  close_out oc;
  print_string("Enter another one? y/n \n");
  let _ = continue_check_helper rent_card_input in
  ()

let rec main () =
  ANSITerminal.(print_string [cyan]
                  "Hey there! To enter a JSON type, enter a card type. 
                  Please enter all card types together, i.e, don't enter 
                  one money card and then a rent card and then a money card again.\n");

  ANSITerminal.(print_string [cyan]
                  "The card types are:
    \nmoney
    \naction
    \nproperty
    \nwildcard
    \nrent\n");

  let _ = match read_line () with
    | "money" -> money_card_input ();
    | "action" -> action_card_input ();
    | "property" -> property_card_input ();
    | "wildcard" -> wildcard_input ();
    | "rent" -> rent_card_input ();
    | _ -> ANSITerminal.(print_string [red] "Invalid card type, try again\n"); 
      main (); in


  print_string "Do you want to enter another card type?";
  let _ = continue_check_helper main in
  () 

let () = main ()