open Printf
let file = "card_data.json"

let rec money_card_input () =
  let _ = print_string "Enter the value of this money card\n" in
  let money_value = match read_int_opt () with
    | None -> print_endline "setting to 0"; 0
    | Some s -> s in

  let _ = print_string "Enter the count of this money card\n" in
  let count_value = match read_int_opt () with
    | None -> print_endline "setting to 0"; 0
    | Some s -> s in

  let json = "{\"value\": " ^ string_of_int money_value ^ ", \"count\": " ^ string_of_int count_value ^ "}," in
  let oc = open_out_gen [Open_append] 0o666 file in (* The octal specifies file permissions *)
  fprintf oc "%s\n" json;
  close_out oc;
  print_string("Enter another one? y/n \n");
  let _= match read_line () with
    | "y" | "Y" -> money_card_input ()
    | _ -> () in
  ()

let main () =
  ANSITerminal.(print_string [cyan]
                  "Hey there! To enter a JSON type, enter a card type. Please enter all card types together, i.e, don't enter one money card and then a rent card and then a money card again.\n");
  money_card_input
    ()

let () = main ()