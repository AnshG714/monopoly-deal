type command = 
  | Draw
  | Play of int
  | ViewPile
  | ViewHand
  | Pile of string
  | Discard of int
  | Pass
  | Quit

exception Malformed of string
exception Empty

(** [id_validator sl keyword] creates command [keyword] if [sl] is a list of
    valid integers. Raises Malformed otherwise. *)
let id_validator (sl: string list) (keyword: string) = 
  if List.length sl <> 1 then raise (Malformed ("please enter the " ^ keyword ^ " keyword followed by the id of the card you want to play."))
  else let el = List.nth sl 0 in
    match int_of_string_opt el with
    | None -> raise (Malformed "the id should be an integer.")
    | Some v ->  if keyword = "play" then Play v else Discard v



(** [parse_helper strlist] parses [strlist] into a command h where h is the 
    first element of [strlist]. *)
let parse_helper strlist = 
  let spaceless = List.filter (fun s -> s <> "") strlist in
  match spaceless with
  | [] -> raise Empty
  | h :: t -> 
    if h = "draw" then if t = [] then Draw else raise (Malformed "draw does not have any keywords after it.")
    else if h = "play" then id_validator t "play"
    else if h = "view" then (match t with
        | h :: [] -> if h = "pile" then ViewPile 
          else if h = "hand" then ViewHand 
          else raise (Malformed "Incorrect view command. You can either call view pile or view hand")
        | _ -> raise (Malformed "Incorrect command. Did you mean to call view hand or view pile?")
      )
    else if h = "pile" then let s = String.concat " " t in Pile s
    else if h = "discard" then id_validator t "discard"
    else if h = "pass" then if t = [] then Pass else raise (Malformed "pass does not have any keywords after it.")
    else if h = "quit" then if t = [] then Quit else raise (Malformed "quit does not have any keywords after it.")
    else raise (Malformed "Unknown keyword")

let parse str = 
  let strlist = String.split_on_char ' ' str in
  parse_helper strlist 