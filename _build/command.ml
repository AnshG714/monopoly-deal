type objective_phrase = string list

type command = 
  | Draw
  | Play of objective_phrase
  | ViewPile
  | ViewHand
  | Discard of objective_phrase
  | Pass

exception Malformed of string
exception Empty

let parse_helper strlist = 
  let spaceless = List.filter (fun s -> s <> "") strlist in
  match spaceless with
  | [] -> raise Empty
  | h :: t -> 
    if h = "draw" then if t = [] then Draw else raise (Malformed "draw does not have any keywords after it.")
    else if h = "play" then if t <> [] then Play t else raise (Malformed "You need to follow the play keyword with the card you want to play.")
    else if h = "view" then (match t with
        | h :: [] -> if h = "pile" then ViewPile 
          else if h = "hand" then ViewHand 
          else raise (Malformed "Incorrect view command. You can either call view pile or view hand")
        | _ -> raise (Malformed "Incorrect command. Did you mean to call view hand or view pile?")
      )
    else if h = "discard" then if t <> [] then Discard t else raise (Malformed "You need to follow the discard keyword with the card you want to play.")
    else if h = "pass" then if t = [] then Pass else raise (Malformed "pass does not have any keywords after it.")
    else raise (Malformed "Unknown keyword")

let parse str = 
  let strlist = String.split_on_char ' ' str in
  parse_helper strlist 