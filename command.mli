(* Type for command. *)
type command = 
  | Draw
  | Play of int
  | ViewPile
  | ViewHand
  | Discard of int
  | Pass
  | Quit

exception Malformed of string
exception Empty

(** [parse str] parses [str] into a command if possible. Raises Empty if [str] 
    is empty. Raises Malformed if [str] is an invalid command. *)
val parse: string -> command
