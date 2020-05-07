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

val parse: string -> command
