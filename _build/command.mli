type command = 
  | Draw
  | Play of int
  | ViewPile
  | ViewHand
  | Discard of int
  | Pass


exception Malformed of string
exception Empty

val parse: string -> command
