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

val parse: string -> command
