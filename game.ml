open Player
open Card
open Deck
open Board
(* Models the interaction between the players and the board *)

type t = {
  player: player;
  deck: deck
}