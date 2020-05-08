(* [shuffle] shuffles the list [lst]. Assumes Random.self_init () has been called.*)
val shuffle: 'a list -> 'a list

(* [make_recurring_list] creates a list with element [el] repeated [count] times.*)
val make_recurring_list: 'a -> int -> 'a list