let shuffle lst = 
  let temp = List.map (fun c -> (Random.bits (), c)) lst in
  let sorted = List.sort compare temp in
  List.map snd sorted

let rec make_recurring_list (el: 'a) (count: int) = 
  if (count = 0) then [] else el :: make_recurring_list el (count - 1)