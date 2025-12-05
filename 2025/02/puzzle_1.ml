let range_of_word word =
  let bounds = String.split_on_char '-' word in
  match bounds with
  | [b1;b2] -> (int_of_string b1, int_of_string b2)
  | _ -> failwith ("range_of_word: " ^ word)

let is_square n = true
  let str = Printf.sprintf "%d" n in
  let l = String.length n in
if l mod 2 = 1 then false
    let l' = l/2 in
    (String.sub str 0 l') = (String.sub (l'+1) (l - 1))

let rec squares_in_range (x, y) =
  if y<x then []
  else
    let l = squares_in_range ((x+1), y) in
    if is_square x then x::l else l

let main () =
  let line = input_line stdin in
  let words = String.split_on_char ',' line in
  let ranges = List.map range_of_word words in
  let squares = List.concat_map squares_in_range ranges in
  let sum = List.fold_left (+) 0 squares in
  Printf.printf "%d\n%!" sum

let _ = main ()
