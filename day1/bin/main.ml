let get_input =
  let ints = ref [] in
  try
    while true do
      let line = input_line stdin in
        ints := int_of_string line :: !ints
    done;
    assert false
  with End_of_file -> List.rev !ints ;;

let count_increases list =
  let rec count_aux count list =
    match list with
      x1::x2::xs ->
        if x2 > x1 then count_aux (count + 1) (x2::xs)
        else count_aux count (x2::xs)
    | _ -> count
  in
    count_aux 0 list ;;

let sliding_window list =
  let rec window_aux list acc =
    match list with
      x1::x2::x3::xs -> window_aux (x2::x3::xs) (x1 + x2 + x3::acc)
    | _ -> List.rev acc
  in
  window_aux list []

(* let () =
  let numbers = get_input in
  Printf.printf "number of increases: %d" (count_increases numbers) *)

let () =
  let numbers = get_input in
  let window = sliding_window numbers in
  Printf.printf "number of increases: %d" (count_increases window)
