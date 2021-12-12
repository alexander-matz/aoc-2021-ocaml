exception No_input ;;
exception Invalid_input of (string * int) ;;

let parse_binary length str : bool array =
  if String.length str != length then raise (Invalid_input (str, length))
  else
    let output = Array.make length false in
    for idx = 0 to length - 1 do
      match str.[idx] with
        '0' -> Array.set output idx false
      | '1' -> Array.set output idx true
      | _ -> raise (Invalid_input (str, length))
    done;
    output
;;

let read_binay_numbers chan : (int * bool array list) =
  let lines = ref [] in
  try
    while true do
      let line = input_line chan in
      lines := line :: !lines
    done;
    assert false
  with End_of_file ->
    let rec build_matrix length lines acc =
      match lines with
        line::rest -> build_matrix length rest ((parse_binary length line)::acc)
      | [] -> List.rev acc
    in
    match !lines with
      first::rest ->
        let length = String.length first in
        (length, build_matrix length (first::rest) [])
    | [] -> raise (No_input)
;;

let rec print_matrix binaries =
  let print_binary binary =
    for idx = 0 to (Array.length binary) - 1 do
      Printf.printf "%s" (if Array.get binary idx then "1" else "0")
    done;
    Printf.printf "\n"
  in
  match binaries with
    x::rest -> print_binary x; print_matrix rest
  | [] -> ()
;;

let rates length binaries =
  let gamma = ref 0 in
  let epsilon = ref 0 in
  let threshold = (List.length binaries) / 2 in
  let rec compute_digit ones idx list =
    match list with
      x::rest ->
        let inc = if (Array.get x idx) then 1 else 0 in
        compute_digit (ones + inc) idx rest
    | [] -> if ones > threshold then 1 else 0
  in
  for idx = 0 to length - 1 do
    gamma := (Int.shift_left !gamma 1) + (compute_digit 0 idx binaries);
    epsilon := (Int.shift_left !epsilon 1) + (1 - (compute_digit 0 idx binaries))
  done;
  (!gamma, !epsilon)
;;


let int_of_binary binary =
  let length = (Array.length binary) in
  let result = ref 0 in
  let increment idx = 
    if (Array.get binary idx) then 1
    else 0
  in
  for idx = 0 to length - 1 do
    result := (Int.shift_left !result 1) + (increment idx)
  done;
  !result
;;

let string_of_binary binary =
  let length = Array.length binary in
  let buf = Buffer.create length in
  for idx = 0 to length - 1 do
    let ch = if (Array.get binary idx) then '1' else '0' in
    Buffer.add_char buf ch
  done;
  Buffer.contents buf
;;

exception No_values_left;;

type commonality = MostCommon | LeastCommon ;;

let split_binaries binaries (filtering: (commonality * int)) =
  let (wants_most_common, idx) = 
    match filtering with
      MostCommon, idx -> true, idx
    | LeastCommon, idx -> false, idx
  in
  let rec find_most_common ones zeros list =
    match list with
      value::rest ->
        if (Array.get value idx) then find_most_common (ones + 1) zeros rest
        else find_most_common ones (zeros + 1) rest
    | [] -> ones >= zeros
  in
  let most_common_bit = find_most_common 0 0 binaries in
  let filter value =
    let has_most_common = (Array.get value idx) == most_common_bit in
    if wants_most_common then has_most_common else not has_most_common
  in
  List.filter filter binaries
;;

let other_rates length binaries (which: commonality) =
  let rec find_rate next_idx list =
    match list with
      x::[] -> int_of_binary x
    | x::xs ->
      if next_idx >= length then raise (Invalid_argument "idx out of bounds")
      else
        find_rate (next_idx + 1) (split_binaries (x::xs) (which, next_idx))
    | [] -> raise (No_values_left)
  in
  find_rate 0 binaries
;;

let () = 
  let (length, binaries) = read_binay_numbers stdin in
  let (gamma, epsilon) = rates length binaries in
  Printf.printf "gamma rate: %d, epsilon rate: %d, product: %d\n" gamma epsilon (gamma * epsilon);
  let oxygen = other_rates length binaries MostCommon in
  let co2 = other_rates length binaries LeastCommon in
  Printf.printf "oxygen: %d, co2: %d, product: %d\n" oxygen co2 (oxygen * co2)
;;