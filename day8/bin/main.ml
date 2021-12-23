type command = char list ;;

(*
0:6     1:2     2:5     3:5     4:4
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

 5:5     6:6     7:3     8:7     9:6
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
*)

let num_numbers = 10 ;;
let max_number = (num_numbers - 1) ;;

let explode s = List.init (String.length s) (String.get s)

let read_input chan: (command list * command list) =
  let order a b = (int_of_char a) - (int_of_char b) in
  let line = input_line chan in
  let segments = String.split_on_char '|' line in
  ignore (assert (List.length segments == 2));
  let no_empty x = String.length x > 0 in
  let inputs = List.filter no_empty (String.split_on_char ' ' (List.hd segments)) in
  let inputs = List.map (explode) inputs in
  let inputs = List.map (List.sort_uniq order) inputs in
  ignore (assert ((List.length inputs) == 10));

  let outputs = List.filter no_empty (String.split_on_char ' ' (List.nth segments 1)) in
  let outputs = List.map (explode) outputs in
  let outputs = List.map (List.sort_uniq order) outputs in
  ignore (assert ((List.length outputs) == 4));
  (inputs, outputs)
;;

let any_of (pred: 'a -> bool) (list: 'a list): bool =
  let rec aux list =
    match list with
      x::xs -> if (pred x) then true else aux xs
    | [] -> false
  in
  aux list
;;

let union (a: char list) (b: char list): char list =
  let is_char_from_b ch =
    any_of (fun x -> x == ch) b
  in
  let order (a: char) (b: char) =
    (int_of_char a) - (int_of_char b)
  in
  let rec aux_a a_list acc = 
    match a_list with
      x::xs -> if is_char_from_b x then aux_a xs (x::acc)
        else  aux_a xs acc
    | [] -> List.sort_uniq (order) acc
  in 
  aux_a a []
;;

let print_combinations (wires: char list array) =
  let rec print_charset first chars =
    match chars with
      x::xs -> 
        if not first then Printf.printf ", " else ();
        Printf.printf "%c" x;
        print_charset false xs
    | [] -> ()
  in
  for i = 0 to 9 do
    let charset = Array.get wires i in
    Printf.printf "%d: " i;
    print_charset true charset;
    Printf.printf "\n"
  done
;;

(* let get_command_by_number_of_segments (wires: char list array) (num_segments: int): (int * char list) =
  let rec aux idx =
    if idx > max_number then raise (Invalid_argument "out of bounds")
    else
      let command = (Array.get wires idx) in
      if List.length command == num_segments then (idx, command)
      else aux (idx + 1)
  in
  aux 0
;; *)

let get_command_by_number_of_segments (wires: char list list) (num_segments: int): (int * char list) =
  let rec aux idx list =
    match list with
      command::rest -> 
        if List.length command == num_segments then (idx, command)
        else aux (idx + 1) rest
    | [] -> raise (Invalid_argument ("number of elements not found in inputs: " ^ (string_of_int num_segments)))
  in
  aux 0 wires
;;

let print_command command = 
  let rec aux commands =
    match commands with
      x::xs ->
        Printf.printf "%c" x;
        aux xs
    | [] -> ()
  in
  aux command
;;

let get_segments digit: int list =
  match digit with
    0 -> [0; 1; 2; 4; 5; 6]
  | 1 -> [2; 5]
  | 3 -> [0; 2; 3; 5; 6]
  | 4 -> [1; 2; 3; 5]
  | 5 -> [0; 1; 3; 5; 6]
  | 6 -> [0; 1; 3; 4; 5; 6]
  | 7 -> [0; 2; 5]
  | 8 -> [0; 1; 2; 3; 4; 5; 6]
  | 9 -> [0; 1; 2; 3; 5; 6]
  | _ -> raise (Invalid_argument ("not between 0 and 9" ^ (string_of_int digit)))
;;

let solve_display (inputs: command list) (outputs: command list) =
  let wires = Array.make num_numbers [] in
  for i = 0 to max_number do
    Array.set wires i ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g']
  done;
  let is_solved idx =
    let ch = (Array.get wires idx) in
    let num_possbilities = List.length ch in
    assert (num_possbilities > 0);
    num_possbilities == 1
  in
  let all_solved () =
    let solved = ref true in
    for idx = 0 to max_number do
      solved := !solved && is_solved idx
    done;
    !solved
  in
  let constrain_digit digit charset =
    let existing = (Array.get wires digit) in
    let constrained = union existing charset in
    Array.set wires digit constrained
  in
  let constrain_by_num_segments digit num_segments =
    let (idx, charset) = get_command_by_number_of_segments inputs num_segments in
    let segments = get_segments digit in
    List.map (fun segment -> constrain_digit segment)
    Printf.printf "found candiate for segments. digit = %d, num_segments = %d, idx = %d, charset = " idx num_segments found_digit;
    print_command charset;
    Printf.printf "\n";
    constrain_digit idx charset
  in
  constrain_by_num_segments 7 3;
  constrain_by_num_segments 4 4;
  constrain_by_num_segments 1 2;
  print_combinations wires;
  ignore outputs; ignore (all_solved ());
;;

let () =
  let (inputs, outputs) = read_input stdin in
  solve_display inputs outputs
;;