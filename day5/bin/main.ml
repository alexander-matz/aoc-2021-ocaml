type pair = (int * int);;
type vent = { start: pair; stop: pair} ;;

let vent_regexp = Str.regexp "\\([0-9]+\\),\\([0-9]+\\) *-> *\\([0-9]+\\),\\([0-9]+\\)" ;;
let read_vent (line: string): vent =
  if Str.string_match vent_regexp line 0 then
    let start = ((int_of_string (Str.matched_group 1 line)), (int_of_string (Str.matched_group 2 line))) in
    let stop = ((int_of_string (Str.matched_group 3 line)), (int_of_string (Str.matched_group 4 line))) in
    { start = start; stop = stop }
  else raise (Invalid_argument ("invalid format for string '" ^ line ^ "'"))
;;

let read_input chan =
  let vents = ref [] in
  try
    while true do
      let line = input_line chan in
      if String.length line == 0 then ()
      else vents := (read_vent line)::!vents;
    done;
    assert false
  with End_of_file -> List.rev !vents
;;

let string_of_vent (vent: vent): string =
  let { start = (x1, x2); stop = (x3, x4) } = vent in
  Printf.sprintf "%d,%d -> %d,%d" x1 x2 x3 x4
;;

let print_vent (vent: vent) =
  Printf.printf "%s\n" (string_of_vent vent)
;;

let only_straight (vents: vent list): vent list =
  let filter value =
    let { start = (x1, y1); stop = (x2, y2) } = value in
    x1 == x2 || y1 == y2
  in
  List.filter filter vents
;;

let find_size (vents: vent list): int * int =
  let max3 v1 v2 v3 = max v1 (max v2 v3) in
  let rec aux width height vents =
    match vents with
      [] -> (width + 1, height + 1)
    | {start = (x1, y1); stop = (x2, y2)}::rest ->
      aux (max3 width x1 x2) (max3 height y1 y2) rest
  in
  aux 0 0 vents
;;

let signum x =
  match x with
    0 -> 0
  | x -> if x < 0 then -1 else +1
;;

let iterate_vent (fn: int -> int -> unit) (vent: vent) =
  let { start = (x1, y1); stop = (x2, y2)} = vent in
  let (dx, dy) = x2 - x1, y2 - y1 in
  if dx != 0 && dy != 0 then assert ((abs dx) == (abs dy))
  else ();
  let (sx, sy) = signum dx, signum dy in
  let rec aux cx cy = 
    fn cx cy;
    
    if cx == x2 && cy == y2 then ()
    else aux (cx + sx) (cy + sy)
  in
  aux x1 y1
;;

let make_floor width height =
  Array.make_matrix width height 0
;;

let print_floor floor =
  let width = (Array.length floor) in
  let height = (Array.length (Array.get floor 0)) in
  for row = 0 to height - 1 do
    for col = 0 to width - 1 do
      let value = floor.(col).(row) in
      if value == 0 then Printf.printf "."
      else Printf.printf "%d" value
    done;
    Printf.printf "\n"
  done;
;;

let count_crossings floor =
  let width = (Array.length floor) in
  let height = (Array.length (Array.get floor 0)) in
  let result = ref 0 in
  for row = 0 to height - 1 do
    for col = 0 to width - 1 do
      if floor.(col).(row) > 1 then result := !result + 1
      else ()
    done;
  done;
  !result
;;

let compute_crossings vents =
  let (width, height) = find_size vents in
  Printf.printf "width = %d, height = %d\n" width height;
  let floor = make_floor width height in
  let cb x y =
    let old = floor.(x).(y) in
    floor.(x).(y) <- old + 1
  in
  ignore (List.map (fun vent -> iterate_vent cb vent) vents);
  let crossings = count_crossings floor in
  Printf.printf "number of crossings: %d\n" crossings
;;

let () =
  let all_vents = read_input stdin in
  Printf.printf "Only straight vents:\n";
  compute_crossings (only_straight all_vents);
  Printf.printf "\n";
  Printf.printf "All vents:\n";
  compute_crossings all_vents
;;