let read_input chan =
  let line = input_line chan in
  let segments = String.split_on_char ',' line in
  List.map (int_of_string) segments
;;

let process_day (fish: int list): int list =
  let rec aux fish next_gen =
    match fish with
      x::xs ->
        if x == 0 then aux xs (8::6::next_gen)
        else aux xs ((x - 1)::next_gen)
    | [] -> List.rev next_gen
  in
  aux fish []
;;

let print_fish fish =
  let rec aux fish =
    match fish with
      x::xs -> Printf.printf "%d " x; aux xs
    | [] -> ()
  in
  aux fish;
  Printf.printf "\n"
;;

let copy_fish fish =
  let rec aux fish acc =
    match fish with
      [] -> List.rev acc
    | x::xs -> aux xs (x::acc)
  in
  aux fish []
;;

let evolve_fish days fish =
  let pop = ref (copy_fish fish) in
  for _ = 1 to days do
    pop := process_day !pop;
    (* print_fish !pop *)
  done;
  List.length !pop
;;

let optimized_repr (fish: int list): int array =
  let pop = Array.make 9 0 in
  let rec aux fish =
    match fish with
      x::xs ->
        Array.set pop x ((Array.get pop x) + 1);
        aux xs
    | [] -> ()
  in
  aux fish;
  pop
;;

let process_day2 (pop: int array) =
  let num_next = (Array.get pop 0) in
  for i = 1 to 8 do
    Array.set pop (i-1) (Array.get pop i)
  done;
  Array.set pop 6 ((Array.get pop 6) + num_next);
  Array.set pop 8 num_next;
;;

let evolve_fish2 (days: int) (pop: int list) =
  let pop = optimized_repr pop in
  for _ = 1 to days do
    process_day2 pop
  done;
  Array.fold_left (fun acc item -> acc + item) 0 pop
;;

let () =
  let fish = read_input stdin in
  let scenario days =
    let num_fish = evolve_fish2 days fish in
    Printf.printf "day %d. population size  = %d\n\n" days num_fish
  in
  scenario 80;
  scenario 256
;;
