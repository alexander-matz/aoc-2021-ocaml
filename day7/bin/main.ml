let read_input chan =
  let line = input_line chan in
  let segments = String.split_on_char ',' line in
  List.map (int_of_string) segments
;;

let nthroot n a =
  let nf = float n in let nf1 = nf -. 1.0 in
  let rec iter x =
     let x' = (nf1 *. x +. a /. (x ** nf1)) /. nf in
     if 0.001 > abs_float (x -. x') then x' else iter x' in
  iter 1.0
;;

let geometric_mean xs =
  let rec aux xs product len =
    match xs with
      x::xs -> aux xs (x * product) (len + 1)
    | [] -> nthroot len (float_of_int product)
  in
  aux xs 1 0
;;

let print_int_array arr = 
  let length = Array.length arr in
  for i = 0 to length - 1 do
    Printf.printf "%d " (Array.get arr i)
  done;
  Printf.printf "\n"
;;

let list_max list =
  List.fold_left (fun acc value -> max acc value) 0 list
;;

let summarize_distances numbers =
  let length = (list_max numbers + 1) in
  let distances = Array.make length 0 in
  let rec aux numbers =
    match numbers with
      x::xs ->
        Array.set distances x ((Array.get distances x) + 1);
        aux xs
    | [] -> ()
  in
  aux numbers;
  distances
;;

let prefix_scan thresholds =
  let length = Array.length thresholds in
  let scan = Array.make length 0 in
  Array.set scan 0 (Array.get thresholds 0);
  for i = 1 to length - 1 do
    Array.set scan i ((Array.get scan (i - 1)) + (Array.get thresholds i))
  done;
  Printf.printf("prefix scan: ");
  print_int_array scan;
  scan
;;

let guess1 distances_hist =
  let length = Array.length distances_hist in
  let fuel_cost pos = 
    let sum = ref 0 in
    if pos >= length then raise (Invalid_argument "out of bounds")
    else
      for i = 0 to length - 1 do
        let crabs_at_this_dist = (Array.get distances_hist i) in
        sum := !sum + (abs (pos - i) * crabs_at_this_dist)
      done;
    !sum
  in
  let min_fuel = ref Int.max_int in
  let min_dist = ref 0 in
  for dist = 0 to length - 1 do
    let fuel = fuel_cost dist in
    if fuel < !min_fuel then begin
      min_fuel := fuel;
      min_dist := dist;
    end else ();
  done;
  Printf.printf "min fuel = %d, at distance = %d\n" !min_fuel !min_dist
;;

let triangle n =
  let rec aux n acc =
    match n with
      0 -> 0
    | 1 -> acc
    | x -> aux (x - 1) (acc + x)
  in
  aux n 1
;;

let guess2 distances_hist =
  let length = Array.length distances_hist in
  let fuel_cost pos = 
    let sum = ref 0 in
    if pos >= length then raise (Invalid_argument "out of bounds")
    else
      for i = 0 to length - 1 do
        let crabs_at_this_dist = (Array.get distances_hist i) in
        sum := !sum + ((triangle (abs (pos - i))) * crabs_at_this_dist)
      done;
    !sum
  in
  let min_fuel = ref Int.max_int in
  let min_dist = ref 0 in
  for dist = 0 to length - 1 do
    let fuel = fuel_cost dist in
    if fuel < !min_fuel then begin
      min_fuel := fuel;
      min_dist := dist;
    end else ();
  done;
  Printf.printf "min fuel = %d, at distance = %d\n" !min_fuel !min_dist
;;


let calculate_stats numbers =
  let num_entries = List.length numbers in
  let rec aux numbers sum count =
    match numbers with
      x::xs -> aux xs (sum + x) (count + 1)
    | [] -> (sum, (float_of_int sum) /. (float_of_int count))
  in
  let sum, avg = aux numbers 0 0 in
  Printf.printf "sum = %d, avg = %f, avg * length = %f\n" sum avg (avg *. (float_of_int num_entries));
  Printf.printf "geo mean = %f\n" (geometric_mean numbers)
;;

let () =
  let positions = read_input stdin in
  calculate_stats positions;
  let distances = summarize_distances positions in
  Printf.printf "part1: "; guess1 distances;
  Printf.printf "part2: "; guess2 distances
;;
