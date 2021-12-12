type marked = Unmarked | Marked ;;
type board = (int * marked) array array ;;
type input = { numbers: int list; boards: board list } ;;

let make_board () = 
  Array.make_matrix 5 5 (0, Unmarked)
;;

let read_draws line = 
  let parts = String.split_on_char ',' line in
  List.map (int_of_string) parts
;;

let read_board_line (board: board) (row_index: int) (line: string) =
  let raw_segments = String.split_on_char ' ' line in
  let segments = List.filter (fun x -> String.length x > 0) raw_segments in
  let numbers = List.map (int_of_string) segments in
  let rec process_row col_index row =
    match row with
      number::rest ->
        board.(row_index).(col_index) <- (number, Unmarked);
        process_row (col_index + 1) rest
    | [] -> ()
  in
  process_row 0 numbers
;;

let print_board (board: board) =
  for row = 0 to 4 do
    for col = 0 to 4 do
      let (number, marked) = board.(row).(col) in
      Printf.printf "%02d%s%s" number (if marked == Unmarked then " " else "*") (if col < 4 then " " else "")
    done;
    Printf.printf("\n")
  done
;;

let rec print_boards boards =
  match boards with
    board::rest ->
      print_board board;
      Printf.printf "\n";
      print_boards rest
  | [] -> ()
;;

let print_input numbers boards = 
  let rec print_numbers first numbers =
    match numbers with
      x::xs -> 
        if first then () else Printf.printf ", ";
        Printf.printf "%d" x;
        print_numbers false xs
    | [] -> Printf.printf "\n"
  in
  let rec print_boards boards =
    match boards with
      x::xs ->
        Printf.printf "\n";
        print_board x;
        print_boards xs
      | [] -> ()
  in
  print_numbers true numbers;
  print_boards boards
;;
let read_input chan : input =
  let first_line = ref true in
  let draws = ref [] in
  let current_board = ref (make_board ()) in
  let boards = ref [] in
  let board_row_index = ref 0 in
  let process_board_line line =
    read_board_line !current_board !board_row_index line;
    if !board_row_index == 4 then begin
      boards := !current_board::!boards;
      current_board := make_board ();
      board_row_index := 0
    end else
      board_row_index := !board_row_index + 1
  in
  let process_line line =
    if !first_line then begin
      draws := read_draws line;
      first_line := false
    end else
      process_board_line line
  in
  try
    while true do
      let line = input_line chan in
      if String.length line == 0 then ()
      else process_line line
    done;
    assert false
  with End_of_file -> {
    numbers = !draws;
    boards = List.rev !boards
  }
;;

let board_check_won (board: board): bool =
  let check_board fn = 
    let is_marked = ref true in
    for i = 0 to 4 do
      let (row, col) = fn i in
      let (_, marked) = board.(row).(col) in
      is_marked := (!is_marked) && (marked == Marked)
    done;
    !is_marked
  in
  (* check_board (fun i -> (i, i)) ||
  check_board (fun i -> (4-i, i)) || *)
  check_board (fun i -> (0, i)) ||
  check_board (fun i -> (1, i)) ||
  check_board (fun i -> (2, i)) ||
  check_board (fun i -> (3, i)) ||
  check_board (fun i -> (4, i)) ||
  check_board (fun i -> (i, 0)) ||
  check_board (fun i -> (i, 1)) ||
  check_board (fun i -> (i, 2)) ||
  check_board (fun i -> (i, 3)) ||
  check_board (fun i -> (i, 4))
;;

let board_sum_unmarked board =
  let sum = ref 0 in
  for i = 0 to 4 do
    for j = 0 to 4 do
      let (number, marked) = board.(i).(j) in
      if marked == Unmarked then
        sum := !sum + number
      else ()
    done
  done;
  !sum
;;

let board_mark_number (board: board) (drawn_number: int): unit =
  for i = 0 to 4 do
    for j = 0 to 4 do
      let (number, _) = board.(i).(j) in
      if number == drawn_number then 
        board.(i).(j) <- (number, Marked)
      else ()
    done
  done
;;

let board_clone (board: board): board =
  let new_board = make_board () in
  for i = 0 to 4 do
    for j = 0 to 4 do
      let (number, marked) = board.(i).(j) in
      new_board.(i).(j) <- (number, marked)
    done
  done;
  new_board
;;

let () = 
  let { numbers = numbers; boards = boards } = read_input stdin in
  let mark_on_all_boards number =
    let rec aux boards =
      match boards with
        board::rest ->
          board_mark_number board number;
          aux rest
      | [] -> ()
    in
    aux boards
  in
  let check_all_boards (): (int * board) list =
    let rec aux idx boards winners =
      match boards with
        board::rest ->
          if board_check_won board then
            aux (idx + 1) rest ((idx, board)::winners)
          else aux (idx + 1) rest winners
      | [] -> winners
    in
    aux 0 boards []
  in
  let first_winner = ref None in
  let winners = Hashtbl.create (List.length boards) in
  let loser = ref ((List.hd boards), 0) in
  let process_winner idx number board =
    match !first_winner with
      Some _ -> ()
    | None -> first_winner := Some (board, number);
    (match Hashtbl.find_opt winners idx with
      Some _ -> ()
    | None ->
        let sum = board_sum_unmarked board in
        Printf.printf "new board won, idx = %d, sum = %d, score = %d\n" idx sum (sum * number) ;
        print_board board;
        loser := ((board_clone board), number);
        Hashtbl.add winners idx true);
  in
  let rec draw_numbers round numbers =
    match numbers with
      number::rest ->
        Printf.printf "Round = %d, number = %d\n" round number;
        mark_on_all_boards number;
        let winners = check_all_boards () in
        ignore (List.map (fun (idx, board) -> process_winner idx number board) winners);
        draw_numbers (round + 1) rest
    | [] -> Printf.printf "No Winner :(\n";
  in
  draw_numbers 0 numbers;

  Printf.printf "First winner:\n";
  (match !first_winner with
    Some (board, number) ->
      print_board board;
      let sum = board_sum_unmarked board in
      Printf.printf "board sum = %d, score = %d\n" sum (sum * number);
  | None -> ());

  Printf.printf "\n";
  Printf.printf "Last winner:\n";
  let last_board, last_number = !loser in
  print_board last_board;
  let sum = board_sum_unmarked last_board in
  Printf.printf "board sum = %d, score = %d\n" sum (sum * last_number);
;;
