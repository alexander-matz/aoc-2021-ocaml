let read_row line = 
  let seq = Seq.map (int_of_char) (String.to_seq line) in
  List.of_seq seq
;; 

let read_input chan = 
  let rows_acc = ref [] in
  let rows = 
    try
      while true do
        let line = input_line chan in
        if String.length line == 0 then ()
        else rows_acc := (read_row line)::!rows_acc;
      done;
      assert false
    with End_of_file -> List.rev !rows_acc in
  let num_columns = List.length (List.hd rows) in
  let num_rows = List.length rows in
  let array = Array.make_matrix num_columns num_rows 0 in
  List.iteri (fun row_idx row ->
    List.iteri (fun col_idx value ->
        array.(col_idx).(row_idx) <- value ) row) rows;
  array
;;

let () = print_endline "Hello, World!"
