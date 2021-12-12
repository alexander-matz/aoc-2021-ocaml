let read_commands chan =
  let commands = ref [] in
  try
    while true do
      let line = input_line chan in
      match String.split_on_char ' ' line with
        dir::num::[] ->
          let (command : string * int) = (dir, int_of_string num) in 
          commands := command :: !commands
      | _ -> ()
    done;
    assert false
  with End_of_file -> List.rev !commands ;;

exception Invalid_input of (string * int);;

let navigate (commands: (string * int) list) =
  let rec nav_step depth pos commands =
    match commands with
      ("forward", num)::rest -> nav_step depth (pos + num) rest
    | ("down", num)::rest -> nav_step (depth + num) pos rest
    | ("up", num)::rest -> nav_step (depth - num) pos rest
    | [] -> (depth, pos)
    | x::_ -> raise (Invalid_input x)
  in
  nav_step 0 0 commands;;

let navigate_aim (commands: (string * int) list) =
  let rec nav_step aim depth pos commands =
    match commands with
      ("forward", num)::rest -> nav_step aim (depth + aim * num) (pos + num) rest
    | ("down", num)::rest -> nav_step (aim + num) depth pos rest
    | ("up", num)::rest -> nav_step (aim - num) depth pos rest
    | [] -> (depth, pos)
    | x::_ -> raise (Invalid_input x)
  in
  nav_step 0 0 0 commands;;

(* let () =
  let commands = read_commands stdin in
  let (depth, pos) = navigate commands in
  Printf.printf "depth: %d, pos: %d, product: %d" depth pos (depth * pos) *)

let () =
  let commands = read_commands stdin in
  let (depth, pos) = navigate_aim commands in
  Printf.printf "depth: %d, pos: %d, product: %d" depth pos (depth * pos)