open Printf

let split s =
  let i = String.index s ' ' in
  let first = String.sub s 0 i in
  let second = String.sub s (i+1) ((String.length s) - (i+1)) in
  let _ = Printf.printf "%s " first in
  let _ = Printf.printf "%s\n" second in
  Position.from_int (int_of_string first) (int_of_string second)

let open_grid filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done;
    failwith "Error while reading the file"
  with End_of_file ->
    close_in chan;
    let lines = List.rev !lines in
    let balls = List.map (fun l -> Rules.make_ball (split l)) lines in
    Rules.new_game balls

let save_grid game =
  ()
