open Printf

(*
Flings files have a simple format:
Each line represent the coordinate x y of a ball
Example:
1 2
3 5
8 14
*)

(* Split a string representing two integers in two part by the space caracter 
   return a corresponding couple of integer *)
let split s =
  let i = String.index s ' ' in
  let first = String.sub s 0 i in
  let second = String.sub s (i+1) ((String.length s) - (i+1)) in
  Position.from_int (int_of_string first) (int_of_string second)

(* Load the file [filename] and return a game containing the corresponding data *)
let open_grid filename =
  let lines = ref [] in
  let chan = open_in filename in
  (* read the file line by line *)
  try
    while true; do
      lines := input_line chan :: !lines
    done;
    failwith "Error while reading the file"
  with End_of_file ->
    close_in chan;
    let lines = List.rev !lines in
    (* Transform the lines in a list of balls *)
    let balls = List.map (fun l -> Rules.make_ball (split l)) lines in
    Rules.new_game balls

(* Save the game [game] in the file 'gridResult.fl' *)
let save_grid game =
  let balls = Rules.get_balls game in
  (* Transform the ball list in a couple of numbers list *)
  let couples = List.map (fun b -> Rules.get_ball_x_y b) balls in
  let oc = open_out "gridResult.fl" in
  (* For each couple, write it in the file *)
  List.iter (fun c -> Printf.fprintf oc "%d %d\n" (fst c) (snd c)) couples;
  close_out oc;
