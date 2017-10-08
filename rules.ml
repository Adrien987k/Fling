type direction = Up | Right | Down | Left

(* A ball is a position and a tag that allow to regognized it *)
type ball = Ball of Position.t * int

(* A move is a ball and a direction to move it *)
type move = Move of ball * direction

(* A game is the list of all ball in its grid *)
type game = Game of ball list

(* A counter to tag the balls *)
let ball_counter = ref (-1)

(* Size of the grid *)
let max_x = 15
let max_y = 15

(* Returns the ball list of the game *)
let get_balls g =
	match g with
	| Game balls -> balls

(* Returns the position of the ball on the board *) 
let position_of_ball b =
	match b with
	| Ball (p, _) -> p

(* Makes a ball from a position and assign it a unique tag *)
let make_ball p =
	ball_counter := !ball_counter + 1; 
	Ball(p, !ball_counter)

(* Get the ball at the position p in the game
   Fail if there is no ball at this position *)
let ball_of_position g p =
	let balls = get_balls g in
	try
		List.find (fun b -> Position.eq (position_of_ball b) p) balls
	with
		Not_found -> failwith "Ball not found"

(* True if there is a ball at position p *) 
let is_ball g p =
    let balls = get_balls g in
    try
		let _ = List.find (fun b -> Position.eq (position_of_ball b) p) balls in
		true
	with
		Not_found -> false

(* Creates a new game from a ball list *)
let new_game bl = Game (bl)

(* Compares the tag of two ball to determine if these are the same *)
let eq_ball b b' =
	match b, b' with
	| Ball (p, n), Ball (p', n') -> n = n'

(* Makes a move from a ball and a direction *)
let make_move b d = Move (b, d)

(* True if the position p is in the grid *) 
let pos_in_grid p =
	Position.proj_x p >= 0 	   &&
	Position.proj_y p >= 0 	   &&
	Position.proj_x p <  max_x &&
	Position.proj_y p <  max_y

(* Returns the vector to add to a vector to move it the the direction d *)
let next_pos_from_dir d =
	match d with
	| Up -> Position.from_int 0 1
	| Right -> Position.from_int 1 0
	| Down -> Position.from_int 0 (-1)
	| Left -> Position.from_int (-1) 0

(* Returns the inverse of the direction d *)
let inverse_dir d = 
	match d with
	| Up -> Down
	| Right -> Left
	| Down -> Up
	| Left -> Right

(* Change the position of the ball b by p *)
let change_ball_pos b p = 
	match b with
	| Ball (p', n) -> Ball (p, n)

(* Returns a ball option
   An option with rhe ball from the ball b in the direction d if there is one 
   None otherwise *)
(*	
	For example 
	(dir_contain_ball g 1 Right) = Some 2
	(dir_contain_ball g 1 Down) = Some 3
	(dir_contain_ball g 1 Up) = None
	(dir_contain_ball g 2 Down) = None 
	1|-|-|2
	3|-|-|-
	-|-|-|-
*) 
let rec dir_contain_ball g b d =
	let dir_vect = (next_pos_from_dir d) in
	match b with
	| Ball (p, n) ->
		let p' = Position.move p dir_vect in
		if not (pos_in_grid p') then None else
		if is_ball g p' 
			then Some p'
		else
		dir_contain_ball g (change_ball_pos b p') d

(* Removes the ball b from the game g *)
let remove_ball g b = 
	let balls = get_balls g in
	let rec remove_ball_aux balls = 
	match balls with
	| [] -> []
	| b' :: q -> if eq_ball b b'
				 	then q 
				 else
				 	b' :: (remove_ball_aux q)
	in
	new_game (remove_ball_aux balls)

(* Returns a new game where [move] has been applied to [game] *)
let apply_move g move =
	(* Returns a new game where the ball b has been moved to the position p *)
	let move_ball g b p =
		let balls = get_balls g in
		(* The function given to List.map search for the ball and moves its position *)
		new_game (List.map (fun b' ->
					if Position.eq (position_of_ball b) (position_of_ball b') then
					   change_ball_pos b p
					else b'
			   	 		   )
			   	 		   balls
             	 )
	in
	(* Apply all the moves recursively. If first is true, the ball cannot move outside the grid *)
	let rec apply_move_aux g b d first =
        let p = position_of_ball b in
        (* If there is another ball just near the ball b in the direction d, the move cannot be applied *)
		if is_ball g (Position.move p (next_pos_from_dir d))
		   then g
		else
			(* check if there is a ball from b in the direction d *)
		    match (dir_contain_ball g b d) with
		    | None -> if first then g else remove_ball g b
		    | Some p' ->
		    	(* If a ball is found, move the ball b near the ball found *)
		    	let inv_d = inverse_dir d in
		    	let new_g = move_ball g b
		    				(Position.move p' (next_pos_from_dir inv_d))
		    	in
		    	(* Apply recursively the move to the ball found
		    	   With 'false' because this time the ball can go out of the grid *)
		    	apply_move_aux new_g (ball_of_position g p') d false
	in
	match move with
	| Move (b , d) -> apply_move_aux g b d true

(* Returns all the valid moves for the game g *)
let moves g =
  let balls = get_balls g in
  let rec moves_aux balls moves =
  	(* Try to add the move: 'move b in the direction d' to the move list 'moves' *)
    let add_move b d moves =
      match dir_contain_ball g b d with
      | None -> moves
      | Some _ -> if is_ball g (Position.move (position_of_ball b) (next_pos_from_dir d))
                    then moves
                  else
                    (make_move b d) :: moves
    in
    (* For each ball, try all the possibles moves *)
    match balls with
    | [] -> moves
    | b :: q -> let moves = add_move b Up moves in
                let moves = add_move b Down moves in
                let moves = add_move b Right moves in
                let moves = add_move b Left moves in
                moves_aux q moves
  in
  moves_aux balls []


(* Add a ball to a game 
   Only used for the Generation module *)
let add_ball game ball = 
	match game with
	| Game balls -> Game (ball :: balls)
