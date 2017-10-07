type direction = Up | Right | Down | Left

type ball = Ball of Position.t * int

type move = Move of ball * direction

type game = Game of ball list

exception Empty

let ball_counter = ref (-1)

let max_x = 15

let max_y = 15

let get_balls g =
	match g with
	| Game balls -> balls


let position_of_ball b =
	match b with
	| Ball (p, _) -> p

let make_ball p =
	ball_counter := !ball_counter + 1; 
	Ball(p, !ball_counter)

let ball_of_position game p =
	let balls = get_balls game in
	let rec ball_of_position_aux balls p =
		match balls with
		| [] -> raise Empty
		| b :: q ->
			if Position.eq (position_of_ball b) p
				then b
			else
				ball_of_position_aux q p
	in
	try
	        ball_of_position_aux balls p
	with
		Empty -> failwith "Ball not found"

let is_ball g p =
      let balls = get_balls g in
      let rec is_ball_aux balls p =
      match balls with
      | [] -> false
      | b :: q -> if Position.eq p (position_of_ball b)
                     then true
                  else
                     is_ball_aux q p
      in
      is_ball_aux balls p

let new_game bl = Game (bl)

let eq_ball b b' =
	match b, b' with
	| Ball (p, n), Ball (p', n') -> n = n'

let make_move b d = Move (b, d)

let pos_in_grid p =
	Position.proj_x p >= 0 	   &&
	Position.proj_y p >= 0 	   &&
	Position.proj_x p <  max_x &&
	Position.proj_y p <  max_y

let next_pos_from_dir d =
	match d with
	| Up -> Position.from_int 0 1
	| Right -> Position.from_int 1 0
	| Down -> Position.from_int 0 (-1)
	| Left -> Position.from_int (-1) 0

let inverse_dir d = 
	match d with
	| Up -> Down
	| Right -> Left
	| Down -> Up
	| Left -> Right

let change_ball_pos b p = 
	match b with
	| Ball (p', n) -> Ball (p, n)

let rec dir_contain_ball g b d =
	let dir_vect = (next_pos_from_dir d) in
	match b with
	| Ball (p, n) ->
		let p' = Position.move p dir_vect in
		if not (pos_in_grid p') then None else
		if is_ball g p' then Some p' else
		dir_contain_ball g (change_ball_pos b p') d

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

let apply_move g move =
	let move_ball g b p =
		let balls = get_balls g in
		new_game (List.map (fun b' ->
					if Position.eq (position_of_ball b) (position_of_ball b') then
					   change_ball_pos b p
					else b'
			   	 		   )
			   	 		   balls
             	 )
	in
	let rec apply_move_aux g b d first =
        let p = position_of_ball b in
		if not (is_ball g p) ||
		   is_ball g (Position.move p (next_pos_from_dir d))
		   then g
		else
		    match (dir_contain_ball g b d) with
		    | None -> if first then g else remove_ball g b
		    | Some p' ->
		    	let inv_d = inverse_dir d in
		    	let new_g = move_ball g b
		    				(Position.move p' (next_pos_from_dir inv_d))
		    	in
		    	apply_move_aux new_g (ball_of_position g p') d false
	in
	match move with
	| Move (b , d) -> apply_move_aux g b d true

let moves g =
  let balls = get_balls g in
  let rec moves_aux balls moves =
    let add_move b d moves =
      match dir_contain_ball g b d with
      | None -> moves
      | Some _ -> if is_ball g (Position.move (position_of_ball b) (next_pos_from_dir d))
                    then moves
                  else
                    (make_move b d) :: moves
    in
    match balls with
    | [] -> moves
    | b :: q -> let moves = add_move b Up moves in
                let moves = add_move b Down moves in
                let moves = add_move b Right moves in
                let moves = add_move b Left moves in
                moves_aux q moves
  in
  moves_aux balls []
