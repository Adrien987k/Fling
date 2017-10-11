
(* Generate a random grid of size max_x max_y with n ball *)
let rec random_generator n max_x max_y =
	(* Generate n differnts couples of int which represent positions *)
	let rec random_generator_aux n l = 
	if n = 0 then l else
	if n > 0 then 
		let x = Random.int max_x and y = Random.int max_y in
		let c = (x, y) in
		try
			let _ = List.find
					(fun e -> if e = c then true else false)
					l 
			in
			random_generator_aux n l
		with
		Not_found -> random_generator_aux (n - 1) (c :: l)
	else failwith "Error: Negative n"
	in
	let couples = random_generator_aux n [] in
	(* Transform the list of couples into a list of ball *)
	let balls = List.map 
				(fun c -> Rules.make_ball (Position.from_int (fst c) (snd c)))
				couples
	in
	let game = Rules.new_game balls in
	(* Try to solve the generated game 
	   If there is a solution return the game
	   Else retry 
	*)
	match Solver.solve game with
	| None -> random_generator n max_x max_y
	| Some _ -> game

(* Generate a random position *)
let random_position max_x max_y = 
	let x = Random.int max_x and y = Random.int max_y in
	Position.from_int x y
	
(* Generate a random ball *)
let random_ball max_x max_y =
	Rules.make_ball (random_position max_x max_y)

(* Generate a random game of size max_x max_y with n ball 
   by constructing a solution *)
let smart_generator n max_x max_y =
	if n < 0 then failwith "Error: Negative n" else
	let balls = [random_ball max_x max_y] in
	let game = Rules.new_game balls in
	(* Construct a solution by adding n-1 ball to g *)
	let rec construct_solution n g =
		if n = 1 then
			g
		else
			(* Try to add a ball with a random position 
			   (The generator is not that smart) *)
			let p = random_position max_x max_y in
			(* If there is already a ball at this position retry *) 
			if Rules.is_ball g p then
				construct_solution n g
			(* ELse add the ball and try to solve the game *)
			else
				(* *)
				let ball = Rules.make_ball p in
				let new_game = Rules.add_ball g ball in
				(* Retry with another ball if the new game have no solution *)
				match Solver.solve new_game with
				| None -> construct_solution n g
				| Some _ -> construct_solution (n - 1) new_game
	in
	construct_solution n game