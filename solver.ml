
type node = Node of Rules.game * Rules.move option * node list

type graph = Graph of node

let rec make_node game m_opt =
  let moves = Rules.moves game in
  let rec compute_configs moves gml = 
    match moves with
    | [] -> gml
    | m :: q -> compute_configs q (((Rules.apply_move game m), m) :: gml)
  in
  let configs = compute_configs moves [] in
  Node (game, m_opt, List.map (fun gm' -> let g', m' = gm' in
                               make_node g' (Some m')
                           	  ) 
							  configs)

let make_game_graph game = Graph (make_node game None)

let solve game =
  let graph = make_game_graph game in
  let rec solve_aux node path =
    match node with
    | Node (g, m_opt, nl) ->
    	let balls = Rules.get_balls g in
    	if List.length balls = 1 then
    	  match m_opt with
    	  | None -> Some path
    	  | Some m -> Some (path @ [m])
    	else
    	  if List.length nl = 0 then None
    	  else 
    	  List.fold_left 
    	  (fun p_opt n ->
    	  	  match p_opt with
	    	  | None -> 
	    	      begin
	    	  	  match m_opt with
		    	  | None -> solve_aux n path
		    	  | Some m -> solve_aux n (path @ [m])
		    	  end
		      | Some p -> Some p
    	  )
    	  None nl
  in
  match graph with
  | Graph n -> solve_aux n []