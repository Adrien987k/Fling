
(* A node represent a configuration of a game with: 
   - The actual game
   - An optional move which is the move played to get to this configuration
   - The list of all configuration reachable in one step from this one *)
type node = Node of Rules.game * Rules.move option * node list

(* A graph is just a root node *)
type graph = Graph of node

(* Make a node representing the configuration of a game
   m_opt is the optional move made to get to this configuration *)
let rec make_node game m_opt =
  let moves = Rules.moves game in
  (* Make a (game, move) list with all configs reachable from this one in one step *)
  let rec compute_configs moves gml = 
    match moves with
    | [] -> gml
    | m :: q -> compute_configs q (((Rules.apply_move game m), m) :: gml)
  in
  let configs = compute_configs moves [] in
  (* Return a node with: 
     - The game 
     - The optional move 
     - the (game, move) list where each element have been replaced
       recursively by the corresponding node *)
  Node (game, m_opt, List.map (fun gm' -> let g', m' = gm' in
                               make_node g' (Some m')
                           	  ) 
							  configs)

(* Make the graph corresponding to a game *)
let make_game_graph game = Graph (make_node game None)

(* Solve the game [game] and return a optionnal list of moves to do it *)
let solve game =
  let graph = make_game_graph game in
  (* Solve a node and add the current move to path *)
  let rec solve_aux node path =
    match node with
    | Node (g, m_opt, nl) ->
      (* If the game is won, return the path *)
    	if Rules.win g then
    	  match m_opt with
    	  | None -> Some path
    	  | Some m -> Some (path @ [m])
      (* Else try all nodes of [node] *)
    	else
        (* If there is no other node, there is no solution, return None *)
    	  if List.length nl = 0 then None
        (* Else for all nodes try to find a solution *) 
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

(* return true if the game can be solved *)
let exist_solution game =
	match solve game with
	| None -> false
	| Some _ -> true