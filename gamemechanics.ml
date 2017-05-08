(*	GAMEMECHANICS.ML
 *	
 *	Magic engine.
 *	Game physics functions are called here to iterate through the game.
 *)

open Gametypes
open Gamephysics
open Basephysics
open Render

let iterate_player (sph, vel, modif) context =
	let nvel = vel_of_player (sph, vel, modif) context in
	let (pos, len) = sph in
	let npos = apply_der pos nvel dt in
	let (np, nc) = handle_env_collision ((npos, len), nvel, modif) context in
	let nnp = handle_rope_collision np in
	(nnp, nc)

(* Splits players and context into two different lists. *)
let sep_players context =
	let rec sp cx (pl, cn) =
		match cx with
		| Player(p)::tl	-> sp tl (p::pl, cn)
		| a::tl			-> sp tl (pl, a::cn)
		| [] -> (pl, cn)
	in sp context ([], [])

(* Puts players back into the context. *)
let rec fus_players pl cx =
	match pl with
	| p::tl -> fus_players tl (Player(p)::cx)
	| [] -> cx

(* Iterates players, updates context etc... *)
let iterate_game context =
	let (pl, cx) = sep_players context in
	let rec ig pl (apl, acx) =
		match pl with
		| p::tl	-> let (np, ncx) = iterate_player p acx in ig tl (np::apl, ncx)
		| []	-> fus_players apl acx
	in ig pl ([], cx)

let rec print_context ctx =
	match ctx with
	| Player(_)::tl			-> print_string "Player\n"; print_context tl
	| Goal(_)::tl			-> print_string "Goal\n"; print_context tl
	| GravField(_)::tl		-> print_string "GravField\n"; print_context tl
	| Star(_)::tl			-> print_string "Star\n"; print_context tl
	| Bubble(_)::tl			-> print_string "Bubble\n"; print_context tl
	| Attractor(_)::tl		-> print_string "Attractor\n"; print_context tl
	| Wall(_)::tl			-> print_string "Wall\n"; print_context tl
	| Monster(_)::tl		-> print_string "Monster\n"; print_context tl
	| [] -> ()

let rec game_loop context it =
	match context with
	| [] -> ()
	| _ -> if (it mod 1000 == 0)	then (draw_level context true)
								else ()
		;let nc = (iterate_game context) in game_loop nc (it + 1)

	(*| _ -> print_context context ; let nc = (iterate_game context) in game_loop nc (it + 1)*)