(*	GAMEMECHANICS.ML
 *	
 *	Magic engine.
 *	Game physics functions are called here to iterate through the game.
 *)

open Gametypes
open Gamephysics
open Basephysics
open Render
open List

let iterate_player (sph, vel, modif) context =
	let nvel = vel_of_player (sph, vel, modif) context in
	let (pos, len) = sph in
	let npos = apply_der pos nvel dt in
	let (np, nc) = handle_env_collision ((npos, len), nvel, modif) context in
	(np, nc)

(* Splits players and context into two different lists. *)
let sep_players context =
	fold_left (fun (pl, cn) cx ->
		match cx with
		| Player(p)	-> (p::pl, cn)
		| a			-> (pl, a::cn)
	) ([], []) context

(* Puts players back into the context. *)
let fus_players pl cx =
	fold_left (fun cx p ->
	Player(p)::cx
	) cx pl

(* Iterates players, updates context etc... *)
let iterate_game context =
	let (pl, cx) = sep_players context in
	let rec ig pl (apl, acx) =
		match pl with
		| p::tl	-> let (np, ncx) = iterate_player p acx in ig tl (np::apl, ncx)
		| []	-> fus_players apl acx
	in ig pl ([], cx)

let print_context ctx =
	fold_left(fun _ v ->
		match v with
		| Player(_)		-> print_string "Player\n"
		| Goal(_)		-> print_string "Goal\n"
		| GravField(_)	-> print_string "GravField\n"
		| Star(_)		-> print_string "Star\n"
		| Bubble(_)		-> print_string "Bubble\n"
		| Attractor(_)	-> print_string "Attractor\n"
		| Wall(_)		-> print_string "Wall\n"
		| Monster(_)	-> print_string "Monster\n"
	) () ctx

let rec game_loop context it =
	match context with
	| [] -> ()
	| _ -> if (it mod 10000 == 0)	then (draw_level context true)
								else ()
		;let nc = (iterate_game context) in game_loop nc (it + 1)

	(*| _ -> print_context context ; let nc = (iterate_game context) in game_loop nc (it + 1)*)