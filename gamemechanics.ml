(*	GAMEMECHANICS.ML
 *	
 *	Magic engine.
 *	Game physics functions are called here to iterate through the game.
 *)

open Gametypes
open Gamephysics
open Gamecontrols
open Basephysics
open Render
open Thread
open List

let num_sims = 1000

(* Sends a new iteration of a player. *)
let iterate_player (sph, vel, modif) context =
	let nvel = vel_of_player (sph, vel, modif) context in
	let (pos, len) = sph in
	let npos = apply_der pos nvel dt in
	let (np, nc) = handle_env_collision ((npos, len), nvel, modif) context in
	(np, nc)

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

(* Manages timing for rendering calls etc. *)
let game_loop context =
	let nt = create (fun () -> delay (1. /. 60.)) () in
	
	let rec gloop context it t =
		if (it mod 10000 == 0) then	(* Creating a thread that will sleep during 1/60 sec *)
		(
			join t;
			draw_level context true;
			let newth = create (fun () -> delay (1. /. 60.)) () in
			let nc = (iterate_game context) in
			gloop nc (it + 1) newth
		)
		else
			let nc = (iterate_game context) in
			gloop nc (it + 1) t
	in gloop context 1 nt
