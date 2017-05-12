(*	GAMEMECHANICS.ML
 *	
 *	Magic engine.
 *	Game physics functions are called here to iterate through the game.
 *)

open Gamephysics
open Gamecontrols
open Basephysics
open Render
open Thread
open List
open Gametypes
open Graphics

let num_sims = 100

(* Sends a new iteration of a player. *)
let iterate_player (sph, vel, modif) context =
	(* Applying speed to the pos *)
	let nvel = vel_of_player (sph, vel, modif) context in
	let (pos, len) = sph in
	let npos = apply_der pos nvel dt in

	(* Handling environment collisions *)
	let (np, nc) = handle_env_collision ((npos, len), nvel, modif) context in
	(np, nc)

(* Iterates players, updates context etc... *)
let iterate_game context x y =
	let (pl, cx) = sep_players context in
	let rec ig pl (apl, acx) =
		match pl with
		| p::tl	-> let (np, ncx) = iterate_player p acx in ig tl (np::apl, ncx)
		| []	-> fus_players apl acx
	in let nc = ig pl ([], cx) in
	handle_mouse_col nc x y

(* Prints a context, mostly for debug... *)
let print_context ctx =
	iter(fun v ->
		match v with
		| Player(_)		-> print_string "Player\n"
		| Goal(_)		-> print_string "Goal\n"
		| GravField(_)	-> print_string "GravField\n"
		| Star(_)		-> print_string "Star\n"
		| Bubble(_)		-> print_string "Bubble\n"
		| Attractor(_)	-> print_string "Attractor\n"
		| Wall(_)		-> print_string "Wall\n"
		| Monster(_)	-> print_string "Monster\n"
		| RopeMaker(_)	-> print_string "RopeMaker\n"
	) ctx

(* Manages timing for rendering calls etc. *)
let game_loop context =
	let nt = create (fun () -> delay (1. /. 60.)) () in
	let (mouseX, mouseY) = mouse_pos () in
	let rec gloop context it t x y = (* x and y are the last mouse positions *)
		let (nX, nY) = mouse_pos () in
		if (it mod num_sims == 0) then	(* Creating a thread that will sleep during 1/60 sec *)
		(
			join t;
			draw_level_game context true;
			let newth = create (fun () -> delay (1. /. 60.)) () in
			let nc = (iterate_game context x y) in
			gloop nc (it + 1) newth nX nY
		)
		else
			let nc = (iterate_game context x y) in
			gloop nc (it + 1) t nX nY
	in gloop context 1 nt mouseX mouseY
