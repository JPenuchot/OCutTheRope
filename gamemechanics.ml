(*	GAMEMECHANICS.ML
 *	
 *	Magic engine.
 *	Game physics functions are called here to iterate through the game.
 *)

open Gametypes
open Gamephysics
open Render

let iterate_player (sph, vel, modif) context =
	let nvel = vel_of_player (sph, vel, modif) context in
	let (np, nc) = handle_env_collision (sph, nvel, modif) context in
	let nnp = handle_rope_collision np in
	(nnp, nc)

let iterate_game context =
	let rec ig ctx rem =
		match rem with
		| Player(p)::tl	-> let (np, nc) = iterate_player p ctx in ig nc tl
		| _::tl			-> ig ctx tl
		| [] -> ctx
	in ig context context

let rec game_loop context =
	match context with
	| [] -> ()
	| _ -> (draw_level context true) ; let nc = (iterate_game context) in game_loop nc