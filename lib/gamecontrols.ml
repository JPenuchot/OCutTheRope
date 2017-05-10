(*	GAMECONTROLS.ML
 *
 *	Where magic interacts with humans.
 *)

open Gametypes
open Basephysics
open Graphics
open List

(* Checks for a collision between a rope and the mouse cursor. *)
let rope_mouse_col mpos ppos rope =
	let (rpos,_,_) = rope in
	(len_of_vec_sq (rpos -.. mpos)) < 400.

(* Checks for mouse collision with a player's modifiers and/or rope(s)
 * then outputs a new player *)
let player_mouse_col mpos ((spos, slen), vel, mods) =
	let slensq = slen ** 2. in
	let nmods = fold_left (fun acc elm ->
		match elm with
		| Bubbled(_) when (len_of_vec_sq (spos -.. mpos) < slensq) -> acc
		| Roped(r) when rope_mouse_col mpos spos r -> acc
		| v -> v::acc
	) [] mods
	in ((spos, slen), vel, nmods)

(* Checks for mouse collisions then outputs a new context. *)
let handle_mouse_col ctx =
	if button_down () then
		let (ixpos, iypos) = mouse_pos () in
		let (xpos, ypos) = ((float_of_int ixpos), (float_of_int iypos)) in
		fold_left (fun acc elm ->
			match elm with
			| Player(p) -> Player((player_mouse_col (xpos, ypos) p))::acc
			| v -> v::acc
		) [] ctx
	else
		ctx
