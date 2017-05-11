(*	GAMECONTROLS.ML
 *
 *	Where magic interacts with humans.
 *)

open Gametypes
open Basephysics
open Graphics
open List
open Catenary

(* Checks for a collision between a rope and the mouse cursor. *)
let rope_mouse_col (opX, opY) (npX, npY) (playerX, playerY) ((ropeX, ropeY), ropeLength, _) =
	if (opX <> npX || opY <> npY) then begin
		(* The equation of the catenary *)
		let cat = getCatenaryFunction playerX playerY ropeX ropeY ropeLength in
		let fopX = float_of_int opX in
		let fopY = float_of_int opY in
		let fnpX = float_of_int npX in
		let fnpY = float_of_int npY in
		(* The equation of the cutting line *)
		let cut =
			(fun x -> fopY +. ((fnpY -. fopY)/.(fnpX -. fopX))*.(x -. fopX))
		in
		(* TODO: Check intersection between cat(x) and cut(x) *)
		let rec check fromX toX =
			if (fromX = toX) then false
			else begin
				let cutY = cut fromX in
				let catY = cat fromX in
				if ((abs_float (cutY -. catY)) <= 1.) then true
				else check (fromX +. 1.) toX
			end
		in
		if (opX < npX) then
			check (float_of_int opX) (float_of_int npX)
		else
			check (float_of_int npX) (float_of_int opX)
	end
	else false

(* Checks for mouse collision with a player's modifiers and/or rope(s)
 * then outputs a new player *)
let player_mouse_col oldPos mpos ((spos, slen), vel, mods) =
	let slensq = slen ** 2. in
	let (npX, npY) = mpos in
	let fmpos = (float_of_int npX, float_of_int npY) in
	let nmods = fold_left (fun acc elm ->
		match elm with
		| Bubbled(_) when (len_of_vec_sq (spos -.. fmpos) < slensq) -> acc
		| Roped(r) when rope_mouse_col oldPos (npX, npY) spos r -> acc
		| _ -> elm::acc
	) [] mods
	in ((spos, slen), vel, nmods)

(* Checks for mouse collisions then outputs a new context. *)
let handle_mouse_col ctx x y =
	if button_down () then
		let (xpos, ypos) = mouse_pos () in
		fold_left (fun acc elm ->
			match elm with
			| Player(p) -> Player((player_mouse_col (x, y) (xpos, ypos) p))::acc
			| _ -> elm::acc
		) [] ctx
	else
		ctx
