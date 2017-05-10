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
		(* The equation of the cutting line *)
		let cut =
			let slope = ((float_of_int npY) -. (float_of_int opY)) /. ((float_of_int npX) -. (float_of_int opX)) in
			Printf.printf "%f\n%!" slope;
			(fun x -> slope *. x +. (float_of_int opY))
		in
		(* TODO: Check intersection between cat(x) and cut(x) *)
		let rec check fromX toX =
			if (fromX = toX) then false
			else begin
				false
			end
		in

		Printf.printf "(%d, %d) -> (%d, %d)\n%!" opX opY npX npY;
		false
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
