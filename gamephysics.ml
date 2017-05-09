(*	GAMEPHYSICS.ML
 *	
 *	Where magic happens.
 *	Physics computations happen here.
 *)

open Basephysics
open Gametypes
open List

let dt = 0.00001
let friction_coef = 0.01

(* Attraction vector formula *)
let attract obj_pos attr_pos attr_str =
	let pl_to_attr = normalize (obj_pos -.. attr_pos) in
	(attr_str /. (len_of_vec (obj_pos -.. attr_pos) ** 2.)) **. pl_to_attr

(* Computes acceleration for a player given its position and the context *)
let acc_of_context ((pos, _), _, _) ctx =
	fold_left (fun acc c ->
		match c with
		| GravField(a)		-> (a +.. acc)
		| Attractor(xy,str)	-> ((attract pos xy str) +.. acc)
		| _					-> acc
	) (0., 0.) ctx

(* Computes acceleration for a given player given its velocity, position and modifiers *)
let acc_of_player_mod ((pos, _), vel, modifs) =
	let norm = normalize vel in
	let friction = (-1. *. friction_coef *. (len_of_vec_sq vel)) **. norm in
	let rec aopm mods acc =
		match mods with
		| Bubbled(a)::tl	-> aopm tl (a +.. acc)
		| Roped(r)::tl		-> aopm tl ((handle_rope_collision pos r) +.. acc)
		| _::tl				-> aopm tl acc
		| []				-> acc
	in aopm modifs friction

(* Computes the speed of a player given a context *)
let vel_of_player player ctx =
	let accel = (acc_of_context player ctx) +.. (acc_of_player_mod player) in
	let (_, vel, m) = player in
	apply_der vel accel dt

(* Handles environment collisions then returns a new player and context. *)
let handle_env_collision player context =
	let rec hec (player, nc) context =
		let (sph, vel, m) = player in
		match context with
		| Star(s)::tl when (check_col_ss sph s)										-> hec ((sph, vel, Point::m), nc) tl
		| Bubble(s, accel)::tl when (check_col_ss sph s)							-> hec ((sph, vel, (Bubbled(accel)::m)), nc) tl
		| Goal(r)::tl when (check_col_corner_sr sph r) || (check_col_wall_sr sph r) -> raise (EndGame(Win))
		| Wall(r)::tl ->
				let (nsph, nvel) = sr_corner_collide sph r vel in
				let (nnsph, nnvel) = sr_wall_collide nsph r nvel in
				hec ((nnsph, nnvel, m), Wall(r)::nc) tl
		| v::tl -> hec (player, (v::nc)) tl
		| []	-> (player, nc)
	in hec (player, []) context
