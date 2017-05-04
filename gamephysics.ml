(*	GAMEPHYSICS.ML
 *	
 *	Where magic happens.
 *	Physics computations happen here.
 *)

open Basephysics
open Gametypes

let gravity = (0., -0.9)
let dt = 0.1

(* Attraction vector formula *)
let attract obj_pos attr_pos attr_str =
	let pl_to_attr = normalize (obj_pos -.. attr_pos) in
	(attr_str /. (len_of_vec (obj_pos -.. attr_pos) ** 2.)) **. pl_to_attr

(* Computes acceleration for a player given its position and the context *)
let acc_of_context (pos, _, _, _) ctx =
	let rec aoc ctx acc =
		match ctx with
		| GravField(a)::tl			-> aoc tl (a +.. acc)
		| Attractor(xy,str)::tl		-> aoc tl ((attract pos xy str) +.. acc)
		| _::tl						-> aoc tl acc
		| []						-> acc
	in aoc ctx (0., 0.)

(* Computes acceleration for a given player given its velocity, position and modifiers *)
let acc_of_player_mod (pos, vel, len, modifs) =
	let rec aopm mods acc =
		match mods with
		| Bubbled(a)::tl	-> aopm tl (a +.. acc)
		| _::tl				-> aopm tl acc
		| []				-> acc
	in aopm modifs (0., 0.)

(* Computes the speed of a player given a context *)
let vel_of_player player ctx =
	let accel = (acc_of_context player ctx) +.. (acc_of_player_mod player) in
	let (_, vel, _, _) = player in
	apply_der vel accel dt

let collide a b =
	match a, b with
	| Sphere(a), Sphere(b)	-> check_col_ss a b
	| Sphere(a), Rect(b)	-> check_col_sr a b
	| Rect(a), Sphere(b)	-> check_col_sr b a
	| Rect(a), Rect(b)		-> check_col_rr a b

(* Handles environment collisions then returns a new player. *)
let rec handle_env_collision player context = ()(*
	let (sph, vel, _) = player in
	let (pos, vel) = 
	match context with
	| Star(s)::tl			when collide Sphere(sph) Sphere(s) -> sr_collide sph s vel
	| Bubble(s, accel)::tl	when collide Sphere(sph) Sphere(s) -> sr_collide sph s vel
	| Goal(r)::tl			when collide Sphere(sph) Rect(r)   -> rr_collide sph r vel
	| Wall(r)::tl			when collide Sphere(sph) Rect(r)   -> rr_collide sph r vel
	| _::tl -> handle_collision player tl
	| []	-> player *)

let handle_rope_collision player = ()
	(*let ((pos, _), velo, modifs) = player in
	let rec hrc pos vel md =*)


(* Computes new player *)
let iterate_player player =
	()