(*	GAMEPHYSICS.ML
 *	
 *	Where magic happens.
 *	Physics computations happen here.
 *)

(*	Defining a vector type that enables us
 *	to keep track of the kind of vector we're manipulating.
 *)

(*	Expliciting types to make sure our code
 *	doesn't depend on the game mechanics.
 *)

open Basephysics
open Gametypes

type rope = pos * length * bool
type ropes = rope list

let gravity = (0., -0.9)
let dt = 0.1

(* Attraction vector formula *)
let attract obj_pos attr_pos attr_str =
	let pl_to_attr = normalize (obj_pos -.. attr_pos) in
	(attr_str /. (len_of_vec (obj_pos -.. attr_pos) ** 2.)) **. pl_to_attr

(* Computes acceleration for a player given its position and the context *)
let acc_of_context (pos, _, _) ctx =
	let rec aoc ctx acc =
		match ctx with
		| GravField(a)::tl			-> aoc tl (a +.. acc)
		| Attractor(xy,str)::tl		-> aoc tl ((attract pos xy str) +.. acc)
		| _::tl						-> aoc tl acc
		| []						-> acc
	in aoc ctx (0., 0.)

(* Computes acceleration for a given player given its velocity, position and modifiers *)
let acc_of_player_mod (pos, vel, modifs) =
	let rec aopm modifs acc =
		match modifs with
		| Bubbled(a)::tl	-> aopm tl (a +.. acc)
		| _::tl				-> aopm tl acc
		| []				-> acc
	in aopm modifs (0., 0.)

(* Computes the speed of a player given a context *)
let vel_of_player player ctx =
	let accel = (acc_of_context player ctx) +.. (acc_of_player_mod player) in
	let (_, vel, _) = player in
	apply_der vel accel dt

(* Computes pos *)
let pos_of_player player  =
	apply_der  

(* Handles collision *)
let handle_wall_collision =
	() (* TODO *)

(* Handles object grabbing *)
let grab =
	()	(* TODO *)
