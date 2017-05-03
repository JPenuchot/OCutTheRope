(*	PHYSICS.ML
 *	
 *	Where magic is summoned.
 *	Manages the game mechanics, uses physics.ml to compute
 *	physics stuff and render.ml to render stuff.
 *)

open Physics

type gameObject =
| Player	of pos * vel * ropes
| Goal	    of pos * size
| Monster   of pos * size
| GravField	of accel
| Star		of pos
| Bubble	of pos
| Attractor	of pos * float
| Rope		of pos * length
| Wall		of pos * size

(*	A context (level state) is described by a list of objects. *)
type context = gameObject list

(*	Computes acceleration	*)
let acc_of_context player_pos ctx =
	let rec aoc ctx acc =
		match ctx with
		| GravField(a)::tl			-> aoc tl (a::acc)
		| Attractor	(xy,str)::tl	-> aoc tl ((attract player_pos xy str)::acc)
		| _::tl						-> aoc tl acc
		| []						-> acc
	in aoc ctx []

(*	Computes speed	*)
let vel_of_context player_pos player_vel ctx =
	()	(* TODO *)

(*	Computes pos	*)
let pos_of_context =
	()	(* TODO *)