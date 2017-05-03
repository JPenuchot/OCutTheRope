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

type rope = pos * length * bool
type ropes = rope list

let gravity = (0., -0.9)
let dt = 0.1

(* Attraction vector formula *)
let attract obj_pos attr_pos attr_str =
	let pl_to_attr = normalize (obj_pos -.. attr_pos) in
	(attr_str /. len_of_vec (obj_pos -.. attr_pos)) ** pl_to_attr