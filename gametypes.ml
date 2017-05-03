(*	PHYSICS.ML
 *	
 *	Where magic operates.
 *	Describes the game physics using high order functions
 *	defined in basephysics.ml.
 *)

open Basephysics

(* Describes a moifier on a player *)
type modifier =
	| Bubbled	of accel
	| Roped		of pos * length
type modifiers = modifier list

(* Describes a player *)
type player = pos * vel * modifiers

(* Describes an object on the map *)
type gameObject =
	| Player	of player
	| Goal		of pos * size
	| GravField	of accel
	| Star		of pos
	| Bubble	of pos * accel
	| Attractor	of pos * float
	| Wall		of pos * size
	| Monster	of pos * size

(* A context (level state) is described by a list of objects. *)
type context = gameObject list