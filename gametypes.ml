(*	GAMETYPES.ML
 *	
 *	Where magic operates.
 *	Describes the game physics using high order functions
 *	defined in basephysics.ml.
 *)

open Basephysics

(* Describes a moifier on a player *)
type modifier =
	| Bubbled	of accel
	| Roped		of rope
type modifiers = modifier list

(* Describes a player *)
type player = sphere * vel * modifiers

(* Describes an object on the map *)
type gameObject =
	| Player	of player
	| Goal		of rect
	| GravField	of accel
	| Star		of sphere
	| Bubble	of sphere * accel
	| Attractor	of pos * float
	| Wall		of rect
	| Monster	of rect
	| Empty     of pos (* Empty object needed for exhaustive matching in level.ml:loadObject. Cannot being declared in a level file *)

(* A context (level state) is described by a list of objects. *)
type context = gameObject list
