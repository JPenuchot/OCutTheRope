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
	| Point
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

(* A context (level state) is described by a list of objects. *)
type context = gameObject list

(* Those are thrown when the game ends. They contain the number of points the player earned. *)
type kindEnd =
	| Win
	| Die
exception EndGame of kindEnd
