(*	PHYSICS.ML
 *	
 *	Where magic happens.
 *	Physics computations happen here.
 *)

(*	Defining a vector type that enables us
 *	to keep track of the kind of vector we're manipulating.
 *)
type vector =
			Pos of float * float
		| 	Vel of float * float
		| 	Acc of float * float
		|   For of float * float

let gravity = Acc(0., -0.9)

let sum_forces lst_f =
	let rec sf lst_f (xa, ya) =
		match lst_f with
		| For(x, y)::tl -> sf tl For(xa +. x, ya +. y)
		| [] -> For(xa, ya)
	in sf lst_f For(0., 0.)

(*let apply_force*)

(*let apply_speed*)