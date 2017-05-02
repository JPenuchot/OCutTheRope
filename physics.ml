(*	PHYSICS.ML
 *	
 *	Where magic happens.
 *	Physics computations happen here.
 *)

(*	Defining a vector type that enables us
 *	to keep track of the kind of vector we're manipulating.
 *)

let gravity = (0., -0.9)
let dt = .1

let sum_forces lst_f =
	let rec sf lst_f (xa, ya) =
		match lst_f with
		| (x, y)::tl -> sf tl (xa +. x, ya +. y)
		| [] -> (xa, ya)
	in sf lst_f (0., 0.)

let force_to_acc (xf, yf) mass =
	(xf /. mass, yf /. mass)

let apply_acc (xv, yv) (xa, ya) =
	(xv +. (dt *. xa), yv +. (dt *. ya))

let apply_speed (xp, yp) (xv, yv) =
	(xp +. (dt *. xv), yp +. (dt *. yv))