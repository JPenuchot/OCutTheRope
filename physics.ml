(*	PHYSICS.ML
 *	
 *	Where magic happens.
 *	Physics computations happen here.
 *)

(*	Defining a vector type that enables us
 *	to keep track of the kind of vector we're manipulating.
 *)

let gravity = (0., -0.9)
let dt = 0.1

let sum_forces lst_f =
	let rec sf lst_f (xf, yf) =
		match lst_f with
		| (x, y)::tl -> sf tl (xf +. x, yf +. y)
		| [] -> (xf, yf)
	in sf lst_f (0., 0.)

let force_to_acc (xf, yf) mass =
	(xf /. mass, yf /. mass)

let apply_der (x, y) (xx, yy) =
	(x +. (dt *. xx), y +. (dt *. yy))
