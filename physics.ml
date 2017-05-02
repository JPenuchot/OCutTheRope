(*	PHYSICS.ML
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
type size = float * float

type pos = float * float
type vel = float * float
type acc = float * float
type force = float * float

type mass = float
type length = float
type radius = float

type forces = force list

let gravity = (0., -0.9)
let dt = 0.1

(*	Useful to make the sum of forces on an object.	*)
let sum_forces lst_f =
	let rec sf lst_f (xf, yf) =
		match lst_f with
		| (x, y)::tl -> sf tl (xf +. x, yf +. y)
		| [] -> (xf, yf)
	in sf lst_f (0., 0.)

(*	Get the corresponding acceleration for an object of a given mass.	*)
let force_to_acc (xf, yf) mass =
	(xf /. mass, yf /. mass)

(*	Apply the derivative of a component
 *	(ex. : compute new speed from current speed and acceleration,
 *	pos from current pos and speed, etc.)
 *)
let apply_der (x, y) (xx, yy) =
	(x +. (dt *. xx), y +. (dt *. yy))
