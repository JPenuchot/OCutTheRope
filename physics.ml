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
type accel = float * float

type mass = float
type length = float

type accels = accel list

type rope = pos * length * bool
type ropes = rope list

let gravity = (0., -0.9)
let dt = 0.1

let ( +.. ) (xa, ya) (xb, yb) =
	(xa +. xb, ya +. yb)

let ( -.. ) (xa, ya) (xb, yb) =
	(xa -. xb, ya -. yb)

let ( *.. ) (xa, ya) (xb, yb) =
	(xa *. xb) +. (ya *. yb)

let ( ** ) a (x, y) =
	(a *. x, a *. y)

let ( // ) (x, y) a =
	(x /. a, y /. a)

let len_of_vec (x, y) = 
	sqrt ((x *. x) +. (y *. y))

let normalize (x, y) =
	let l = 1. /. (len_of_vec (x, y)) in
	l ** (x, y)

(*	Get the corresponding acceleration for an object of a given mass.	*)
let force_to_acc (xf, yf) mass =
	(xf /. mass, yf /. mass)

(*	Apply the derivative of a component
 *	(ex. : compute new speed from current speed and acceleration,
 *	pos from current pos and speed, etc.)
 *)
let apply_der (x, y) (xx, yy) =
	(x +. (dt *. xx), y +. (dt *. yy))

let reflect norm inc =
	inc -.. ((2. *. (inc *.. norm)) ** norm)

let attract obj_pos attr_pos attr_str =
	let pl_to_attr = normalize (obj_pos -.. attr_pos) in
	(attr_str /. len_of_vec (obj_pos -.. attr_pos)) ** pl_to_attr