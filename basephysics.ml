(*	BASEPHYSICS.ML
 *	
 *	What magic relies on.
 *	Provides a set of tools for the physics engine such as
 *	vector algebra and other helper functions.
 *)

(* Base types *)

type size = float * float

type pos = float * float
type vel = float * float
type accel = float * float
type dim = float * float

type mass = float
type length = float

type accels = accel list

type sphere = pos * length
type rect = pos * size

type hitbox =
	| Sphere	of sphere
	| Rect		of rect

(* Vector addition *)
let ( +.. ) (xa, ya) (xb, yb) =
	(xa +. xb, ya +. yb)

(* Vector substraction *)
let ( -.. ) (xa, ya) (xb, yb) =
	(xa -. xb, ya -. yb)

(* Dot product *)
let ( *.. ) (xa, ya) (xb, yb) =
	(xa *. xb) +. (ya *. yb)

(* Scalar vector product *)
let ( **. ) a (x, y) =
	(a *. x, a *. y)

(* Scalar vector division *)
let ( //. ) (x, y) a =
	(x /. a, y /. a)

(* Returns the length of a vector *)
let len_of_vec (x, y) = 
	sqrt ((x *. x) +. (y *. y))

(* Normalizes the vector *)
let normalize (x, y) =
	let l = 1. /. (len_of_vec (x, y)) in
	l **. (x, y)

(* Returns the reflection of a vector with norm being the normal of the plan *)
let reflect norm inc =
	inc -.. ((2. *. (inc *.. norm)) **. norm)

(*	Apply the derivative of a component
 *	(ex. : compute new speed from current speed and acceleration,
 *	pos from current pos and speed, etc.)
 *)
let apply_der (x, y) (xx, yy) dt =
	(x +. (dt *. xx), y +. (dt *. yy))

(* COLLISION DETECTION *)

(* Checks for collision between a sphere and a rectangle. *)
let check_col_sr sp re =
	false

(* Checks for collision between two spheres. *)
let check_col_ss spa spb =
	let (posa, lena) = spa in
	let (posb, lenb) = spb in
	(len_of_vec (posb -.. posa)) <= (lena +. lenb)

(* Checks for collision between two rectangles. *)
let check_col_rr ((xa, ya),(wa, ha)) ((xb, yb),(wb, hb)) =
	   (((xa > xb && xa < xb +. wb) || (xa +. wa > xb && xa +. wa < xb +. wb))
	&&	((ya > yb && ya < yb +. hb) || (ya +. ha > yb && ya +. ha < yb +. hb)))
	||
	   (((xb > xa && xb < xa +. wa) || (xb +. wb > xa && xb +. wb < xa +. wa))
	&&	((yb > ya && yb < ya +. ha) || (yb +. hb > ya && yb +. hb < ya +. ha)))

let check_col_rope pl_pos rope =
	()

(* COLLISION HANDLING *)

let sr_collide sa sb vel =
	()
let rr_collide s r vel =
	()