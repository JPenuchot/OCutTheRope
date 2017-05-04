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
type rope = pos * length * float

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

(* Checks for collision between a sphere and the corner of a rectangle. *)
let check_col_corner_sr sp re =
	let (spos, srad) = sp in
	let ((rx, ry), (rw, rh)) = re in
	len_of_vec (spos -.. (rx, ry)) < srad ||
	len_of_vec (spos -.. (rx, ry +. rh)) < srad ||
	len_of_vec (spos -.. (rx +. rw, ry)) < srad ||
	len_of_vec (spos -.. (rx +. rw, ry +. rh)) < srad

(* Checks for collision between a sphere and the face of a rectangle. *)
let check_col_wall_sr sp re =
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

(* Checks for a rope collision. *)
let check_col_rope pl_pos rope =
	let (rope_pos, rope_len) = rope in
	len_of_vec (pl_pos -.. rope_pos) > rope_len

(* COLLISION HANDLING *)

let sr_wall_collide s r vel =
	if check_col_wall_sr s r then
		(s, vel)
	else
		(s, vel)

let sr_corner_collide s r vel =
	if check_col_corner_sr s r then
		
		let (spc, slen) = s in
		let ((x, y), (w, h)) = r in
		
		(* Check which corner was hit *)
		let corlist = [(x, y); (x, y+.h); (x+.w, y); (x+.w, y+.h)] in
		let rec mindist co acc =
			let (best, min) = acc in
			match co with
			| c::tl ->	let curr = len_of_vec (c -.. spc) in
						if curr < min then
							mindist tl (c, curr)
						else
							mindist tl acc
			| [] -> (best, min)
		in
		let (cor, dist) = mindist corlist ((0., 0.), max_float) in

		(* Update speed accordingly *)
		let norm = normalize (spc -.. cor) in
		let nvel = reflect norm vel in

		(* Then update sphere position *)
		let nspc = spc +.. ((slen -. dist) **. norm) in
		((nspc, slen),nvel)
	else
		(s, vel)

(* Manages collision between two spheres *)
let ss_collide sa sb vel dt =
	if check_col_ss sa sb then
		let (posa, lena) = sa in
		let (posb, lenb) = sb in
		let dist = posa -.. posb in
		let norm = normalize dist in
		let nvel = reflect norm vel in
		let ncoef = lena +. lenb -. (len_of_vec dist) in
		let npos = (ncoef **. norm) +.. (dt **. nvel) +.. posa in
		((npos, lena), nvel)
	else
		(sa, vel)

let rope_collide rope pos vel =
	()
