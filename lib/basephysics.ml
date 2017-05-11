(*  BASEPHYSICS.ML
 *  
 *  What magic relies on.
 *  Provides a set of tools for the physics engine such as
 *  vector algebra and other helper functions.
 *)

(* Base types *)

let solid_bounce_coef = 0.8

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
    | Sphere    of sphere
    | Rect      of rect

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

let len_of_vec_sq (x, y) = 
    (x ** 2.) +. (y ** 2.)

(* Returns the length of a vector *)
let len_of_vec (x, y) = 
    sqrt ((x *. x) +. (y *. y))

(* Normalizes the vector *)
let normalize (x, y) =
    let l = 1. /. (len_of_vec (x, y)) in
    l **. (x, y)
    

let string_of_pair (x, y) =
    "(" ^ (string_of_float x) ^ ", " ^ (string_of_float y) ^ ")"

(* Returns the reflection of a vector with norm being the normal of the plan *)
let reflect norm inc =
    inc -.. ((2. *. (inc *.. norm)) **. norm)

(*  Apply the derivative of a component
 *  (ex. : compute new speed from current speed and acceleration,
 *  pos from current pos and speed, etc.)
 *)
let apply_der (x, y) (xx, yy) dt =
    (x +. (dt *. xx), y +. (dt *. yy))

(* COLLISION DETECTION *)

(* Checks for collision between a sphere and the corner of a rectangle. *)
let check_col_corner_sr sp re =
    let (spos, srad) = sp in
    let srad_sq = srad ** 2. in
    let ((rx, ry), (rw, rh)) = re in
    len_of_vec_sq (spos -.. (rx, ry)) < srad_sq ||
    len_of_vec_sq (spos -.. (rx, ry +. rh)) < srad_sq ||
    len_of_vec_sq (spos -.. (rx +. rw, ry)) < srad_sq ||
    len_of_vec_sq (spos -.. (rx +. rw, ry +. rh)) < srad_sq

(* Checks for collision between a sphere and the face of a rectangle. *)
let check_col_wall_sr ((sx, sy), srad) ((rx, ry), (rw, rh)) =
    (* Centre entre les bornes du rectangle && Distance centre-mur < rayon *)
    ((sx > rx && sx < rx +. rw) && (((abs_float (sy -. ry) < srad)) || (abs_float (sy -. (ry +. rh)) < srad)))
        ||
    ((sy > ry && sy < ry +. rh) && (((abs_float (sx -. rx) < srad)) || (abs_float (sx -. (rx +. rw)) < srad)))
    (* Centre entre les bornes du rectangle && Distance centre-mur < rayon *)

(* Checks for collision between two spheres. *)
let check_col_ss spa spb =
    let (posa, lena) = spa in
    let (posb, lenb) = spb in
    (len_of_vec (posb -.. posa)) <= (lena +. lenb)

(* Checks for collision between two rectangles. *)
let check_col_rr ((xa, ya),(wa, ha)) ((xb, yb),(wb, hb)) =
       (((xa > xb && xa < xb +. wb) || (xa +. wa > xb && xa +. wa < xb +. wb))
    &&  ((ya > yb && ya < yb +. hb) || (ya +. ha > yb && ya +. ha < yb +. hb)))
    ||
       (((xb > xa && xb < xa +. wa) || (xb +. wb > xa && xb +. wb < xa +. wa))
    &&  ((yb > ya && yb < ya +. ha) || (yb +. hb > ya && yb +. hb < ya +. ha)))

(* Checks for a rope collision. *)
let check_col_rope pl_pos rope =
    let (rope_pos, rope_len) = rope in
    len_of_vec (pl_pos -.. rope_pos) > rope_len

(* COLLISION HANDLING *)

(* Triggered in case of corner collision *)
let sr_corner_collide s r vel =
    if check_col_corner_sr s r then
        let (spc, slen) = s in
        let ((x, y), (w, h)) = r in
        
        (* Check which corner was hit *)
        let corlist = [(x, y); (x, y+.h); (x+.w, y); (x+.w, y+.h)] in
        let rec mindist co acc =
            let (best, min) = acc in
            match co with
            | c::tl ->  let curr = len_of_vec (c -.. spc) in
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
        ((nspc, slen),solid_bounce_coef **. nvel)
    else
        (s, vel)

(* Triggered in case of wall collision *)
let sr_wall_collide s r vel =
    if check_col_wall_sr s r then
        let (spos, srad) = s in
        let ((rx, ry), (rw, rh)) = r in

        let corlist = [ ((rx,ry), 0);                   
                        ((rx,ry +. rh), 1);             
                        ((rx +. rw,ry), 2);             
                        ((rx +. rw,ry +. rh), 3)] in    
        
        let ((_,idv1), (_,idv2),_,_) = List.fold_left (fun acc elmt ->      (* Finding the two closest corners *)
            let ((cor1, idv1), (cor2, idv2), len1, len2) = acc in
            let (cor, idv) = elmt in
                let len = len_of_vec_sq (cor -.. spos) in
                if      (len < len1) then ((cor, idv), (cor1, idv1), len, len1)
                else if (len < len2) then ((cor1, idv1), (cor, idv), len1, len)
                else ((cor1, idv1), (cor2, idv2), len1, len2)
        ) (((0., 0.), 0), ((0., 0.), 0), max_float, max_float) corlist in
        
        let (sx, sy) = spos in  (* Computing normal and getting the sphere out of the rectangle. *)
        let (norm, dist) =
            if (idv1 == 0 && idv2 == 1) || (idv1 == 1 && idv2 == 0) then (* GAUCHE *)
                ((-1.,0.), (sx +. srad) -. rx)
            else if (idv1 == 0 && idv2 == 2) || (idv1 == 2 && idv2 == 0) then (* BAS *)
                ((0.,-1.), (sy +. srad) -. ry)
            else if (idv1 == 3 && idv2 == 1) || (idv1 == 1 && idv2 == 3) then (* HAUT *)
                ((0., 1.), ry +. rh -. (sy -. srad))
            else if (idv1 == 3 && idv2 == 2) || (idv1 == 2 && idv2 == 3) then (* DROITE *)
                ((1., 0.), rx +. rw -. (sx -. srad))
            else
                raise (Failure "Wrong corner computation in sphere/rectangle wall collision.")
        in
        let (nvel, nspos) = (reflect norm vel, spos +.. (dist **. norm)) in
        ((nspos, srad), solid_bounce_coef **. nvel)
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
        let npos = (ncoef **. norm) +.. posa in
        ((npos, lena), solid_bounce_coef **. nvel)
    else
        (sa, vel)

(* Handles rope collisions then returns acceleration. *)
let handle_rope_collision pos rope =    (* TODO *)
    let (ropos, rlen, rstr) = rope in
    let dist = len_of_vec (pos -.. ropos) in
    if dist > rlen then
        let norm = (normalize (ropos -.. pos)) in
        (rstr *. ((rlen -. dist) ** 2.)) **. norm
    else
        (0.,0.)

(* Attraction vector formula. *)
let attract obj_pos attr_pos attr_str =
    let pl_to_attr = normalize (attr_pos -.. obj_pos) in
    (attr_str /. (len_of_vec_sq (obj_pos -.. attr_pos))) **. pl_to_attr