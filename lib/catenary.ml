(*  CATENARY.ML
 *  
 *  This is the Graal!
 *  A function that returns a function to draw a catenary.
 *)

(* Return the formatted function to draw a curve from (x1, y1) to (x2, y2) with a given length *)
let getCatenaryFunction x1 y1 x2 y2 l =
    (* Compute z *)
    let rec computeZ z =
        if (((sinh z) /. z) < ((sqrt (l**2. -. (y2-.y1)**2.)) /. (x2-.x1))) then
            computeZ (z +. 0.001)
        else
            z
    in
    let z = computeZ 0.001 in
    (* Calculate the curve parameters *)
    let a = (x2 -. x1) /. 2. /. z in
    let p = (x1+.x2-.a*.(log ( (l+.y2-.y1) /. (l-.y2+.y1) ))) /. 2. in
    let q = (y2+.y1-.l*.(cosh z)/.(sinh z)) /. 2. in
    (* The function *)
    fun x ->
        a *. (cosh ((x-.p)/.a)) +. q