(*	EDITOR.ML
 *	
 *	Creativity funnel.
 *	Allow you to create your own levels.
 *)

open Graphics
open Render
open Level
open Gametypes

(* Load a file if there is one given as a parameter *)
let level =
	(*Printf.printf "Loading level\n%!";*)
	if (Array.length Sys.argv) >= 2 then
		loadLevel Sys.argv.(1)
	else
		[]

(* Check if a player is in the level *)
let rec containsPlayer level =
	match level with
	| Player(_)::_ -> true
	| _::l         -> containsPlayer l
	| []           -> false

(* Function to draw each game object in the right menu *)
let draw_menu drawPlayer =
	if drawPlayer then draw_image player_sprite 535 620;
	draw_image bubble_sprite 535 540;
	draw_image star_sprite 535 460;
	draw_image attractor_sprite 535 380;
	draw_image wall_sprite 535 300;
	draw_image goal_sprite 522 170;
	draw_image monster_sprite 510 40

(* Detect if a point is in an given game object *)
let pointIsInObject pointX pointY o =
	let pX = float_of_int pointX in
	let pY = float_of_int pointY in
	match o with
	| Player((((x, y), radius), _, _))   -> sqrt((pX-.x)**2. +. (pY-.y)**2.) <= radius
	| Star(((x, y), radius))             -> sqrt((pX-.x)**2. +. (pY-.y)**2.) <= radius
	| Attractor((x, y), _)               -> sqrt((pX-.x)**2. +. (pY-.y)**2.) <= 25. (* Attractor has only the size of its sprite *)
	| Bubble(((x, y), radius), _)        -> sqrt((pX-.x)**2. +. (pY-.y)**2.) <= radius
	| Goal(((x, y), (width, height)))    -> pX >= x && pX <= (x +. width) && pY >= y && pY <= (y +. height)
	| Wall(((x, y), (width, height)))    -> pX >= x && pX <= (x +. width) && pY >= y && pY <= (y +. height)
	| Monster(((x, y), (width, height))) -> pX >= x && pX <= (x +. width) && pY >= y && pY <= (y +. height)
	| _                                  -> false

(* Detect the element under the mouse *)
let rec getPointedObject x y level =
	match level with
	| o::q -> if (pointIsInObject x y o) then (true, o) else getPointedObject x y q
	| [] -> (false, GravField(0.,0.)) (* Return a dummy object (will not be used due to false as first value) *)

(* Drag an objet until mouse released *)
let rec dragObject o level =
	Printf.printf "dragObject\n%!"

(* Main function, will be called reccursivly *)
let rec main level =
	try
		(* Draw the level without displaying it *)
		draw_level level false;
		
		(* Draw the games objects in the right menu *)
		draw_menu (not (containsPlayer level));
		synchronize ();
		
		(* Wait for a drag *)
		wait_next_event [Button_down];

		(* We must caught an exception because getPointedObjet may raise NoPointedObject *)
		let pointed = getPointedObject (fst (mouse_pos())) (snd (mouse_pos())) level in

		(* If we are on an object *)
		if fst pointed then
			dragObject (snd pointed) level;

		(* Reccursivly call the main editor function *)
		Printf.printf "Reccursivly\n%!";
		main level

	(* Caught the graphics exceptions (for example window closing) *)
	with Graphics.Graphic_failure(_) -> ()

let () =
	main level