(*	EDITOR.ML
 *	
 *	Creativity funnel.
 *	Allow you to create your own levels ET TOUT CE QUI S'EN SUIT.
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

(* Update the position of a gameObject *)
let updatePosition o x y =
	match o with
	| Player(((_, a), b, c)) -> Player((((x, y), a), b, c))
	| Star((_, a))           -> Star(((x, y), a))
	| Attractor(_, a)        -> Attractor((x, y), a)
	| Bubble((_, a), b)      -> Bubble(((x, y), a), b)
	| Goal((_, a))           -> Goal(((x, y), a))
	| Wall((_, a))           -> Wall(((x, y), a))
	| Monster((_, a))        -> Monster(((x, y), a))
	| _                      -> o

(* Return the position of an object (left-bottom, not middle for circles) *)
let objectPosition o =
	match o with
	| Player((((x, y), _), _, _)) -> (int_of_float x, int_of_float y)
	| Star(((x, y), _))           -> (int_of_float x, int_of_float y)
	| Attractor((x, y), _)        -> (int_of_float x, int_of_float y)
	| Bubble(((x, y), _), _)      -> (int_of_float x, int_of_float y)
	| Goal(((x, y), _))           -> (int_of_float x, int_of_float y)
	| Wall(((x, y), _))           -> (int_of_float x, int_of_float y)
	| Monster(((x, y), _))        -> (int_of_float x, int_of_float y)
	| _                           -> (0, 0)

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

(* Drag an objet until mouse released
 * @params: o     The object to move
 *          level The current context
 *          rX    The X coord relative to the object
 *          rY    The Y coord relative to the object
 *)
let rec dragObject o level rX rY =
	let event = wait_next_event [Button_up; Mouse_motion] in
	(* If the mouse button is still pressed *)
	if event.button then begin
		(* Remove the object from the list and add the new moved object *)
		let newObject = updatePosition o (float_of_int ((event.mouse_x-rX)/5*5)) (float_of_int ((event.mouse_y-rY)/5*5)) in
		let newLevel = (List.filter (fun e -> e <> o) level)@[newObject] in
		(* Redraw the level *)
		draw_level level false;
		draw_menu (not (containsPlayer level));
		synchronize ();
		(* Reccursive call *)
		dragObject newObject newLevel rX rY
	end else level

(* Main function, will be called reccursivly *)
let rec main level =
	try
		(* Draw the level without displaying it *)
		draw_level level false;
		
		(* Draw the games objects in the right menu *)
		draw_menu (not (containsPlayer level));
		synchronize ();
		
		(* Wait for a drag *)
		let event = wait_next_event [Button_down] in

		(* We must caught an exception because getPointedObjet may raise NoPointedObject *)
		let pointed = getPointedObject (fst (mouse_pos())) (snd (mouse_pos())) level in

		(* If we are on an object *)
		if fst pointed then begin
			(* Calculate the mouse position relative to the object *)
			let objectPos = objectPosition (snd pointed) in
			(* Drag the object *)
			let newLevel = dragObject (snd pointed) level (event.mouse_x-(fst objectPos)) (event.mouse_y-(snd objectPos)) in
			(* Reccursive call on the new level *)
			main newLevel
		end else

			(* Reccursivly call the main editor function *)
			main level;

	(* Caught the graphics exceptions (for example window closing) *)
	with Graphics.Graphic_failure(_) -> ()

let () =
	main level