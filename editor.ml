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
		failwith "You must specify a level file!"

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

(* Detect the element under the mouse *)
let rec getPointedObject x y level =
	(* Fold from left to find the last object pointed in the list *)
	List.fold_left (fun last el -> if (pointIsInObject x y el) then (true, el) else last) (false, GravField(0.,0.)) level

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
		(* Create the new moved object (only on a mutiple of 5) *)
		let newObject = updatePosition o (float_of_int ((event.mouse_x-rX)/5*5)) (float_of_int ((event.mouse_y-rY)/5*5)) in
		(* Redraw the level *)
		draw_level level false;
		draw_menu (not (containsPlayer level));
		synchronize ();
		(* Reccursive call with the context where the old object is replaced with the new one *)
		dragObject newObject ((removeFromLevel o level)@[newObject]) rX rY
	end else level

(* Search for a click an a new object *)
let checkNewObject level =
	let pX = fst (mouse_pos()) in
	let pY = snd (mouse_pos()) in
	if ((not (containsPlayer level)) && (pointIsInObject pX pY (Player(((560.,645.),25.),(0.,0.),[])))) then
		let newObject = Player(((560.,645.),25.),(0.,0.),[]) in
		dragObject newObject (level@[newObject]) (pX-560) (pY-645)
	else if (pointIsInObject pX pY (Star(((560., 485.), 25.)))) then
		let newObject = Star(((560., 485.), 25.)) in
		dragObject newObject (level@[newObject]) (pX-560) (pY-485)
	else if (pointIsInObject pX pY (Attractor((560.,405.),0.))) then
		let newObject = Attractor((560.,405.),0.) in
		dragObject newObject (level@[newObject]) (pX-560) (pY-405)
	else if (pointIsInObject pX pY (Bubble(((560.,565.),25.),(0.,0.)))) then
		let newObject = Bubble(((560.,565.),25.),(0.,0.)) in
		dragObject newObject (level@[newObject]) (pX-560) (pY-565)
	else if (pointIsInObject pX pY (Goal(((522.,170.),(75.,100.))))) then
		let newObject = Goal(((522.,170.),(75.,100.))) in
		dragObject newObject (level@[newObject]) (pX-522) (pY-170)
	else if (pointIsInObject pX pY (Wall(((535.,300.),(50.,50.))))) then
		let newObject = Wall(((535.,300.),(50.,50.))) in
		dragObject newObject (level@[newObject]) (pX-535) (pY-300)
	else if (pointIsInObject pX pY (Monster(((510.,40.),(100.,100.))))) then
		let newObject = Monster(((510.,40.),(100.,100.))) in
		dragObject newObject (level@[newObject]) (pX-510) (pY-40)
	else
		level

(* Remove the objects out of the level 
 * Warning: not terminal reccursive :'( *)
let rec removeOutObjects level =
	match level with
	| o::q -> (
		let pos = objectPosition o in
		if (fst pos) < 0 || (fst pos) > 500 || (snd pos) < 0 || (snd pos) > 700 then
			removeOutObjects q
		else
			o::(removeOutObjects q)
	)
	| []   -> level

(* Main function, will be called reccursivly *)
let rec main level =
	try

		(* Remove objects out of the level *)
		let level =
			removeOutObjects level
		in

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

			(* Check if we must add a new game object and reccursivly call the main editor function *)
			main (checkNewObject level);

	(* Caught the graphics exceptions (for example window closing) *)
	with Graphics.Graphic_failure(_) ->
		(* Save the level (and add a gravity field) *)
		let isGravity =
			List.exists (fun e -> match e with | GravField(_) -> true | _ -> false) level
		in
		if isGravity then
			saveLevel level Sys.argv.(1)
		else
			saveLevel ((GravField((0.,-1.)))::level) Sys.argv.(1)

let () =
	main level