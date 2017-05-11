(*	EDITOR.ML
 *	
 *	Creativity funnel.
 *	Allow you to create your own levels ET TOUT CE QUI S'EN SUIT.
 *)

open Graphics
open Render
open Level
open Gametypes
open Http
open Scripts

(* Default values. *)
let default_grav = (0., -1.)
let default_bubble_grav = (0., 1.15)
let default_rope_str = 1.
let default_att_str = 3000.

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
	moveto 520 680;
	draw_string "Upload level";
	if drawPlayer then
		draw_image player_sprite 535 620
	else
		fill_circle 560 645 5;
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

(* Detect the rope origin under the mouse *)
let getPointedRope x y m =
	List.fold_left (
		fun last el ->
			match el with
			| Roped(((rX, rY), _, _)) -> if (sqrt((rX-.x)**2. +. (rY-.y)**2.) <= 5.) then (true, el) else last
			| _                       -> last
	) (false, Point) m

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
		draw_level_editor level false;
		draw_menu (not (containsPlayer level));
		synchronize ();
		(* Reccursive call with the context where the old object is replaced with the new one *)
		dragObject newObject ((removeFromLevel o level)@[newObject]) rX rY
	end else level

(* Update the posotion of a rope *)
let updateRopePosition r nX nY p =
	(* We need the player position to calculate the length of the rope *)
	let playerPos = objectPosition p in
	(* Do it! *)
	match r with
	| Roped((_, _, b)) -> Roped(((nX, nY), (sqrt (((float_of_int (fst playerPos)) -. nX)**2. +. ((float_of_int (snd playerPos)) -. nY)**2.))+.50., b))
	| _                -> r

(* Replace a rope in a player *)
let replaceRope oldRope newRope p =
	(* Creates the new list *)
	let lastModifiers = match p with | Player((_, _, l)) -> l | _ -> [] in
	let newModifiers = (removeModifier oldRope lastModifiers)@[newRope] in
	(* Create the new player *)
	match p with
	| Player((a, b, _)) -> Player((a, b, newModifiers))
	| _                 -> p

(* Add a rope to a player *)
let addRope newRope p =
	match p with
	| Player((a, b, c)) -> Player((a, b, (c@[newRope])))
	| _                 -> p

(* Remove ropes out for a player *)
let removeOutRopes p =
	(* Remove out ropes from a modifier list *)
	let rop m =
		List.filter (
			fun e ->
				match e with
				| Roped(((x, y), _, _)) -> (x >= 0. && x <= 500. && y >= 0. && y <= 700.)
				| _                     -> true
		) m
	in
	match p with
	| Player((a,b,c)) -> Player((a, b, (rop c)))
	| _ -> p

(* Drag a rope for the player *)
let rec dragRope r level p =
	let event = wait_next_event [Button_up; Mouse_motion] in
	(* If the mouse button is still pressed *)
	if event.button then begin
		(* Create the new Roped object (only on a mutiple of 5) *)
		let newRope = updateRopePosition r (float_of_int (event.mouse_x/5*5)) (float_of_int (event.mouse_y/5*5)) p in
		(* Create a player with the updated rope *)
		let newPlayer = replaceRope r newRope p in
		(* Redraw the level *)
		draw_level_editor level false;
		draw_menu (not (containsPlayer level));
		synchronize ();
		(* Reccursive call with the context where the old player is replaced with the new one *)
		dragRope newRope ((removeFromLevel p level)@[newPlayer]) newPlayer
	end else level

(* Search for a click an a new object *)
let checkNewObject level =
	let pX = fst (mouse_pos()) in
	let pY = snd (mouse_pos()) in
	let playerIn = containsPlayer level in
	if ((not playerIn) && (pointIsInObject pX pY (Player(((560.,645.),25.),(0.,0.),[])))) then
		let newObject = Player(((560.,645.),25.),(0.,0.),[]) in
		dragObject newObject (level@[newObject]) (pX-560) (pY-645)
	else if (playerIn && (sqrt((560.-.(float_of_int pX))**2. +. (645.-.(float_of_int pY))**2.) <= 5.)) then
		let newRope = Roped(((560.,645.),0.,default_rope_str)) in
		let levelPlayer = getPlayer level in
		let newPlayer = addRope newRope levelPlayer in
		let newLevel = (removeFromLevel levelPlayer level)@[newPlayer] in
		dragRope newRope newLevel newPlayer
	else if (pointIsInObject pX pY (Star(((560., 485.), 25.)))) then
		let newObject = Star(((560., 485.), 25.)) in
		dragObject newObject (level@[newObject]) (pX-560) (pY-485)
	else if (pointIsInObject pX pY (Attractor((560.,405.),0.))) then
		let newObject = Attractor((560.,405.),default_att_str) in
		dragObject newObject (level@[newObject]) (pX-560) (pY-405)
	else if (pointIsInObject pX pY (Bubble(((560.,565.),25.),(0.,0.)))) then
		let newObject = Bubble(((560.,565.),25.),default_bubble_grav) in
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

(* Remove the objects (and ropes) out of the level
 * Warning: not terminal reccursive :'( *)
let rec removeOutObjects level =
	match level with
	| o::q -> (
		let pos = objectPosition o in
		if (fst pos) < 0 || (fst pos) > 500 || (snd pos) < 0 || (snd pos) > 700 then
			removeOutObjects q
		else begin
			(* Check if it's a player and if we need to check the ropes *)
			if (match o with | Player(_) -> true | _ -> false) then
				(removeOutRopes o)::(removeOutObjects q)
			else
				o::(removeOutObjects q)
		end
	)
	| [] -> level

(* Upload a level to the server *)
let uploadLevel title level =
	let resp = httpGET ("http://octr.walter.tw/upload.php?level=" ^ (urlencode (level2String level)) ^ "&description=" ^ (urlencode title)) in
	if ((fst resp) <> 200) then
		messageBox "Upload level" "An error occured!\nYour level has not been uploaded."
	else
		messageBox "Upload level" "Your level has been uploaded!"

(* Main function, will be called reccursivly *)
let rec main level =
	try

		(* Remove objects out of the level *)
		let level =
			removeOutObjects level
		in

		(* Draw the level without displaying it *)
		draw_level_editor level false;
		
		(* Draw the games objects in the right menu *)
		draw_menu (not (containsPlayer level));
		synchronize ();
		
		(* Wait for a drag *)
		let event = wait_next_event [Button_down; Key_pressed] in

		(* Bind the escape key to leave the editor *)
		if ((Char.code event.key) = 27) then
			raise (Graphic_failure("Game closed with escape."));

		(* Check for upload *)
		if ((event.mouse_x >= 520) && (event.mouse_y >= 680)) then begin
			let title = inputBox "Enter the name of your level:" in
			if (title <> "") then
				uploadLevel title level;
		end;

		(* This will be a pair, the first value is a boolean that indicates if an object is pointed *)
		let pointed = getPointedObject (fst (mouse_pos())) (snd (mouse_pos())) level in

		(* If we are on an object *)
		if fst pointed then begin
			(* Calculate the mouse position relative to the object *)
			let objectPos = objectPosition (snd pointed) in
			(* Drag the object *)
			let newLevel = dragObject (snd pointed) level (event.mouse_x-(fst objectPos)) (event.mouse_y-(snd objectPos)) in
			(* Reccursive call on the new level *)
			main newLevel
		end else begin

			(* If the mouse doesn't point a gameObject, we must check if it points a player modifier (a rope) *)
			if (containsPlayer level) then begin

				(* Get the player *)
				let levelPlayer = getPlayer level in
				let playerModifiers = match levelPlayer with | Player(_, _, m) -> m | _ -> [] in

				(* Get the pointed rope *)
				let ropePointed = getPointedRope (float_of_int (fst (mouse_pos()))) (float_of_int (snd (mouse_pos()))) playerModifiers in

				(* Drag it if there is one *)
				if (fst ropePointed) then begin
					let newLevel = dragRope (snd ropePointed) level levelPlayer in
					main newLevel
				end

			end;

			(* Check if we must add a new game object and reccursivly call the main editor function *)
			main (checkNewObject level);

		end

	(* Caught the graphics exceptions (for example window closing) *)
	with Graphics.Graphic_failure(_) ->
		(* Save the level (and add a gravity field) *)
		let level = removeOutObjects level in
		let isGravity =
			List.exists (fun e -> match e with | GravField(_) -> true | _ -> false) level
		in
		if isGravity then
			saveLevel level Sys.argv.(1)
		else
			saveLevel ((GravField(default_grav))::level) Sys.argv.(1);
		exit 0

let () =
	main level