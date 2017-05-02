(*	RENDER.ML
 *	
 *	Where magic shows up.
 *	Rendering happens here.
 *)

open Graphics
open Ppm
open Level
open Gamemechanics

(* Open a graphic window *)
let () = open_graph " 500x700";

(* Enable double buffering, use "synchronize()" to update window *)
auto_synchronize false

(* Load pictures *)
let player_sprite =
	image_from_ppm "sprites/player.ppm"
let bubble_sprite =
	image_from_ppm "sprites/bubble.ppm"
let star_sprite =
	image_from_ppm "sprites/star.ppm"
let monster_sprite =
	image_from_ppm "sprites/monster.ppm"
let unknow_sprite =
	image_from_ppm "sprites/unknow.ppm"

(* Simple function to draw a single element *)
let draw_single_element element =
	match element with
	| GravField((posX, posY))      -> draw_image unknow_sprite (int_of_float posX) (int_of_float posY)
	| Star((posX, posY))           -> draw_image star_sprite (int_of_float posX) (int_of_float posY)
	| Bubble((posX, posY))         -> draw_image bubble_sprite (int_of_float posX) (int_of_float posY)
	| Attractor((posX, posY), _)   -> draw_image unknow_sprite (int_of_float posX) (int_of_float posY)
	| Rope((posX, posY))           -> () (* Rope is not a simple sprite *)
	| Wall((posX, posY), _)        -> draw_image unknow_sprite (int_of_float posX) (int_of_float posY)
	| SMonster((posX, posY), _)    -> draw_image monster_sprite (int_of_float posX) (int_of_float posY)
	| Player((posX, posY), _, _)   -> draw_image player_sprite (int_of_float posX) (int_of_float posY)
	| DMonster((posX, posY), _, _) -> draw_image monster_sprite (int_of_float posX) (int_of_float posY)


(* Just call this function to draw the level *)
let draw_level level =
	(* Clear old drawing *)
	clear_graph();
	(* Internal function to draw the elements of a gameElement list *)
	let rec draw_elements elementsList =
		match elementsList with
		| [] -> ()
		| t::q -> draw_single_element t; draw_elements q;
	in
	draw_elements level;
	synchronize()
