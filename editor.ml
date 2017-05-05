(*	GAMEMECHANICS.ML
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

(* Function to draw each game object in the right menu *)
let draw_menu () =
	draw_image player_sprite 535 620;
	draw_image bubble_sprite 535 540;
	draw_image star_sprite 535 460;
	draw_image attractor_sprite 535 380;
	draw_image wall_sprite 535 300;
	draw_image goal_sprite 522 170;
	draw_image monster_sprite 510 40

(* Main function, will be called reccursivly *)
let rec main () =
	try
		(* Draw the level without displaying it *)
		draw_level level false;
		(* Draw the games objects in the right menu *)
		draw_menu ();
		synchronize ();
		(* Reccursivly call the main editor function *)
		main ()
	(* Caught the graphics exceptions (for example window closing) *)
	with Graphics.Graphic_failure(_) -> ()

let () =
	main ()