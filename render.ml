(*	RENDER.ML
 *	
 *	Where magic shows up.
 *	Rendering happens here.
 *)

open Graphics
open Ppm
open Level

(* Open a graphic window *)
let () = open_graph " 500x700" ;;

(* Enable double buffering, use "synchronize()" to update window *)
auto_synchronize false;;

(* Load pictures *)
let player_sprite =
	image_from_ppm "sprites/player.ppm";;
let bubble_sprite =
	image_from_ppm "sprites/bubble.ppm";;
let star_sprite =
	image_from_ppm "sprites/star.ppm";;
let monster_sprite =
	image_from_ppm "sprites/monster.ppm";;