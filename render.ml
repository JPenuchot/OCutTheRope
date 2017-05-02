(*	RENDER.ML
 *	
 *	Where magic shows up.
 *	Rendering happens here.
 *)

open Graphics
open Ppm
open Level

(* Open a graphic window *)
let () = open_graph " 300x500" ;;

(* Set transparent color *)

(* Load pictures *)
let player_sprite =
	image_from_ppm "sprites/player.ppm";;