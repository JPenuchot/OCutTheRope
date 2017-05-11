(* GUI.ML
 *
 * Graphical User Interface
 *)

open Graphics

(* Draw a text in the middle *)
let drawCenteredText x y text =
	let textSize = text_size text in
	moveto (x - (fst textSize)/2) (y - (snd textSize)/2);
	draw_string text

(* Draw a button *)
let drawButton x y width height text =
	set_color (rgb 127 127 127);
	fill_rect (x+5) (y-5) width height;
	set_color (rgb 200 200 200);
	fill_rect x y width height;
	set_color black;
	draw_rect x y width height;
	drawCenteredText (x + (width/2)) (y + (height/2)) text