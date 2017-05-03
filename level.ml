(*	PHYSICS.ML
 *	
 *	Where magic is loaded and stored.
 *	Levels are loaded and stored here (Will be used for the editor).
 *)

(*	Object loading *)
(*let loadObject str =
	()	(*	TODO	*)

(*	Level loading *)
let loadLevel file =
	let chan = open_in file in
	try
 		while true; do
			lines := input_line chan :: !lines
		done;
	with End_of_file ->
		close_in chan;*)