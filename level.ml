(*	PHYSICS.ML
 *	
 *	Where magic is loaded and stored.
 *	Levels are loaded and stored here (Will be used for the editor).
 *)

open Str
open String
open Gametypes

(* The regular expression to match the object in a line *)
let regExpFloat =
	"\\([0-9]+\\.[0-9]*\\)"
(* Types from basephysics.ml *)
let regExpPos =
	"\\((" ^ regExpFloat ^ "," ^ regExpFloat ^ ")\\)"
let regExpVel = regExpPos
let regExpAccel = regExpPos
let regExpSize = regExpPos
let regExpDim = regExpPos
let regExpMass = regExpFloat
let regExpLength = regExpFloat
let regExpSphere =
	"\\((" ^ regExpPos ^ "," ^ regExpLength ^ ")\\)"
let regExpRect =
	"\\((" ^ regExpPos ^ "," ^ regExpSize ^ ")\\)"
(* Types from gametypes.ml *)
let regExpModifier =
	"\\(\\(Bubbled(" ^ regExpAccel ^ ")\\)\\|\\(Roped(" ^ regExpPos ^ "," ^ regExpLength ^ ")\\)\\)"
let regExpModifiers =
	"\\(\\[\\(" ^ regExpModifier ^ "\\(;" ^ regExpModifier ^ "\\)*" ^ "\\)?\\]\\)"
let regExpPlayer =
	"\\(Player(" ^ regExpSphere ^ "," ^ regExpVel ^ "," ^ regExpModifiers ^ ")\\)"
let regExpGoal =
	"\\(Goal(" ^ regExpRect ^ ")\\)"
let regExpGravField =
	"\\(GravField(" ^ regExpAccel ^ ")\\)"
let regExpStar =
	"\\(Star(" ^ regExpSphere ^ ")\\)"
let regExpBubble =
	"\\(Bubble(" ^ regExpSphere ^ "," ^ regExpAccel ^ ")\\)"
let regExpAttractor =
	"\\(Attractor(" ^ regExpPos ^ "," ^ regExpFloat ^ ")\\)"
let regExpWall =
	"\\(Wall(" ^ regExpRect ^ ")\\)"
let regExpMonster =
	"\\(Monster(" ^ regExpRect ^ ")\\)"
let regExpGameObject =
	"^\\(" ^
		regExpPlayer ^ "\\|" ^
		regExpGoal ^ "\\|" ^
		regExpGravField ^ "\\|" ^
		regExpStar ^ "\\|" ^
		regExpBubble ^ "\\|" ^
		regExpAttractor ^ "\\|" ^
		regExpWall ^ "\\|" ^
		regExpMonster ^
	"\\)$"
let regExpLine = regexp regExpGameObject

let rec print_groups str n =
	Printf.printf "%s\n" (matched_group n str);
	print_groups str (n+1)

(*	Object loading *)
let loadObject str =
	(* Check the string *)
	if not (string_match regExpLine str 0) then
		failwith ("loadObject: Given string is not a correct game object! \"" ^ str ^ "\"");
	(* Match the object *)
	match (sub str 0 2) with
	| "Pl" -> Player(
						((float_of_string (matched_group 3 str), float_of_string (matched_group 4 str)), float_of_string (matched_group 5 str)),
						(float_of_string (matched_group 7 str), float_of_string (matched_group 8 str)),
						[]
					)
	(*| "Pl" -> print_groups str 1*)
	| _ -> Wall(((-100.,-100.),(0.,0.)))

(*	Level loading *)
let loadLevel file =
	(* Reccursive function to read the file *)
	let rec read_file handle check_header =
		(* The file must have "# OCutTheRope Level File 1.0" on it's first line *)
		if check_header then begin
			if (input_line handle) <> "# OCutTheRope Level File 1.0" then
				failwith "Level file header not found!";
			read_file handle false
		end
		else begin
			try
				(* Give the line to the loading object function *)
				loadObject (input_line handle);
				read_file handle false
			with End_of_file ->
				close_in handle
		end;
	in
	read_file (open_in file) true