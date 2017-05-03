(*	PHYSICS.ML
 *	
 *	Where magic is loaded and stored.
 *	Levels are loaded and stored here (Will be used for the editor).
 *)

open Str

(* The regular expression to match the object in a line *)
let regExpFloat =
	"\\([0-9]+\\.[0-9]*\\)"
let regExpPos =
	"\\((" ^ regExpFloat ^ "," ^ regExpFloat ^ ")\\)"
let regExpVel = regExpPos
let regExpAccel = regExpPos
let regExpSize = regExpPos
let regExpMass = regExpFloat
let regExpLength = regExpFloat
let regExpModifier =
	"\\(\\(Bubbled(" ^ regExpAccel ^ ")\\)\\|\\(Roped(" ^ regExpPos ^ "," ^ regExpLength ^ ")\\)\\)"
let regExpModifiers =
	"\\(\\[\\(" ^ regExpModifier ^ "\\(;" ^ regExpModifier ^ "\\)*" ^ "\\)?\\]\\)"
let regExpPlayer =
	"\\(Player(" ^ regExpPos ^ "," ^ regExpVel ^ "," ^ regExpModifiers ^ ")\\)"
let regExpGoal =
	"\\(Goal(" ^ regExpPos ^ "," ^ regExpSize ^ ")\\)"
let regExpGravField =
	"\\(GravField(" ^ regExpAccel ^ ")\\)"
let regExpStar =
	"\\(Star(" ^ regExpPos ^ ")\\)"
let regExpBubble =
	"\\(Bubble(" ^ regExpPos ^ "," ^ regExpAccel ^ ")\\)"
let regExpAttractor =
	"\\(Attractor(" ^ regExpPos ^ "," ^ regExpFloat ^ ")\\)"
let regExpWall =
	"\\(Wall(" ^ regExpPos ^ "," ^ regExpSize ^ ")\\)"
let regExpMonster =
	"\\(Monster(" ^ regExpPos ^ "," ^ regExpSize ^ ")\\)"
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

(*	Object loading *)
let loadObject str =
	(* Check the string *)
	if not (string_match regExpLine str 0) then
		failwith ("loadObject: Given string is not a correct game object! \"" ^ str ^ "\"")
	(* Load the object *)

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