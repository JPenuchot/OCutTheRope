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
let regExpBubbled =
	"\\(Bubbled(" ^ regExpAccel ^ ")\\)"
let regExpRoped =
	"\\(Roped(" ^ regExpPos ^ "," ^ regExpLength ^ ")\\)"
let regExpModifier =
	"\\(" ^ regExpBubbled ^ "\\|" ^ regExpRoped ^ "\\)"
let regExpModifiersInner =
	"\\(" ^ regExpModifier ^ "\\(;" ^ regExpModifier ^ "\\)*" ^ "\\)?"
let regExpModifiers =
	"\\(\\[" ^ regExpModifiersInner ^ "?\\]\\)"
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
(* Match any gameObject *)
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

(* TODO: Une fonction qui prend une châine et une regexp et qui retourne une liste de mots matchés *)

let rec print_groups str n =
	Printf.printf "print_groups:%s\n" (matched_group n str);
	print_groups str (n+1)

(* Returns a modifiers list from a text *)
let rec loadModifiers str =
	if (length str) <> 0 then begin
		if not (string_match (regexp regExpModifiersInner) str 0) then
			failwith "loadModifiers: Given string is not a correct modifiers list";
		let subModifier =
			matched_group 2 str
		in
		(* We need to use this expression because "matched_group" uses the lasr regexp used *)
		if not (string_match (regexp regExpModifier) subModifier 0) then
			failwith "loadModifiers: Given string is not a correct modifier";
		let subModifierLen = length subModifier in
		(* If there is more than one modifier *)
		if subModifierLen <> 0 then begin
			(* Get the remaining modifiers to parse *)
			let nextString =
				let stringStart = (subModifierLen+1) in
				let stringLen = ((length str)-subModifierLen-1) in
				if stringLen > 0 then
					(sub str stringStart stringLen)
				else
					""
			in
			let nexModifiers = (loadModifiers nextString) in
			(* Match the current modifier and create it *)
			match (sub subModifier 0 1) with
			| "B" -> string_match (regexp regExpBubbled) subModifier 0;
				Bubbled(((float_of_string (matched_group 3 subModifier)),(float_of_string (matched_group 4 subModifier))))
				::nexModifiers
			(* We must re-match to avoid weird errors *)
			| "R" -> string_match (regexp regExpRoped) subModifier 0;
				Roped(((float_of_string (matched_group 3 subModifier)),(float_of_string (matched_group 4 subModifier))), (float_of_string (matched_group 5 subModifier)))
				::nexModifiers
		end else
			[]
	end else
		[]

(*	Object loading *)
let loadObject str =
	(* Check the string *)
	if not (string_match regExpLine str 0) then
		failwith ("loadObject: Given string is not a correct game object! \"" ^ str ^ "\"");
	let playerModifiers = loadModifiers (matched_group 12 str) in
	string_match regExpLine str 0;
	(* Match the object *)
	Printf.printf "loadObject %s\n" str;
	match (sub str 0 2) with
	| "Pl" -> Player(
						((float_of_string (matched_group 5 str), float_of_string (matched_group 6 str)), float_of_string (matched_group 7 str)),
						(float_of_string (matched_group 9 str), float_of_string (matched_group 10 str)),
						playerModifiers
					)
	(*| "Pl" -> print_groups str 1*)
	| _ -> Empty((0.,0.)) (* We use an Empty element to have an exhaustive pattern matching *)

(*	Level loading *)
let loadLevel file =
	(* Reccursive function to read the file *)
	let rec read_file handle check_header level =
		(* The file must have "# OCutTheRope Level File 1.0" on it's first line *)
		if check_header then begin
			if (input_line handle) <> "# OCutTheRope Level File 1.0" then
				failwith "Level file header not found!";
			read_file handle false level
		end
		else begin
			try
				(* Give the line to the loading object function *)
				read_file handle false level@[loadObject (input_line handle)]
			with End_of_file ->
				close_in handle;
				level
		end;
	in
	read_file (open_in file) true []