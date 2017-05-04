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
let regExpRope =
	"\\((" ^ regExpPos ^ "," ^ regExpLength ^ "," ^ regExpFloat ^ ")\\)"
(* Types from gametypes.ml *)
let regExpBubbled =
	"\\(Bubbled(" ^ regExpAccel ^ ")\\)"
let regExpRoped =
	"\\(Roped(" ^ regExpRope ^ ")\\)"
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

(* Return an array of string representing the result of matched_group for a given regexp *)
let get_matched_groups exp str =
	(* Match the string for using matched_group *)
	string_match exp str 0;
	(* Reccursive function who build the array *)
	let rec gmg str arr n =
		try
			gmg str (Array.append arr [|matched_group n str|]) (n+1)
		with Not_found ->
			arr
	in
	(* We put an element in the array so the indice will match with matched_group's indice *)
	gmg str [|""|] 1


(* Returns a modifiers list from a text *)
let rec loadModifiers str =
	if (length str) = 0 then
		[]
	else begin
		(* Check if match *)
		if not (string_match (regexp regExpModifiersInner) str 0) then
			failwith ("loadModifiers: Given string is not a corret modifier list! \"" ^ str ^ "\"");
		(* Get the current modifier and its informations *)
		let modifiersGroups = get_matched_groups (regexp regExpModifiersInner) str in
		let currentModifier = modifiersGroups.(3) in
		let currentModifierLen = length currentModifier in
		let nextModifierLen = (length str) - currentModifierLen - 1 in
		let nextModifier = if nextModifierLen <= 0 then "" else (sub str (currentModifierLen + 1) nextModifierLen) in
		(* Match the modifier *)
		match (sub str 0 1) with
		| "B" -> Bubbled(((float_of_string modifiersGroups.(6)),(float_of_string modifiersGroups.(7))))
				::(loadModifiers nextModifier)
		| "R" -> 
				Roped(((0.),(0.)),(0.),(0.))
				::(loadModifiers nextModifier)
	end

(*	Object loading *)
let loadObject str =
	(* Check the string *)
	if not (string_match regExpLine str 0) then
		failwith ("loadObject: Given string is not a correct game object! \"" ^ str ^ "\"");
	(* Get the latched group *)
	let lineGroups = get_matched_groups regExpLine str in
	(* Match the object *)
	match (sub str 0 2) with
	| "Pl" -> Player (
					(((float_of_string lineGroups.(5)), (float_of_string lineGroups.(6))), (float_of_string lineGroups.(7))),
					((float_of_string lineGroups.(9)), (float_of_string lineGroups.(10))),
					(loadModifiers lineGroups.(12))
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