open Level
open Basephysics
open Gamemechanics

(* Load a file if there is one given as a parameter *)
let level =
	(*Printf.printf "Loading level\n%!";*)
	if (Array.length Sys.argv) >= 2 then
		loadLevel Sys.argv.(1)
	else
		failwith "You must specify a level file!"

let () = 
	try
		game_loop level
	with Graphics.Graphic_failure(_) -> exit 0