open Level
open Basephysics
open Gamemechanics

(* Main function, will increment the level *)
let main () =
	try
		game_loop (snd levelInfo)
	with
	| Gametypes.EndGame(Win) -> Printf.printf "Gagné !\n";
	| Gametypes.EndGame(Die) -> Printf.printf "Perdu !\n"

let () =
	(* Load a file if there is one given as a parameter *)
	if (Array.length Sys.argv) >= 2 then
		(-1, loadLevel Sys.argv.(1))
	else
		(1, loadLevel "levels/1.lvl")

	(* Run the game *)
	try
		main ()
	with Graphics.Graphic_failure(_) -> exit 0
