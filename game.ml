open Level
open Basephysics
open Gamemechanics

(* Load a file if there is one given as a parameter *)
let levelInfo =
	(*Printf.printf "Loading level\n%!";*)
	if (Array.length Sys.argv) >= 2 then
		(-1, loadLevel Sys.argv.(1))
	else
		(1, loadLevel "levels/1.lvl")

let () =
	try
		try
			game_loop (snd levelInfo)
		with
		| Gametypes.EndGame(Win) -> Printf.printf "Gagné !\n";
		| Gametypes.EndGame(Die) -> Printf.printf "Perdu !\n"
	with Graphics.Graphic_failure(_) -> exit 0