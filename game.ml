open Level
open Render
open Basephysics
open Gamemechanics
open Gametypes
open Graphics
open Gui

(* Display a win message *)
let displayWin () =
	let rec wait () =
		moveto 50 50;
		drawCenteredText 250 400 "Congratulations, you win!";
		synchronize ();
		wait ()
	in
	clear_graph();
	ignore (
		try
			set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1"
		with Graphics.Graphic_failure(_) -> ()
	);
	wait ()

(* Display a message before next level
 * Must have a player to count the stars
 *)
let displayNext score =
	let rec drawStars n m =
		if (n < m) then begin
			let width = m * 50 in
			let space = (m-1) * 30 in
			let x = 250 - (width+space)/2 + 80*n in
			draw_image star_sprite x 400;
			drawStars (n+1) m
		end
		else
			()
	in
	(* Wait a click *)
	let rec wait_click () =
		(* Draw *)
		clear_graph();
		drawCenteredText 250 550 "You did it, try the next level!";
		drawCenteredText 250 500 "Your score";
		drawStars 0 score;
		drawButton 175 300 150 50 "Next";
		drawButton 175 200 150 50 "Rage quit";
		synchronize ();
		(* Wait for a click *)
		let event = wait_next_event [Button_down] in
		if event.button then begin
			if ((event.mouse_x >= 175) && (event.mouse_x <= 325) && (event.mouse_y >= 300) && (event.mouse_y <= 350)) then
				ignore (wait_next_event [Button_up])
			else if ((event.mouse_x >= 175) && (event.mouse_x <= 325) && (event.mouse_y >= 200) && (event.mouse_y <= 250)) then begin
				ignore (wait_next_event [Button_up]);
				exit 0
			end
			else
				wait_click ()
		end
		else
			wait_click ()
	in
	ignore (
		try
			set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1"
		with Graphics.Graphic_failure(_) -> ()
	);
	wait_click ()


(* Display a retry message *)
let displayRetry () =
	(* Wait a click *)
	let rec wait_click () =
		(* Draw *)
		clear_graph();
		drawCenteredText 250 550 "Sorry, you loose....";
		drawButton 175 400 150 50 "Retry";
		drawButton 175 300 150 50 "Rage quit";
		synchronize ();
		(* Wait for a click *)
		let event = wait_next_event [Button_down] in
		if event.button then begin
			if ((event.mouse_x >= 175) && (event.mouse_x <= 325) && (event.mouse_y >= 400) && (event.mouse_y <= 450)) then
				ignore (wait_next_event [Button_up])
			else if ((event.mouse_x >= 175) && (event.mouse_x <= 325) && (event.mouse_y >= 300) && (event.mouse_y <= 350)) then begin
				ignore (wait_next_event [Button_up]);
				exit 0
			end
			else
				wait_click ()
		end
		else
			wait_click ()
	in
	ignore (
		try
			set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1"
		with Graphics.Graphic_failure(_) -> ()
	);
	wait_click ()

(* Main function, will increment the level *)
let rec main levelInfos =
	try
		game_loop (snd levelInfos)
	with
	| EndGame(Win(score)) -> (
		let newID = (fst levelInfos) + 1 in
		try
			(* Load the next level *)
			let newLevel = loadLevel ("levels/" ^ (string_of_int newID) ^ ".lvl") in
			(* Show the score for this level *)
			displayNext score;
			(* Launch the new level *)
			main (newID, newLevel)
		with LevelLoadError ->
			(* If the incrementing loading fail, that means we are a the last level, so we win *)
			displayWin ()
	)
	| EndGame(Die) -> (
		let rec wait_up () =
			if (button_down ())then
				wait_up ()
			else
				()
		in
		wait_up ();
		(* If the player die, we display the retry menu *)
		displayRetry ();
		main levelInfos
	)

let () =
	(* Load a file if there is one given as a parameter *)
	let firstLevel =
		if (Array.length Sys.argv) >= 2 then
			(-1, loadLevel Sys.argv.(1))
		else
			(1, loadLevel "levels/1.lvl")
	in

	(* Run the game *)
	try
		main firstLevel
	with Graphics.Graphic_failure(_) -> exit 0
