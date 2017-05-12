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
		drawCenteredText 250 400 "Congratulations, you win!";
		synchronize ();
		ignore (wait_next_event [Button_down]);
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
		drawButton 175 200 150 50 "Retry";
		drawButton 175 100 150 50 "Rage quit";
		synchronize ();
		(* Wait for a click *)
		let event = wait_next_event [Button_down] in
		if event.button then begin
			if ((event.mouse_x >= 175) && (event.mouse_x <= 325) && (event.mouse_y >= 300) && (event.mouse_y <= 350)) then begin
				ignore (wait_next_event [Button_up]);
				0 (* Return 0 to continue *)
			end
			else if ((event.mouse_x >= 175) && (event.mouse_x <= 325) && (event.mouse_y >= 200) && (event.mouse_y <= 250)) then begin
				ignore (wait_next_event [Button_up]);
				1 (* Return 1 to retry *)
			end
			else if ((event.mouse_x >= 175) && (event.mouse_x <= 325) && (event.mouse_y >= 100) && (event.mouse_y <= 150)) then begin
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

(* Merge all the values of an array *)
let mergeArray arr =
	Array.fold_left (fun acc str -> acc ^ "\"" ^ str ^ "\" ") "" arr

(* Allow user to select a style *)
let styleSelect () =
	(* Function called on a click *)
	let buttonAction p =
		ignore (wait_next_event [Button_up]);
		close_graph ();
		ignore (Sys.command ((mergeArray Sys.argv) ^ " -s " ^ p));
		exit 0
	in
	(* Wait a click *)
	let rec wait_click () =
		(* Draw *)
		clear_graph();
		drawCenteredText 250 550 "Choose a theme....";
		drawButton 175 400 150 50 "Basic";
		drawButton 175 300 150 50 "Mario";
		drawButton 175 200 150 50 "Cut The Rope";
		synchronize ();
		(* Wait for a click *)
		let event = wait_next_event [Button_down] in
		if event.button then begin
			if ((event.mouse_x >= 175) && (event.mouse_x <= 325) && (event.mouse_y >= 400) && (event.mouse_y <= 450)) then
				buttonAction "basic"
			else if ((event.mouse_x >= 175) && (event.mouse_x <= 325) && (event.mouse_y >= 300) && (event.mouse_y <= 350)) then
				buttonAction "mario"
			else if ((event.mouse_x >= 175) && (event.mouse_x <= 325) && (event.mouse_y >= 200) && (event.mouse_y <= 250)) then
				buttonAction "ctr"
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
		let rec wait_up () =
			if (button_down ()) then
				wait_up ()
			else
				()
		in
		wait_up ();
		let newID = (fst levelInfos) + 1 in(* Show the score for this level *)
		if ((displayNext score) = 0) then begin
			try
				(* Load the next level *)
				let newLevel = loadLevel ("levels/" ^ (string_of_int newID) ^ ".lvl") in
				(* Launch the new level *)
				main (newID, newLevel)
			with LevelLoadError ->
				(* If the incrementing loading fail, that means we are a the last level, so we win *)
				displayWin ()
		end
		else
			(* Retry *)
			main levelInfos
	)
	| EndGame(Die) -> (
		let rec wait_up () =
			if (button_down ()) then
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

	(* Detect if a style wasn't given *)
	if (not (Array.mem "-s" Sys.argv)) then
		styleSelect ();

	(* Load a file if there is one given as a parameter *)
	let firstLevel =
		if ((Array.length Sys.argv) >= 2 && Sys.argv.(1) <> "-s") then
			(-1, loadLevel Sys.argv.(1))
		else
			(1, loadLevel "levels/1.lvl")
	in

	(* Run the game *)
	try
		main firstLevel
	with Graphics.Graphic_failure(_) -> exit 0
