(*	SCRIPTS.ML
 *	
 *	Manage the python scripts.
 *)

(* Call an input box written in python ('inputbox.py') and return the result *)
let inputBox title =
	let ib = Unix.open_process_in ("python scripts/inputbox.py \"" ^ title ^ "\"") in
	let getStdOut =
		try
			input_line ib
		with _ -> ""
	in
	ignore (Unix.close_process_in ib);
	getStdOut

(* Call an message box in python ('messagebox.py') *)
let messageBox title message =
	let mb = Unix.open_process_in ("python scripts/messagebox.py \"" ^ title ^ "\" \"" ^ message ^ "\"") in
	try
		ignore (input_line mb)
	with _ -> ignore (Unix.close_process_in mb)