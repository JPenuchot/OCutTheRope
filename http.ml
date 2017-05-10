(*	HTTP.ML
 *	
 *	Contains simple functions to send HTTP GET requests
 *)

open Lwt
open Cohttp
open Cohttp_lwt_unix

exception HTTPError

(* string -> (int * string)
 * Return the HTTP code and the response content of a GET request *)
let httpGET url =
	let get =
		Client.get (Uri.of_string url) >>= fun (resp, body) ->
	 	let code = resp |> Response.status |> Code.code_of_status in
	 	(*Printf.printf "Response code: %d\n" code;
	 	Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
	 	body |> Cohttp_lwt_body.to_string >|= fun body ->
	 	(*Printf.printf "Body of length: %d\n" (String.length body);*)
 		(code, body)
 	in
 	Lwt_main.run get


let urlencode param =
	let chars = String.explode param in
	let rec fn = function
		| x::tl ->
		begin
			let s =
				if x = ' ' then "+"
				else match x with
				| 'A'..'Z'
				| 'a'..'z'
				| '0'..'9'
				| '$' | '-' | '_' | '.' | '!'
				| '*' | '\'' | '(' | ')' | ',' ->
					String.of_char x
				| _ ->
					Printf.sprintf "%%%2x" (Char.code x)
			in
			s ^ fn tl
		end
		| [] ->
		""
	in fn chars
