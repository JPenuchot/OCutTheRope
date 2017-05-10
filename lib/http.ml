(*	HTTP.ML
 *	
 *	Contains simple functions to send HTTP GET requests.
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

(* From https://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/explode.ml *)
let explode s =
	let rec expl i l =
		if i < 0 then l else
		expl (i - 1) (s.[i] :: l) in
 	expl (String.length s - 1) []

(* From https://github.com/samoht/ocaml-wget/blob/master/src/http.ml#L54 *)
let urlencode param =
	let chars = explode param in
	let rec fn = function
		| x::tl ->
		begin
			let s =
				if x = ' ' then "+"
				else match x with
				| '\n' -> "%0A"
				| 'A'..'Z'
				| 'a'..'z'
				| '0'..'9'
				| '$' | '-' | '_' | '.' | '!'
				| '*' | '\'' | '(' | ')' | ',' ->
					String.make 1 x
				| _ ->
					Printf.sprintf "%%%2x" (Char.code x)
			in
			s ^ fn tl
		end
		| [] ->
		""
	in fn chars
