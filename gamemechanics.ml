(*	PHYSICS.ML
 *	
 *	Where magic is summoned.
 *	Manages the game mechanics, uses physics.ml to compute
 *	physics stuff and render.ml to render stuff.
 *)

type gameObject =
	| Player	of (**)
	| Monster	of (**)
	| Star		of (**)
	| Bubble	of (**)
	| Attractor	of (**)
	| Rope		of (**)
	| Wall		of (**)
	| GravField	of (**)