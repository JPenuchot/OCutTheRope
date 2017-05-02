(*	PHYSICS.ML
 *	
 *	Where magic is summoned.
 *	Manages the game mechanics, uses physics.ml to compute
 *	physics stuff and render.ml to render stuff.
 *)

open Physics

type gameObject =
| GravField	of pos
| Star		of pos
| Bubble	of pos
| Attractor	of pos * float
| Rope		of pos * float
| Wall		of pos * size
| SMonster	of pos * size

| Player	of pos * vel * forces
| DMonster	of pos * vel * forces


(*	A context (level state) is described by a list of objects. *)
type context = gameObject list
