(*	PHYSICS.ML
 *	
 *	Where magic is summoned.
 *	Manages the game mechanics, uses physics.ml to compute
 *	physics stuff and render.ml to render stuff.
 *)

type player		=	{x : float; y : float; xx : float; yy : float}
type monster	=	{x : float; y : float}
type gravField	=	{x : float; y : float}
type star		=	{x : float; y : float}
type bubble		=	{x : float; y : float; gf : gravField}
type attractor	=	{x : float; y : float; str : float}
type rope		=	{x : float; y : float; len : float}
type wall		=	{x : float; y : float; w : float; h : float}
