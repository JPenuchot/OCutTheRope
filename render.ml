(*  RENDER.ML
 *  
 *  Where magic shows up.
 *  Rendering happens here.
 *)

open Graphics
open Ppm
open Level
open Gametypes

let () = 
    (* Open a graphic window with the size depending on the running programm
     * "contains" is from level.ml *)
    if (contains Sys.executable_name "editor") then
        open_graph " 620x700"
    else
        open_graph " 500x700";
    (* Enable double buffering, use "synchronize()" to update window *)
    auto_synchronize false

(* Load pictures *)
let background =
    image_from_ppm "sprites/background.ppm"
let player_sprite =
    image_from_ppm "sprites/player.ppm"
let bubble_sprite =
    image_from_ppm "sprites/bubble.ppm"
let bubbled_sprite =
    image_from_ppm "sprites/bubbled.ppm"
let star_sprite =
    image_from_ppm "sprites/star.ppm"
let goal_sprite =
    image_from_ppm "sprites/goal.ppm"
let monster_sprite =
    image_from_ppm "sprites/monster.ppm"
let wall_sprite =
    image_from_ppm "sprites/wall.ppm"
let attractor_sprite =
    image_from_ppm "sprites/attractor.ppm"
let unknow_sprite =
    image_from_ppm "sprites/unknow.ppm"

type typePlayerInfos = {
    x : float;
    y : float;
    r : float;
}

(* Draw a line bewteen two pos with a specified length *)
(* The parameter inv is used to know if the points as been inverted (set false by default) *)
let rec drawRope (x1, y1) (x2, y2) l inv =
    (* Check for a too long rope *)
    let tooLong = (sqrt((x2-.x1)**2. +. (y2-.y1)**2.)) > l in
    let ix1 = int_of_float x1 in
    let ix2 = int_of_float x2 in
    let iy1 = int_of_float y1 in
    let iy2 = int_of_float y2 in
    let il = int_of_float l in
    (* Check for a verticle rope *)
    if ((ix1 = ix2) || tooLong) then begin
        if tooLong then set_color 0xFF0000;
        if inv then
            fill_circle ix1 iy1 5
        else
            fill_circle ix2 iy2 5;
        if (iy1 > iy2) then begin
            moveto ix1 iy1;
            lineto ix2 (if tooLong then iy2 else iy2 - (il-(iy1-iy2))/2)(* Add the rope remaining *)
        end else begin
            moveto ix2 iy2;
            lineto ix1 (if tooLong then iy1 else iy1 - (il-(iy2-iy1))/2)(* Add the rope remaining *)
        end;
        if tooLong then set_color 0x000000;
    end
    (* Rearange the points *)
    else if (ix2 < ix1) then
        drawRope (x2,y2) (x1,y1) l (not inv)
    else begin
        (* Compute z *)
        let rec computeZ z =
            if (((sinh z) /. z) < ((sqrt (l**2. -. (y2-.y1)**2.)) /. (x2-.x1))) then
                computeZ (z +. 0.001)
            else
                z
        in
        let z = computeZ 0.001 in
        (* Calculate the curve parameters *)
        let a = (x2 -. x1) /. 2. /. z in
        let p = (x1+.x2-.a*.(log ( (l+.y2-.y1) /. (l-.y2+.y1) ))) /. 2. in
        let q = (y2+.y1-.l*.(cosh z)/.(sinh z)) /. 2. in
        (* A function to draw a curve from a given function *)
        let rec drawCurve fromX toX step func =
            lineto (int_of_float fromX) (int_of_float (func fromX));
            if (fromX < toX) then
                drawCurve (fromX +. step) toX step func
            else
                ()
        in
        (* The function *)
        let f x =
            a *. (cosh ((x-.p)/.a)) +. q
        in
        (* Draw the curve that corresponds to the rope *)
        if inv then
            fill_circle (int_of_float x1) (int_of_float y1) 5
        else
            fill_circle (int_of_float x2) (int_of_float y2) 5;
        moveto (int_of_float x1) (int_of_float y1);
        drawCurve x1 x2 1. f
    end


(* Draw the modifiers of a player *)
let rec draw_modifiers p m =
    (* Get player informations *)
    let playerInfos =
        match p with
        | Player((((pX, pY), radius), _, _)) -> {x = pX; y = pY; r = radius}
        | _                                  -> {x = 0.; y = 0.; r = 0.}
    in
    (* Match the modifier list *)
    match m with
    | h::q -> (
        match h with
        | Bubbled(_)           -> draw_image bubbled_sprite (int_of_float (playerInfos.x-.playerInfos.r)) (int_of_float (playerInfos.y-.playerInfos.r))
        | Roped(((x,y),len,_)) -> drawRope (playerInfos.x, playerInfos.y) (x, y) len false
        | _                    -> ()
    ); draw_modifiers p q
    | []   -> ()

(* Simple function to draw a single element *)
let draw_single_element element =
    match element with
    | Player(((posX, posY), radius), _, m) -> draw_image player_sprite    (int_of_float (posX-.radius)) (int_of_float (posY-.radius));
                                              draw_modifiers element m;
    | Goal((posX, posY), _)                -> draw_image goal_sprite      (int_of_float posX) (int_of_float posY)
    | Star((posX, posY), radius)           -> draw_image star_sprite      (int_of_float (posX-.radius)) (int_of_float (posY-.radius))
    | Bubble(((posX, posY), radius), _)    -> draw_image bubble_sprite    (int_of_float (posX-.radius)) (int_of_float (posY-.radius))
    | Attractor((posX, posY), _)           -> draw_image attractor_sprite (int_of_float (posX-.25.)) (int_of_float (posY-.25.))
    | Wall((posX, posY), _)                -> draw_image wall_sprite      (int_of_float posX) (int_of_float posY)
    | Monster((posX, posY), _)             -> draw_image monster_sprite   (int_of_float posX) (int_of_float posY)
    | _                                    -> ()

(* Just call this function to draw the level
 * @params
 *   level of context : the level
 *   sync  of bool    : true to synchronize, false to don't
 *)
let draw_level level sync =
    (* Clear old drawing *)
    clear_graph();
    (* Draw background *)
    draw_image background 0 0;
    (* Internal function to draw the elements of a gameElement list *)
    let rec draw_elements elementsList =
        match elementsList with
        | [] -> ()
        | t::q -> draw_single_element t; draw_elements q;
    in
    draw_elements level;
    if sync then synchronize()
