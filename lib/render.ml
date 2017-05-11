(*  RENDER.ML
 *  
 *  Where magic shows up.
 *  Rendering happens here.
 *)

open Graphics
open Ppm
open Level
open Gametypes
open List
open Catenary

let () = 
    (* Open a graphic window with the size depending on the running programm
     * "contains" is from level.ml *)
    if (contains Sys.executable_name "editor") then begin
        open_graph " 620x700";
        set_window_title "OCutTheRope - Level Editor"
    end else begin
        open_graph " 500x700";
        set_window_title "OCutTheRope"
    end;
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
    set_line_width 2;
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
        (* A function to draw a curve from a given function *)
        let rec drawCurve fromX toX step func =
            if (fromX >= toX) then
                lineto ix2 iy2 (* Prevent that the curve go further than the point *)
            else
                lineto (int_of_float fromX) (int_of_float (func fromX));
            if (fromX < toX) then
                drawCurve (fromX +. step) toX step func
            else
                ()
        in
        (* Draw the curve that corresponds to the rope *)
        if inv then
            fill_circle (int_of_float x1) (int_of_float y1) 5
        else
            fill_circle (int_of_float x2) (int_of_float y2) 5;
        moveto (int_of_float x1) (int_of_float y1);
        drawCurve x1 x2 1. (getCatenaryFunction x1 y1 x2 y2 l)
    end

(* Separates modifiers for rendering *)
let separate_modifiers m =
    let (bl, rl) =
    fold_left (fun (bl, rl) elm ->
        match elm with
        | Roped(_)      ->  (elm::bl, rl)
        | Bubbled(_)    ->  (bl, elm::rl)
        | _ -> (bl, rl)
    ) ([], []) m in
    [rl; bl]

(* Draw the modifiers of a player *)
let rec draw_mods p m =
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
    ); draw_mods p q
    | []   -> ()

let draw_modifiers p m =
    fold_left(fun _ lm ->
        draw_mods p lm
    ) () (separate_modifiers m)

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

(* Separate elements into different lists for rendering priority. *)
let separate_elements lst =
    let (a1,a2,a3,a4,a5,a6,a7,a8) = fold_left (fun (l1, l2, l3, l4, l5, l6, l7, l8) elmt ->
        match elmt with
        | Player(_)     -> (elmt::l1, l2, l3, l4, l5, l6, l7, l8)
        | Goal(_)       -> (l1, elmt::l2, l3, l4, l5, l6, l7, l8)
        | GravField(_)  -> (l1, l2, elmt::l3, l4, l5, l6, l7, l8)
        | Star(_)       -> (l1, l2, l3, elmt::l4, l5, l6, l7, l8)
        | Bubble(_)     -> (l1, l2, l3, l4, elmt::l5, l6, l7, l8)
        | Attractor(_)  -> (l1, l2, l3, l4, l5, elmt::l6, l7, l8)
        | Wall(_)       -> (l1, l2, l3, l4, l5, l6, elmt::l7, l8)
        | Monster(_)    -> (l1, l2, l3, l4, l5, l6, l7, elmt::l8)
    ) ([],[],[],[],[],[],[],[]) lst in [a8;a7;a6;a5;a4;a3;a2;a1]


(* Just call this function to draw the level
 * @params
 *   level of context : the level
 *   sync  of bool    : true to synchronize, false to don't
 *)
let draw_level_game level sync =
    (* Clear old drawing *)
    clear_graph();
    (* Draw background *)
    draw_image background 0 0;
    (* Internal function to draw the elements of a gameElement list *)
    fold_left (fun _ lst ->
        fold_left(fun _ elmt ->
            draw_single_element elmt
        ) () lst
    ) () (separate_elements level);
    if sync then synchronize()


let draw_level_editor level sync =
    (* Clear old drawing *)
    clear_graph();
    (* Draw background *)
    draw_image background 0 0;
    (* Internal function to draw the elements of a gameElement list *)
    fold_left (fun _ elmt ->
        draw_single_element elmt
    ) () level;
    if sync then synchronize()
