open Raylib

(** The signature of the boat character. *)
module type BoatSig = sig
  val boat_pos : Vector2.t ref
  val boat_h : Vector2.t
  val boat_v : Vector2.t
  val boat_face : Vector2.t ref
  val move : Vector2.t -> unit
  val draw : unit -> unit
  val is_border_crossed : Vector2.t -> bool

end

(** The module used for controlling the boat. *)
module Boat : BoatSig = struct
  let boat_pos = ref (Vector2.create 400. 225.)
  let boat_h = Vector2.create 15. 10.
  let boat_v = Vector2.create 10. 15.
  let boat_face = ref boat_h

  let move (v : Vector2.t) : unit =
    if Vector2.x v = 0. then boat_face := boat_v else boat_face := boat_h;
    boat_pos := Vector2.add !boat_pos v

  let draw (() : unit) : unit =
    (* The x and y coordinates of the center of the boat for easy reference. *)
    let boatx = int_of_float (Vector2.x !boat_pos) in
    let boaty = int_of_float (Vector2.y !boat_pos) in
    (* The horiztonal and vertical radii respectively of the ellipse that
       represents the boat. *)
    let radh = Vector2.x !boat_face in
    let radv = Vector2.y !boat_face in
    draw_ellipse boatx boaty radh radv Color.brown
  
  let is_border_crossed (v : Vector2.t) : bool =
    failwith ""
end