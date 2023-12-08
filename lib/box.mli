open Raylib
open Color

(** The signature of the box object. *)
module type BoxSig = sig
  type t
  (** The type of a box. *)

  val generate : float -> float -> float -> float -> t
  (** [generate x y w h] is a new rectangle positioned at (x, y) with width w
      and height h. *)

  val draw : t -> Color.t -> unit
  (** Given a box, draw it at its current position with a specific color. *)

  val draw_text : t -> string -> float -> float -> Color.t -> unit

  val colliding : Vector2.t -> t -> bool
  (** Given the mouse position and a box, return true if the mouse is hovering
      over the box or not. *)
end

module Box : BoxSig