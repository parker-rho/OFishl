open Raylib
open Boat
open Fish
open Coin
open Seamine
open AudioSprite
open Constants
open Target
open Score
open Mapcustom
open Box

type state =
  | StartMenu
  | Main
  | Minigame
  | Store
  | Settings

let current_state = ref Main
let current_fish = ref (Fish.spawn ())
let current_coin = ref (Coin.generate ())
let current_seamine = ref (Seamine.generate ())
let score = Score.new_score ()

(* Documentation of the library can be found here:
   https://tjammer.github.io/raylib-ocaml/raylib/Raylib/index.html *)

module type WindowSig = sig
  val setup : string -> string -> unit
  val loop : string -> bool -> unit
end

module MainWin : WindowSig = struct
  let store_box = ref (Box.generate 231. 231. 50. 50.)
  let score_box = ref (Box.generate 380. 15. 125. 35.)

  (** Helper function for drawing the current background by file name. *)
  let draw_background (background : string) =
    let texture_background = load_texture background in
    draw_texture texture_background 0 0 Color.raywhite

  let setup (map : string) (user : string) =
    Raylib.set_window_title (user ^ "'s Game | Map " ^ map)

  let loop (map : string) (is_custom : bool) =
    (* Custom map implemention incomplete (TODO)*)
    if is_custom then begin
      let arr = [| [| 1; 0; 1 |]; [| 1; 0; 1 |] |] in
      Custom.generate_map arr
    end
    else begin_drawing ();
    draw_background ("data/sprites/bkg" ^ map ^ ".png");

    if Raylib.window_should_close () then Raylib.close_window ()
    else if
      (* Responding to key presses. *)
      is_key_down Key.A || is_key_down Key.Left
    then Boat.move (Vector2.create ~-.2. 0.);
    if is_key_down Key.D || is_key_down Key.Right then
      Boat.move (Vector2.create 2. 0.);
    if is_key_down Key.W || is_key_down Key.Up then
      Boat.move (Vector2.create 0. ~-.2.);
    if is_key_down Key.S || is_key_down Key.Down then
      Boat.move (Vector2.create 0. 2.);
    if is_key_pressed Key.F && Fish.colliding !Boat.boat_pos !current_fish then (
      current_state := Minigame;
      Score.update_score score 3;
      current_fish := Fish.spawn ());
    if Coin.colliding !Boat.boat_pos !current_coin then (
      current_coin := Coin.generate ();
      Score.update_score score 1);
    if Seamine.colliding !Boat.boat_pos !current_seamine then (
      Score.update_score score (Seamine.get_damage !current_seamine);
      current_seamine := Seamine.generate ());
    if is_key_pressed Key.F && Box.colliding !Boat.boat_pos !store_box then
      current_state := Store;

    Boat.draw ();
    Fish.draw !current_fish;
    Seamine.draw !current_seamine;
    Coin.draw !current_coin;
    Box.draw !store_box Color.lightgray;
    Box.draw !score_box Color.white;
    Score.print score;
    end_drawing ()
end

module MiniWin : WindowSig = struct
  (** The current score in the minigame. *)
  let score = ref 0

  (** Represents the current target to be displayed in the window. *)
  let current_target = ref (Target.generate ())

  let setup (map : string) (user : string) =
    Raylib.set_window_title "Catch the fish!"

  let loop (map : string) is_custom =
    if !score = 5 then (
      score := 0;
      current_state := Main)
    else if
      is_mouse_button_pressed MouseButton.Left
      && Target.colliding (get_mouse_position ()) !current_target
    then (
      score := succ !score;
      current_target := Target.generate ());
    begin_drawing ();
    clear_background Color.raywhite;
    Target.draw !current_target;
    end_drawing ()
end

module StoreWin : WindowSig = struct
  (** The current score in the minigame. *)

  (** Represents the current target to be displayed in the window. *)
  let buy_rod_button = ref (Box.generate 100. 400. 100. 50.)

  let buy_bait_button = ref (Box.generate 300. 400. 100. 50.)
  let exit_button = ref (Box.generate 15. 15. 15. 15.)

  let setup (map : string) (user : string) =
    Raylib.set_window_title "Welcome home!";
    Box.draw !buy_rod_button Color.lightgray;
    draw_text "$3 Rod" 107 413 25 Color.black;
    Box.draw !buy_bait_button Color.beige;
    draw_text "$1 Bait" 310 413 25 Color.black;
    Box.draw !exit_button Color.red

  let loop (map : string) is_custom =
    Score.print score;
    if
      is_mouse_button_pressed MouseButton.Left
      && Score.get_score score >= 3
      && Box.colliding (get_mouse_position ()) !buy_rod_button
    then Score.update_score score (-3);
    if
      is_mouse_button_pressed MouseButton.Left
      && Score.get_score score >= 1
      && Box.colliding (get_mouse_position ()) !buy_bait_button
    then Score.update_score score (-1);
    if
      is_mouse_button_pressed MouseButton.Left
      && Box.colliding (get_mouse_position ()) !exit_button
    then current_state := Main;
    begin_drawing ();
    clear_background Color.raywhite;
    end_drawing ()
end

let setup (map : string) (user : string) =
  Raylib.init_window 512 512 (user ^ "'s Game | Map " ^ map);
  AudioSprite.start ();
  AudioSprite.play "data/audio-sprites/track1.wav";
  Raylib.set_target_fps 60

let rec looper (map : string) (user : string) (st : state) =
  let is_custom = if map <> "c" then false else true in
  match st with
  | StartMenu -> ()
  | Main ->
      MainWin.setup map user;
      MainWin.loop map is_custom;
      looper map user !current_state
  | Minigame ->
      MiniWin.setup map user;
      MiniWin.loop map is_custom;
      looper map user !current_state
  | Store ->
      StoreWin.setup map user;
      StoreWin.loop map is_custom;
      looper map user !current_state
  | Settings -> ()

let run (map : string) (user : string) =
  Raylib.set_trace_log_level Error;
  setup map user;
  (* Silence verbose log output. *)
  looper map user !current_state
