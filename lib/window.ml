open Raylib
open Boat
open AudioSprite
open Constants
open Score
open Sprites
open Box
open Loadables

type state =
  | StartMenu
  | Main
  | Minigame
  | Store
  | GameOver
  | Quit

let current_state = ref StartMenu
let boat = Boat.new_boat 310. 260. 30. 20.
let current_fish = ref (Fish.generate boat (Box.generate 0. 0. 0. 0.))
let current_coin = ref (Coin.generate boat (Box.generate 0. 0. 0. 0.))
let current_seamine = ref (Seamine.generate boat (Box.generate 0. 0. 0. 0.))
let score = Score.new_score ()

type game_data = {
  mutable final_score : int;
  mutable rods : int;
  mutable bait : int;
  mutable trophy : bool;
}

let game_data =
  { final_score = Score.get_score score; rods = 0; bait = 0; trophy = false }

module type WindowSig = sig
  val setup : string -> string -> Loadables.t -> unit
  val loop : string -> Loadables.t -> unit
end

module StartMenuWin : WindowSig = struct
  let play_button = Box.generate 100. 256. 100. 50.
  let quit_button = Box.generate 300. 256. 100. 50.

  let setup (map : string) (user : string) (loads : Loadables.t) =
    Raylib.set_window_title "OFishl: The OCaml Fishing Tournament";
    Box.draw play_button Color.lightgray;
    Box.draw_text play_button "Play" 25. 118. (Color.create 66 20 0 150)
      (Loadables.uni_font loads);
    Box.draw quit_button Color.beige;
    Box.draw_text quit_button "Quit" 25. 322. (Color.create 66 20 0 150)
      (Loadables.uni_font loads);
    Raylib.draw_text_ex
      (Loadables.bolden_font loads)
      "Click to play the game!" (Vector2.create 80. 120.) 40. 1.
      (Color.create 255 102 204 400)
  (* Exit button *)

  let loop (map : string) (loads : Loadables.t) =
    if Raylib.window_should_close () then current_state := Quit
    else if Box.colliding (get_mouse_position ()) play_button then (
      if is_mouse_button_down MouseButton.Left then
        Box.draw play_button Color.darkgreen;
      if is_mouse_button_released MouseButton.Left then current_state := Main;
      Box.draw_text play_button "Play" 25. 118. (Color.create 46 14 0 150)
        (Loadables.uni_font loads))
    else (
      Box.draw play_button (Color.create 41 205 63 100);
      Box.draw_text play_button "Play" 25. 118. (Color.create 46 14 0 150)
        (Loadables.uni_font loads));

    if Box.colliding (get_mouse_position ()) quit_button then (
      if is_mouse_button_down MouseButton.Left then
        Box.draw quit_button (Color.create 216 52 6 100);
      if is_mouse_button_released MouseButton.Left then current_state := Quit;
      Box.draw_text quit_button "Quit" 25. 322. (Color.create 46 14 0 150)
        (Loadables.uni_font loads))
    else (
      Box.draw quit_button (Color.create 245 110 110 100);
      Box.draw_text quit_button "Quit" 25. 322. (Color.create 46 14 0 150)
        (Loadables.uni_font loads));

    begin_drawing ();
    clear_background (Color.create 194 243 255 120);
    end_drawing ()
end

module MainWin : WindowSig = struct
  let store_box = ref (Box.generate 230. 218. 50. 50.)
  let score_box = Box.generate 350. 15. 150. 35.
  let fish_type = ref (Random.int 2)

  let setup (map : string) (user : string) (loads : Loadables.t) =
    (* TODO: *)
    if map = "2" then store_box := Box.generate 65. 410. 50. 50. else ();
    if map = "3" then store_box := Box.generate 93. 139. 50. 50. else ();
    Raylib.set_window_title (user ^ "'s Game | Map " ^ map)

  let loop (map : string) (loads : Loadables.t) =
    draw_texture (Loadables.map loads) 0 0 Color.raywhite;

    let texture_fish =
      if !fish_type = 0 then Loadables.hsufish loads
      else Loadables.kozenfish loads
    in

    let texture_trap = Loadables.trapmine loads in
    let texture_mine = Loadables.minemine loads in
    let texture_bomba = Loadables.bombamine loads in
    let texture_coin = Loadables.coinpic loads in

    begin_drawing ();

    if Raylib.window_should_close () then current_state := Quit;

    (* Responding to key presses. *)
    if
      (is_key_down Key.W || is_key_down Key.Up)
      && (is_key_down Key.A || is_key_down Key.Left)
      && (not (is_key_down Key.S || is_key_down Key.Down))
      && not (is_key_down Key.D || is_key_down Key.Right)
    then Boat.move boat (-.Constants.diag_speed ()) (-.Constants.diag_speed ())
    else if
      (is_key_down Key.A || is_key_down Key.Left)
      && (is_key_down Key.S || is_key_down Key.Down)
      && (not (is_key_down Key.D || is_key_down Key.Right))
      && not (is_key_down Key.W || is_key_down Key.Up)
    then Boat.move boat (-.Constants.diag_speed ()) (Constants.diag_speed ())
    else if
      (is_key_down Key.S || is_key_down Key.Down)
      && (is_key_down Key.D || is_key_down Key.Right)
      && (not (is_key_down Key.W || is_key_down Key.Up))
      && not (is_key_down Key.A || is_key_down Key.Left)
    then Boat.move boat (Constants.diag_speed ()) (Constants.diag_speed ())
    else if
      (is_key_down Key.D || is_key_down Key.Right)
      && (is_key_down Key.W || is_key_down Key.Up)
      && (not (is_key_down Key.A || is_key_down Key.Left))
      && not (is_key_down Key.S || is_key_down Key.Down)
    then Boat.move boat (Constants.diag_speed ()) (-.Constants.diag_speed ())
    else (
      if is_key_down Key.A || is_key_down Key.Left then
        Boat.move boat (-.Constants.get_speed ()) 0.;
      if is_key_down Key.D || is_key_down Key.Right then
        Boat.move boat (Constants.get_speed ()) 0.;
      if is_key_down Key.W || is_key_down Key.Up then
        Boat.move boat 0. (-.Constants.get_speed ());
      if is_key_down Key.S || is_key_down Key.Down then
        Boat.move boat 0. (Constants.get_speed ()));

    if is_key_pressed Key.F && Fish.colliding (Boat.get_vect boat) !current_fish
    then (
      current_state := Minigame;
      fish_type := Random.int 2;
      current_fish := Fish.generate boat !store_box);
    if Coin.colliding (Boat.get_vect boat) !current_coin then (
      AudioSprite.play (Loadables.coin_sound loads);
      current_coin := Coin.generate boat !store_box;
      Score.update_score score (Coin.get_score !current_coin));
    if Seamine.colliding (Boat.get_vect boat) !current_seamine then (
      Score.update_score score (Seamine.get_score !current_seamine);
      AudioSprite.play (Loadables.seamine_sound loads);

      current_seamine := Seamine.generate boat !store_box);
    if is_key_pressed Key.F && Box.colliding (Boat.get_vect boat) !store_box
    then current_state := Store;

    Fish.draw texture_fish !current_fish;

    (match Seamine.get_score !current_seamine with
    | -1 -> Seamine.draw texture_trap !current_seamine
    | -3 -> Seamine.draw texture_mine !current_seamine
    | -5 -> Seamine.draw texture_bomba !current_seamine
    | _ -> failwith "");

    Coin.draw texture_coin !current_coin;

    Boat.draw boat (Loadables.boat loads)
      ( is_key_down Key.W || is_key_down Key.Up,
        is_key_down Key.A || is_key_down Key.Left,
        is_key_down Key.S || is_key_down Key.Down,
        is_key_down Key.D || is_key_down Key.Right );

    Box.draw score_box (Color.create 232 253 255 150);

    Boat.border_crossed boat;

    Score.print score (Loadables.uni_font loads);
    end_drawing ()
end

module MiniWin : WindowSig = struct
  (** The current score in the minigame. *)
  let mini_score = ref 0

  (** The score required to win the minigame. *)
  let win_con = ref 10

  (** Represents the current target to be displayed in the window. *)
  let current_target = ref (Target.generate boat (Box.generate 0. 0. 0. 0.))

  let setup (map : string) (user : string) (loads : Loadables.t) =
    Raylib.set_window_title "Catch the fish!";
    win_con := if game_data.rods < 10 then 10 - game_data.rods else 1

  let loop (map : string) (loads : Loadables.t) =
    let texture_target = Loadables.bobber loads in
    if !mini_score = !win_con then (
      mini_score := 0;
      current_state := Main;
      Score.update_score score (Fish.get_score !current_fish + game_data.bait))
    else if
      is_mouse_button_pressed MouseButton.Left
      && Target.colliding (get_mouse_position ()) !current_target
    then (
      mini_score := !mini_score + Target.get_score !current_target;
      AudioSprite.play (Loadables.water_sound loads);
      current_target := Target.generate boat (Box.generate 0. 0. 0. 0.));
    begin_drawing ();
    clear_background (Color.create 168 235 255 100);
    Target.draw texture_target !current_target;
    end_drawing ()
end

module StoreWin : WindowSig = struct
  (** The current score in the minigame. *)

  (** Represents the current target to be displayed in the window. *)
  let buy_rod_button = Box.generate 100. 400. 100. 50.

  let buy_bait_button = Box.generate 300. 400. 100. 50.
  let buy_trophy_button = Box.generate 157. 300. 200. 50.
  let exit_button = Box.generate 15. 15. 15. 15.
  let score_box = Box.generate 350. 15. 150. 35.

  let setup (map : string) (user : string) (loads : Loadables.t) =
    Raylib.set_window_title "Welcome home!";
    (* Text for Store *)
    Raylib.draw_text_ex (Loadables.uni_font loads)
      "Conquer virtual waters\n       with rods and bait!"
      (Vector2.create 95. 105.) 25. 0.5 (Color.create 26 12 1 400);
    (* Buy Rod for $3 button *)
    Box.draw buy_rod_button Color.lightgray;
    Box.draw_text buy_rod_button "$3 Rod" 25. 107. (Color.create 66 20 0 150)
      (Loadables.uni_font loads);
    (* Buy Bait for $1 button *)
    Box.draw buy_bait_button Color.beige;
    Box.draw_text buy_bait_button "$1 Bait" 25. 307. (Color.create 66 20 0 150)
      (Loadables.uni_font loads);
    (* Buy Trophy for $3110 button *)
    Box.draw buy_trophy_button Color.gold;
    Box.draw_text buy_trophy_button "$3110 Trophy" 25. 175. Color.darkbrown
      (Loadables.uni_font loads);
    (* Exit button *)
    draw_texture (Loadables.exit loads) 15 15 Color.raywhite;
    (* Box around score *)
    Box.draw score_box (Color.create 232 253 255 150)

  let loop (map : string) (loads : Loadables.t) =
    if Raylib.window_should_close () then current_state := Quit
    else (
      Score.print score (Loadables.uni_font loads);
      if Box.colliding (get_mouse_position ()) buy_rod_button then (
        if
          is_mouse_button_pressed MouseButton.Left && Score.get_score score >= 3
        then
          if is_key_down Key.Left_shift && Score.get_score score >= 30 then (
            Score.update_score score (-30);
            AudioSprite.play (Loadables.click_sound loads);
            game_data.rods <- game_data.rods + 10)
          else (
            Score.update_score score (-3);
            AudioSprite.play (Loadables.click_sound loads);
            game_data.rods <- game_data.rods + 1);

        if is_mouse_button_down MouseButton.Left then
          if Score.get_score score >= 3 then (
            Box.draw buy_rod_button Color.darkgray;
            Box.draw_text buy_rod_button "$3 Rod" 25. 107.
              (Color.create 46 14 0 150) (Loadables.uni_font loads))
          else (
            Box.draw buy_rod_button (Color.create 245 110 110 100);
            Box.draw_text buy_rod_button "$3 Rod" 25. 107.
              (Color.create 46 14 0 150) (Loadables.uni_font loads)));

      if Box.colliding (get_mouse_position ()) buy_bait_button then (
        if
          is_mouse_button_pressed MouseButton.Left && Score.get_score score >= 1
        then
          if is_key_down Key.Left_shift && Score.get_score score >= 10 then (
            Score.update_score score (-10);
            AudioSprite.play (Loadables.click_sound loads);
            game_data.bait <- game_data.bait + 10)
          else (
            Score.update_score score (-1);
            AudioSprite.play (Loadables.click_sound loads);
            game_data.bait <- game_data.bait + 1);
        if is_mouse_button_down MouseButton.Left then
          if Score.get_score score >= 3 then (
            Box.draw buy_bait_button (Color.create 161 138 101 150);
            Box.draw_text buy_bait_button "$1 Bait" 25. 307.
              (Color.create 46 14 0 150) (Loadables.uni_font loads))
          else (
            Box.draw buy_bait_button (Color.create 245 110 110 100);
            Box.draw_text buy_bait_button "$1 Bait" 25. 307.
              (Color.create 46 14 0 150) (Loadables.uni_font loads)));

      if Box.colliding (get_mouse_position ()) buy_trophy_button then (
        if
          is_mouse_button_pressed MouseButton.Left
          && Score.get_score score >= 3110
        then (
          Score.update_score score (-3110);
          AudioSprite.play (Loadables.click_sound loads);
          game_data.trophy <- true;
          current_state := GameOver);
        if is_mouse_button_down MouseButton.Left then
          if Score.get_score score >= 3110 then (
            Box.draw buy_trophy_button (Color.create 194 155 0 100);
            Box.draw_text buy_trophy_button "$3110 Trophy" 25. 175.
              (Color.create 46 14 0 150) (Loadables.uni_font loads))
          else (
            Box.draw buy_trophy_button (Color.create 245 110 110 100);
            Box.draw_text buy_trophy_button "$3110 Trophy" 25. 175.
              (Color.create 46 14 0 150) (Loadables.uni_font loads)));

      if
        is_mouse_button_pressed MouseButton.Left
        && Box.colliding (get_mouse_position ()) exit_button
        || is_key_pressed Key.X
      then current_state := Main;
      begin_drawing ();
      clear_background (Color.create 255 245 237 100);
      end_drawing ())
end

module WinScreen : WindowSig = struct
  let quit_button = Box.generate 200. 256. 100. 50.

  let setup (map : string) (user : string) (loads : Loadables.t) =
    Raylib.set_window_title "CONGRATULATIONS!";
    Box.draw quit_button Color.beige;
    Box.draw_text quit_button "Quit" 25. 220. (Color.create 66 20 0 150)
      (Loadables.uni_font loads);
    Raylib.draw_text_ex
      (Loadables.bolden_font loads)
      "You are officially THE OFishl Fisher!" (Vector2.create 55. 120.) 30. 1.
      (Color.create 0 0 0 400);
    Raylib.draw_text_ex
      (Loadables.bolden_font loads)
      "Press esc to quit and record your victory!" (Vector2.create 12. 145.) 30.
      1. (Color.create 0 0 0 400)

  let loop (map : string) (loads : Loadables.t) =
    if Raylib.window_should_close () then current_state := Quit
    else if Box.colliding (get_mouse_position ()) quit_button then (
      if is_mouse_button_down MouseButton.Left then
        Box.draw quit_button (Color.create 216 52 6 100);
      if is_mouse_button_released MouseButton.Left then current_state := Quit;
      Box.draw_text quit_button "Quit" 25. 220. (Color.create 46 14 0 150)
        (Loadables.uni_font loads))
    else (
      Box.draw quit_button (Color.create 245 110 110 100);
      Box.draw_text quit_button "Quit" 25. 220. (Color.create 46 14 0 150)
        (Loadables.uni_font loads));

    begin_drawing ();
    clear_background Color.gold;
    end_drawing ()
end

let setup (map : string) (user : string) =
  Raylib.init_window 512 512 (user ^ "'s Game | Map " ^ map);
  AudioSprite.start ();
  Raylib.set_target_fps 60;
  let img = Raylib.load_image "data/fish-sprites/smallerkozen.png" in
  Raylib.unload_image img;
  Raylib.set_window_icon img;
  Raylib.set_window_min_size 512 512;
  Constants.set_speed 1.5;
  Score.set_font_size score 22.;
  Score.set_color score (Color.create 255 161 0 1000);
  Loadables.initialize map

let rec looper (map : string) (user : string) (st : state) (loads : Loadables.t)
    =
  if not (AudioSprite.is_playing (Loadables.background_sound loads)) then
    AudioSprite.play (Loadables.background_sound loads);

  match st with
  | StartMenu ->
      StartMenuWin.setup map user loads;
      StartMenuWin.loop map loads;
      looper map user !current_state loads
  | Main ->
      MainWin.setup map user loads;
      MainWin.loop map loads;
      looper map user !current_state loads
  | Minigame ->
      MiniWin.setup map user loads;
      MiniWin.loop map loads;
      looper map user !current_state loads
  | Store ->
      StoreWin.setup map user loads;
      StoreWin.loop map loads;
      looper map user !current_state loads
  | GameOver ->
      WinScreen.setup map user loads;
      WinScreen.loop map loads;
      looper map user !current_state loads
  | Quit ->
      game_data.final_score <- Score.get_score score;
      Raylib.close_window ()

let run (map : string) (user : string) =
  (* Silence verbose log output. *)
  Raylib.set_trace_log_level Error;

  let loads = setup map user in
  AudioSprite.play (Loadables.background_sound loads);
  Raylib.set_sound_volume (Loadables.coin_sound loads) 0.1;
  Raylib.set_sound_volume (Loadables.seamine_sound loads) 0.3;
  Raylib.set_sound_volume (Loadables.water_sound loads) 0.8;
  Raylib.set_sound_volume (Loadables.click_sound loads) 0.2;
  looper map user !current_state loads
