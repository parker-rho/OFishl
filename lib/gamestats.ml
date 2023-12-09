open Unix
open Raylib
open Window

let save_to_file (user : string) (map : string) (content : string) =
  let output_file = "player_data/" ^ user ^ "_stats.txt" in
  let out_channel = open_out output_file in
  let file_contents =
    "\n\
    \  🐪---------------------🐠\n\
    \  |  OFishl Game Results |\n\
    \  🐠---------------------🐪\n\n" ^ content
  in
  output_string out_channel file_contents;
  close_out out_channel

let print_save_notif is_confirmed user =
  match is_confirmed with
  | true ->
      print_string
        ("\nYour game data has been saved, " ^ user
       ^ ". \nView your stats at: \027[1;32m" ^ "player_data/" ^ user
       ^ "_stats.txt\027[0m")
  | false -> print_string "\nYour game data has not been saved."

let rec ask_to_save (user : string) (map : string) (content : string) =
  print_string "\n\nWould you like to save your data? (Y/N) ";
  flush Stdlib.stdout;
  try
    let decision = read_line () in
    match decision with
    | "Y" | "y" | "YES" | "yes" | "Yes" -> true
    | "N" | "n" | "NO" | "no" | "No" -> false
    | _ ->
        print_string "Invalid choice. Enter yes or no.";
        ask_to_save user map content
  with Failure _ ->
    print_string "Invalid choice. Enter yes or no.";
    ask_to_save user map content

let print_content (data : Window.game_data) (user : string) (map : string) =
  let score_string =
    "Your final score was: " ^ string_of_int data.final_score
  in
  let map_played =
    "\nGame map played: "
    ^ if map <> "custom" then "Map " ^ map else "Custom-generated map"
  in
  let purchases =
    "\nNumber of fishing rods purchased: " ^ string_of_int data.rods ^ " rods ("
    ^ string_of_int (3 * data.rods)
    ^ " score points spent in total)" ^ "\nAmount of bait purchased: "
    ^ string_of_int data.bait ^ " units of bait ("
    ^ string_of_int (1 * data.bait)
    ^ " score points spent in total)"
  in
  let closing = "\n\nThanks for playing OFishl, " ^ user ^ "! " in
  print_string (score_string ^ map_played ^ purchases ^ closing);

  score_string ^ map_played ^ purchases
