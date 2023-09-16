type item = Food | Medicine | Weapon
type creature = Quiet | Noisy

type scene =
  | SafeHouse of item list
  | Path of creature
  | Store of item list

let describe_scene s =
  match s with
  | SafeHouse items -> Printf.sprintf "SafeHouse with items: %s" (String.concat ", " (List.map (fun x -> match x with Food -> "food" | Medicine -> "medicine" | Weapon -> "weapon") items))
  | Path creature -> Printf.sprintf "Path with a %s creature" (match creature with Quiet -> "quiet" | Noisy -> "noisy")
  | Store items -> Printf.sprintf "Store with items: %s" (String.concat ", " (List.map (fun _ -> "item") items))

let move scene =
  match scene with
  | SafeHouse _ -> print_endline "You are leaving the safe house. Walk quietly."; Path (if Random.bool () then Quiet else Noisy)
  | Path _ -> print_endline "You are on the path. The store is close."; Store [Food; Medicine]
  | Store _ -> print_endline "You are in the store. Grab supplies quickly."; SafeHouse []

let check_noise level =
  if level > 7 then (
    print_endline "You made too much noise! Game Over.";
    exit 0
  )

let rec game_loop scene noise_level =
  print_endline (describe_scene scene);
  print_string "Current Noise Level: ";
  print_int noise_level;
  print_newline ();

  check_noise noise_level;

  let action = read_line () in
  let additional_noise amt = Random.int amt in

  match action with
  | "move" ->
      let new_scene = move scene in
      game_loop new_scene (noise_level + additional_noise 5)
  | "wait" -> game_loop scene (noise_level - additional_noise 3)
  | "quit" -> print_endline "Game over. You quit."
  | _ -> print_endline "Invalid action"; game_loop scene noise_level

let () =
  Random.self_init ();
  print_endline "Welcome to 'A Quiet Journey'. Type 'move', 'wait', or 'quit'.";
  let initial_scene = SafeHouse [Food; Medicine; Weapon] in
  game_loop initial_scene 0