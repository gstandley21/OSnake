open Display
open Enemies

(** [getachar sl] gets one char (no matter what it is) from the imput after 
    wating for [sl] * 0.1 seconds. If there is already an input char, it will 
    return it. Raises [End_of_file] if there is no input when the time is up.*)
let getachar sl =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false; Unix.c_vmin = 0; Unix.c_vtime = sl} 
      in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

open ANSITerminal
open Unix

(** Snake movement direction. For example, [Up] means the upward movement. *)
type direction =
  |Up
  |Down
  |Left
  |Right

let get_snake_head snake =
  let head = List.hd snake in
  (List.hd head, List.nth head 1)

let check_eat apple apple_power snake =
  let head = get_snake_seg snake 0 in
  let head_x = get_seg_xcorr head in
  let head_y = get_seg_ycorr head in
  let apple_extent = apple_extent apple apple_power in
  let rec check_apple_extent apple_extent = 
    match apple_extent with 
    | [] -> false 
    | h::t ->
      let apple_x = fst h in
      let apple_y = snd h in
      (apple_x == head_x &&  apple_y == head_y ||
       (apple_x == (head_x+1) && apple_y == head_y)) || check_apple_extent t in

  check_apple_extent apple_extent

let snake_add_head (dir:direction) snake =
  match snake with
  |[] -> []
  |[x;y] :: t -> begin
      match dir with
      |Up -> [x; y-1] :: snake
      |Down -> [x; y+1] :: snake
      |Left -> [x-2; y] :: snake
      |Right -> [x+2; y] :: snake
    end
  |_ -> failwith "impossible" (** shouldn't be executed*)

let snake_remove_tail snake = 
  if List.length snake = 0 then snake else
    snake |> List.rev |> List.tl |> List.rev

let is_dead snake enemies= 
  match snake with
  | [] -> false
  | [x; y] :: t -> let (hx, hy) = get_snake_head snake in 
    y = 4 || y = ter_hei-1 || x <= 1 || x >= width || List.mem [x;y] t ||
    List.mem (hx, hy) enemies|| List.mem (hx+1, hy) enemies
  | _ -> false

(** [new_state snake apple apple_power enemies dir will_grow] creates a new 
    state for the board. The state is updated from [snake], [apple] with 
    [apple_power] and the info of whether the apple is eaten is added.
    [dir] is the new movement direction of the snake. If [will_grow], the snake 
    grows by one segment in front. *)
let new_state snake apple apple_power enemies dir (will_grow:bool)=
  set_cursor 1 row_top;
  let new_snake = if will_grow then snake |> snake_add_head dir
    else snake |> snake_add_head dir |> snake_remove_tail in
  let is_eat_apple = check_eat apple apple_power new_snake in
  let apple_seed = make_apple new_snake enemies in 
  let new_apple = if is_eat_apple then fst apple_seed else apple in 
  let new_power = if is_eat_apple then snd apple_seed else if apple_power > 4 
    then apple_power - 1 else apple_power in 
  (new_snake, new_apple, new_power, is_eat_apple)

(** [move snake apple apple_power dir will_grow enemies] draws the board with 
    new state updated from [snake], [apple] with power [apple_power] and 
    [enemies]. [dir] is the new movement direction of the snake. If [will_grow], 
    the snake grows by one segment in front.*)
let move snake apple apple_power dir (will_grow:bool) enemies=
  let (s, a, p, e) = new_state snake apple apple_power enemies dir will_grow in
  let enemies' = if e then make_enemies s a p true enemies else enemies in
  make_board width height s a p enemies';(s,a,p,enemies')

let time_delay snake =
  let len = List.length snake in
  if len <= 10 then 5
  else if len <= 20 then 4
  else if len <= 30 then 3
  else if len <= 40 then 2
  else 1

(** [receive_input snake] is the direction depends on the button being pressed. 
    [w] is up, [s] is down, [a] is left and [d] is right. If buttons other
    than this four are pressed, it continuously receive from input until one of
    wasd is pressed. *)
let rec receive_input snake=
  let time = time_delay snake in
  let input = getachar time in
  match input with
  |'w' -> Up | 's' -> Down | 'a' -> Left | 'd' -> Right
  | _ -> receive_input snake;;

let is_opposite new_dir old_dir = 
  match new_dir with
  |Up -> old_dir = Down
  |Down -> old_dir = Up
  |Left -> old_dir = Right
  |Right -> old_dir = Left

let play_game () =
  let snake = [[width/2; height/2]] in
  let seed = make_apple snake [] in 
  let apple = fst seed in
  let ap_power = snd seed in

  make_board width height snake apple ap_power [];

  (* [grow] is the number of iterations the snake should grow, incremented (by 
     2) by eating an apple. decreases by one each turn the snake grows.*)
  let rec play n_snake n_apple (apple_power:int) old_dir (grow:int) enemies = 
    let will_grow = grow > 0 in
    (try
       (let input = receive_input n_snake in
        if is_opposite input old_dir then
          failwith "maintain the old direction" else (* will be catched*)
          let (new_snake, new_apple, new_apple_power, enemies') = 
            move n_snake n_apple apple_power input will_grow enemies in 
          let new_grow = 
            (if check_eat n_apple apple_power new_snake then apple_power/2 
             else 0) + (if grow>0 then grow-1 else grow) in
          if is_dead new_snake enemies then game_over new_snake
          else play new_snake new_apple new_apple_power input new_grow enemies')
     with
     |exp -> (let input = old_dir in 
              let (new_snake, new_apple, new_apple_power, enemies') = 
                move n_snake n_apple apple_power input will_grow enemies in 
              let new_grow = 
                (if check_eat n_apple apple_power new_snake then apple_power/2 
                 else 0) + (if grow>0 then grow-1 else grow) in
              if is_dead new_snake enemies then game_over new_snake
              else 
                play new_snake new_apple new_apple_power input new_grow enemies' 
             ))
  in 
  play snake apple ap_power Left 0 
    (make_enemies snake apple 4 true [])