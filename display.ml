open Unix
open ANSITerminal
open Enemies


let reset_terminal () = 
  let termio = Unix.tcgetattr Unix.stdin in
  let new_ter =
    { termio with Unix.c_icanon = true; Unix.c_vmin = 1; Unix.c_vtime=0} in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN new_ter

let rec whitespace num = 
  if num = 0 then "" else
  if num = 1 then " " 
  else " " ^ whitespace (num-1)

let get_snake_seg snake i=
  List.nth snake i

let get_seg_ycorr seg =
  List.nth seg 1

let get_seg_xcorr seg =
  List.nth seg 0

(** The format of each snake segment. *)
let snake_seg =
  "[]"

(** [draw_snake snake] draws [snake] according to the cordinates 
    contained in it.*)
let rec draw_snake snake = 
  match snake with
  |[] -> ()
  |h :: t -> set_cursor (get_seg_xcorr h) (get_seg_ycorr h);
    print_string[green] (snake_seg);draw_snake t

let rec make_apple snake enemies =  
  let power = 4 + if Random.int 5 == 1 then 15 else 0 in 
  let rec good_pos power = 
    let rand_pos = produce_random_pos() in 
    if check_apple_conflicts snake enemies rand_pos power then good_pos power 
    else rand_pos in 
  (good_pos power, power)

(**[draw_verti_edge w h] drows the vertical boundaries with height [h]. 
    The distance between two vertical lines is [w]. *)
let rec draw_verti_edge w h = 
  let row = "|" ^ (whitespace (w)) ^ "|"
  in
  if h = 1 then (print_endline row; print_endline) 
  else (print_endline row; draw_verti_edge w (h-1))

(**[draw_horiz_edge w] draws the horizontal bondrary of the board with 
   length [w]. *)
let rec draw_horiz_edge w  = 
  if w = 1 then "-" 
  else "-" ^ draw_horiz_edge (w-1)

(**[draw_enemies enemies] draws the [enemies] on the board. *)
let rec draw_enemies = function
  | [] -> ()
  | (x, y) :: t -> set_cursor x y;
    print_string[blue] ("*"); draw_enemies t

(** [draw_apple apple apple_power] draws the apple on the board at position 
    [apple] according to its power level [apple_power]. Apple power minimum 
    is 4. 
    Raises [Failure] it the apple_power is larger than [20]. *)
let draw_apple apple apple_power =
  (*stage starts at 10*)
  match apple_power/2 with 
  | 2 -> set_cursor (fst(apple)) (snd(apple));
    print_string [red]"o"(*regular apple*)
  | 3 ->  set_cursor (fst(apple)) (snd(apple));
    print_string [magenta]"O"
  | 4 -> set_cursor (fst(apple)) (snd(apple));
    print_string [magenta]"OO"
  | 5 -> set_cursor (fst(apple)) (snd(apple)-1);
    print_string [magenta]"o";
    set_cursor (fst(apple)-1) (snd(apple));
    print_string [magenta]"OOO";
    set_cursor (fst(apple)) (snd(apple) + 1);
    print_string [magenta]"o"
  | 6 -> set_cursor (fst(apple)) (snd(apple)-1);
    print_string [magenta]"O";
    set_cursor (fst(apple)-1) (snd(apple));
    print_string [magenta]"OOO";
    set_cursor (fst(apple)) (snd(apple) + 1);
    print_string [magenta]"O"
  | 7 -> set_cursor (fst(apple)) (snd(apple)-1);
    print_string [magenta]"O";
    set_cursor (fst(apple)-1) (snd(apple));
    print_string [magenta]"OOO";
    set_cursor (fst(apple)) (snd(apple) + 1);
    print_string [magenta]"O";
  | 8 -> set_cursor (fst(apple)) (snd(apple)-1);
    print_string [magenta]"O";
    set_cursor (fst(apple)-1) (snd(apple));
    print_string [magenta]"OOO";
    set_cursor (fst(apple)) (snd(apple) + 1);
    print_string [magenta]"O";
  | 9 -> set_cursor (fst(apple)) (snd(apple)-1);
    print_string [magenta]"O";
    set_cursor (fst(apple)-1) (snd(apple));
    print_string [magenta]"OOO";
    set_cursor (fst(apple)) (snd(apple) + 1);
    print_string [magenta]"O";
  | _ -> failwith "problem with stage"

let make_board w h snake apple apple_power enemies=
  print_endline (" " ^ draw_horiz_edge (w));
  draw_verti_edge w h;
  print_endline (" " ^ draw_horiz_edge (w));
  let pos = (1,26) in
  set_cursor (fst apple) (snd apple);
  draw_apple apple apple_power; 
  draw_snake snake;
  draw_enemies enemies;
  set_cursor (fst pos) ((snd pos)+1);
  print_string[blue] ("  Score: " ^ string_of_int(List.length snake) ^ 
                      whitespace(w-10));
  set_cursor (fst pos) ((snd pos)+4)

let update_h_score old_sc new_sc  = 
  if new_sc > old_sc then new_sc
  else old_sc

(** [update_ath_score curr_sc] holds the value of the high score for this 
    session of the game. It only updates the score stroed in 
    [all_time_high_score.txt] to [curr_sc] if the the latter 
    is greater. *)
let update_ath_score curr_sc = 
  let score_file =  open_in("all_time_high_score.txt") in 
  let standing_sc = int_of_string (input_line score_file) in 
  close_in score_file;

  if standing_sc > curr_sc then standing_sc
  else
    let outf =  open_out "all_time_high_score.txt" in 
    Printf.fprintf (outf) "%s\n" (string_of_int curr_sc);
    close_out outf; 
    curr_sc

let game_over snake = 
  let pos = (ter_wid, ter_hei) in
  let box_w = 30 in 
  let box_h = 5 in 
  set_cursor (width/2 - box_w/2) (height/2 + (box_h/2 -3));
  print_endline (" " ^ draw_horiz_edge box_w);

  let rec vert xpos ypos h = 
    set_cursor xpos ypos;
    if h = 0 then (print_endline ("|" ^ (whitespace (box_w)) ^ "|"); 
                   print_endline) 
    else (print_endline ("|" ^ (whitespace (box_w)) ^ "|"); 
          vert xpos (ypos+1) (h-1))
  in
  vert (width/2 -box_w/2) (height/2 + (box_h/2 -2)) 5;

  set_cursor (width/2 -box_w/2) (height/2 + (box_h/2 + 4));
  print_endline (" " ^ draw_horiz_edge box_w);

  set_cursor (width/2 -4) (height/2 + (box_h/2 -1 ));
  print_string[red] ("GAME OVER ");
  set_cursor (width/2 - 4) (height/2 + (box_h/2 + 1));

  let curr_score = List.length snake in 
  let all_time_high_score = update_ath_score curr_score in 

  print_string[blue] ("Score: " ^ string_of_int curr_score);
  (* ^ ", Best Score: " ^ string_of_int(h_score));*)
  (*put this back in if we implement losing segments*)
  set_cursor (width/2 - 11) (height/2 + (box_h/2 + 2));
  print_string[blue] ("All Time High Score: " ^ string_of_int 
                        all_time_high_score);
  set_cursor (fst pos) ((snd pos)+4);

  reset_terminal()