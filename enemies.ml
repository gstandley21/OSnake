let width = 58

let height = 20

let ter_hei = height + 6

let ter_wid = width + 2

let row_top = 4 

let produce_random_pos ()=
  ((3 + Random.int (width-3)), (5 + Random.int (height-5)))

(**[get_all_enem_pos is_hor pos num acc] are all positions of enemies with 
   starting position [pos] and length [num]. if [is_hor] is true, then enemies 
   are horizontal otherwise vertical. [acc] is initial enemy positions. *)
let rec get_all_enem_pos is_hor pos num acc=
  if num > 0 then
    let (x,y) = pos in
    let acc' = (x,y)::acc in
    if is_hor then get_all_enem_pos is_hor ((x+1),y) (num-1) acc'
    else get_all_enem_pos is_hor (x, (y+1)) (num-1) acc'
  else acc

(** apple_extend apple power] produces the [apple] based on its [power]. The 
    bigger the int [power], the bigger the apple. *)
let apple_extent apple power = 
  match power/2 with 
  | 2 -> [apple]
  | 3 ->  [apple]
  | 4 -> [apple; ((fst apple)+ 1, (snd apple))]
  | 5 -> [apple; ((fst apple)+ 1, (snd apple)); ((fst apple)-1, (snd apple));
          ((fst apple), (snd apple) - 1); ((fst apple), (snd apple) + 1) ]
  | 6 -> [apple; ((fst apple)+ 1, (snd apple)); ((fst apple)-1, (snd apple));
          ((fst apple), (snd apple) - 1); ((fst apple), (snd apple) + 1) ]
  | 7 -> [apple; ((fst apple)+ 1, (snd apple)); ((fst apple)-1, (snd apple));
          ((fst apple), (snd apple) - 1); ((fst apple), (snd apple) + 1) ]
  | 8 ->[apple; ((fst apple)+ 1, (snd apple)); ((fst apple)-1, (snd apple));
         ((fst apple), (snd apple) - 1); ((fst apple), (snd apple) + 1) ]
  | 9 ->[apple; ((fst apple)+ 1, (snd apple)); ((fst apple)-1, (snd apple));
         ((fst apple), (snd apple) - 1); ((fst apple), (snd apple) + 1) ]
  | _ -> failwith "problem with apple_extent"

let check_conflicts snake apple apple_power enemies =
  let apple_extent = apple_extent apple apple_power in
  let rec check_enemies enemies= 
    match enemies with 
    | [] -> false
    | h::t -> 
      let x = fst h in
      let y = snd h in
      let snk_h = [fst h; snd h] in 
      List.mem h apple_extent || List.mem snk_h snake || x <= 1 || x >= width ||
      y <= 4 || y == ter_hei-1 || check_enemies t in 
      check_enemies enemies

let rec make_enemies snake apple apple_power is_hor enemies=
  let rand = 1+Random.int 5 in
  (* new-produced enemies position *)
  let enem_pos = produce_random_pos() in
  (* all enemies positions *)
  let enemies_pos = get_all_enem_pos is_hor enem_pos rand enemies in
  if check_conflicts snake apple apple_power enemies_pos ||
     (fst enem_pos) + rand > width
     (*cannot make enemies at the same positon as snake head or apple*)
  then make_enemies snake apple apple_power is_hor enemies
  else enemies_pos

let check_apple_conflicts snake enemies apple_pos apple_power  =
  let apple_extent = apple_extent apple_pos apple_power in
  let rec check_apple apple_ex enemies = 
    match apple_ex with 
    | [] -> false
    | h::t -> 
      let x = fst h in 
      let y = snd h in 
      let snk_h = [x; y] in 
      List.mem h enemies || List.mem (x+1,y) enemies || List.mem (x-1,y) enemies 
      || List.mem (x, y+1) enemies || List.mem (x, y-1) enemies || 
      List.mem snk_h snake || x <= 1 || x >= width ||  y <= 4 || y == ter_hei-1
      || check_apple t enemies  in 
      check_apple apple_extent enemies