(** Width of the canvas. *)
val width : int

(** Height of the canvas. *)
val height : int

(** Height of the terminal. *)
val ter_hei : int

(** Width of the terminal. *)
val ter_wid : int

(** y cordinate of the top line of the canvas.*)
val row_top : int

(** [produce_random_pos ()] is a random position inside the canvas. *)
val produce_random_pos: unit -> int * int

(** [check_power_apple_conflicts snake enemies apple_pos apple_power] checks 
    whether the [snake] head or apple at position [apple_pos] with power
    [apple_power] overlaps with [enemies]. *)
val check_apple_conflicts : int list list -> (int * int) list -> int * int -> 
  int -> bool

(** [apple_extent apple power] is the power apple position, which is a list
    of all its components cordinates. The power apple is produced around
    [apple] and it's size depends on [power]. Larger power conresponds to
    larger power apple. 
    Raises [Failure] if [power] is greater than 13. *)
val apple_extent : int * int -> int -> (int * int) list

(**[check_conflicts snake apple apple_power enemies] checks whether the [snake] 
   head or [apple] with power [apple_power] overlaps with [enemies]. *)
val check_conflicts : int list list -> int * int -> int -> (int * int) list -> 
    bool

(**[make_enemies snake apple apple_power is_hor enemies] are positions of all 
   enemies, which include both already-existed and new produced one. The new 
   produced enemies' orientation depends on [is_hor] and their length is 
   between [1] and [5] (inclusive). It's guranteed that the enemies will not
   be produced above [apple] or [snake]. *)
val make_enemies: int list list -> int * int -> int -> bool -> (int * int) list
  -> (int * int) list