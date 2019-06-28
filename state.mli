(** the type of snakee movement direction*)
type direction =
  |Up
  |Down
  |Left
  |Right

(** [play_game ()] starts the game. *)
val play_game : unit -> unit

(** [get_snake_head snake] is the snake head in pair format. *)
val get_snake_head : 'a list list -> 'a * 'a

(** [check_eat apple apple_power snake] checks whether [snake] can eat the 
    [apple] with power [apple_power]. If the [snake] head touches any componet
    of the power apple then the apple can be eaten. *)
val check_eat : int * int -> int -> int list list -> bool

(** [snake_add_head dir snake] adds a new segment to the head of [snake] 
    following the direction [dir]. 
    Raises [Failure] if the snake segment is not a two-element list. *)
val snake_add_head : direction -> int list list -> int list list 

(** [snake_remove_tail snake] removes the last segment of [snake]. *)
val snake_remove_tail : int list list -> int list list

(** [is_dead snake enemies] checks whether [snake] hits walls or itself or
    [enemies]. 
    Raises [Failure] if the snake segment is not a two-element list. *)
val is_dead : int list list -> (int * int) list -> bool

(** [time_delay snake] is the speed depending on the length of [snake]. Large
    value means small speed. *)
val time_delay : 'a list -> int

(** [is_opposite new_dir old_dir] checks whether the new direction is the 
    opposite of the old one. For example, [Up] is the opposite of [Down]. *)
val is_opposite : direction -> direction -> bool