(** [reset_terminal ()] resets the terminal to the state before the 
    game begins. *)
val reset_terminal : unit -> unit

(** [whitespace num] is a string composed of only continuous whitespace
    with length [num]. *)
val whitespace : int -> string

(** [get_snake_seg snake i] gets the [i]th segment of [snake]. *)
val get_snake_seg : int list list -> int -> int list

(** [get_seg_ycorr seg] gets the y corrdinate of the snake segment [seg]. *)
val get_seg_ycorr : int list -> int

(** [get_seg_xcorr seg] gets the x corrdinate of the snake segment [seg]. *)
val get_seg_xcorr : int list -> int

(** [make_apple snake enemies] is a pair of the position of the power apple and 
    its power. *)
val make_apple : int list list -> (int * int) list -> (int * int) * int

(** [make_board w h snake apple apple_power enemies] draws the canvas with 
    [snake] and [apple] with power [apple_power] and [enemies]inside. [w] and 
    [h] are the width and height of the canvas. Current score which is 
    determined by the [snake] length is displayed under the canvas. *)
val make_board : int -> int -> int list list -> int * int -> int 
  -> (int * int) list -> unit

(** [game_over snake h_score] prints a game over box with current score and 
    history highest score over the last game board and resets terminal to 
    original state before the game started. *)
val game_over : int list list -> unit