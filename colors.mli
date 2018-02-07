type color =
  | HSL of int * int * int
  | RGB of int * int * int;;

(** Make a color of int that stores RGB values.. *)
val of_int: int -> color;;

(* Return an int representation of the color. *)
val int_of_color: color -> int;;

(** Calculates color avarage. For now it does only hue calculation,
    Saturation and lightness is not suported. *)
val avarage: color list -> color;; 

(* Change the lightness of the color. *)
val lighten: color -> color;;
