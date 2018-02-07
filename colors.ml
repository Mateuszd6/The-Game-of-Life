let pi = 4. *. Pervasives.atan 1.;;

type color =
  | HSL of int * int * int
  | RGB of int * int * int;;

let to_hsl r g b = 
  failwith "Not implemented";;

let to_rgb col =
  match col with 
  | RGB(r, g, b) -> r, g, b
  | HSL(h, s, l) -> 
    if s = 0 then l, l, l (* Shade of grey. *)
    else 
      let v = 
        if l < 128 then l * (255 + s) asr 8 
        else (((l + s) lsl 8) - l*s) asr 8 in
      if v <= 0 then 0, 0, 0
      else 
        let m = l + l - v in 
        let h = h*6 in
        let sextant = h asr 8 in
        let fract = h - (sextant lsl 8) in
        let vsf = (v * fract * (v - m) / v) lsr 8 in
        let mid1 = m + vsf 
        and mid2 = v - vsf in
        if sextant = 0 then v, mid1, m
        else if sextant = 1 then mid2, v, m
        else if sextant = 2 then m, v, mid1
        else if sextant = 3 then m, mid2, v
        else if sextant = 4 then mid1, m, v
        else if sextant = 5 then v, m, mid2
        else failwith "Unexpected algorithm state.";;

let rec int_of_color col =
  match col with 
  | HSL (h, s, l) -> 
    let r, g, b = to_rgb col in     
    int_of_color (RGB(r, g, b))
  | RGB (r, g, b) -> (r lsl 16) + (g lsl 8) + b;;
  
let of_int col = 
  let r = (col asr 16) mod 256
  and g = (col asr 8) mod 256
  and  b = col mod 256 in 
  RGB (r, g, b);;

(* TODO: Change it later so that it also calcs an avarage of 
         saturation and light. *)
let avarage col_list =
  let pi = 4. *. Pervasives.atan 1. in
  let x, y, num = List.fold_left (
    fun (acc_x, acc_y, num) col -> 
      match col with
      | HSL(h, _, _) -> 
        let hue = float_of_int h /. 255. *. 360. in
        acc_x +. cos (hue /. 180. *. pi),
        acc_y +. sin (hue /. 180. *. pi),
        num + 1
      | _ -> failwith ""
  ) (0.0, 0.0, 0) col_list in
  let x = x /. float_of_int num
  and y = y /. float_of_int num in
  let new_hue = int_of_float (atan2 y x *. 180. /. pi *. 255. /. 360.) in
  let new_hue = 
    if new_hue < 0 then 255 + new_hue
    else new_hue in
  HSL(new_hue, 255, 128);;

let rec lighten = function
  | HSL (h, s, l) -> HSL(h, s, min 255 (l + l * 10 / 11))
  | RGB (r, g, b) -> to_hsl r g b |> lighten;; 