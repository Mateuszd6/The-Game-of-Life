type cell_info =
  {
    mutable col: Colors.color; 
    mutable is_filled: bool;
  };;

Random.self_init ();;
  
let test_color = [(Graphics.rgb 255 0 0); (Graphics.rgb 0 255 0); 
                  (Graphics.rgb 0 0 255)];;
let init_color = [(Graphics.rgb 255 0 0); (Graphics.rgb 255 0 128);
                  (Graphics.rgb 255 0 255); (Graphics.rgb 128 0 255);
                  (Graphics.rgb 0 0 255); (Graphics.rgb 0 128 255);
                  (Graphics.rgb 0 255 255); (Graphics.rgb 0 255 128);
                  (Graphics.rgb 0 255 0); (Graphics.rgb 128 255 0);
                  (Graphics.rgb 255 255 0); (Graphics.rgb 255 128 0)];;
(* TODO: Rows and colums are swapped!!! *)
let rows = 64;;
let columns = 64;;
let dir = [(1, 0); (1, 1); (0, 1); (-1,1);
           (-1,0);(-1,-1);(0,-1);(1,-1)];;
let cell_size = ref 12;;
let cell_size_delta = 2;;

(* Which generation it is. *)
let generation = ref 0;;
(* How many celss have never been alive? *)
let unpopulated = ref (rows * columns);;
let offset = ref 0;;
let pause = ref false;;
let frame_delta = [| 1. /. 60. ; 1. /. 30. ; 1. /. 8. ; 1. |];;
let speed = ref 0;;

Random.self_init ();;

(* Is cell alive? Whats the color? *)
let cells = 
  Array.init rows (fun _ -> Array.init columns (fun _ ->
    { 
      is_filled = false; 
      col = (Colors.HSL (Random.int 250, 255, 128)); 
    }
  ));;

(* Was this cell EVER alived? If so the color of the last lived cell,
  but lighten a bit is stored. *)
let visited = 
  Array.init rows (fun _ -> Array.init columns (fun _ ->
    { 
      is_filled = false;
      col = (Colors.HSL (0, 0, 255))
    }
  ));;

let visit_cell i j = 
  if visited.(i).(j).is_filled = false then begin
    visited.(i).(j).is_filled <- true;
    unpopulated := !unpopulated - 1
  end;
  visited.(i).(j).col <-Colors.lighten cells.(i).(j).col;;

let inicialize_game () = 
  generation := 0;
  unpopulated := (rows * columns);
  pause := false;
  speed := 0;
  for i = 0 to rows - 1 do
    for j = 0 to columns - 1 do
      let is_brick = Random.int 2 = 1 in begin
        cells.(i).(j).is_filled <- is_brick;
        cells.(i).(j).col <- Colors.HSL (Random.int 255, 255, 128);
        visited.(i).(j).is_filled <- false;
        visited.(i).(j).col <- (Colors.HSL (0, 0, 255));
        if is_brick then visit_cell i j
      end
    done
  done;;

(* Generate next state of the game. *)
let generate_next () =
  let get_neib (i, j) = 
    let add_vectors (a, b) (c, d) = (a + c, b + d) in
    dir |> List.map (add_vectors (i, j)) 
    |> List.filter (fun (a, b) -> 
      a >= 0 && a < rows && b >= 0 
      && b < columns && cells.(a).(b).is_filled <> false)
  and tmp = Array.make_matrix rows columns false in
  for i = 0 to rows - 1 do
    for j = 0 to columns - 1 do 
      let neib = get_neib (i, j) in 
      let childs = List.length neib in 
      if cells.(i).(j).is_filled <> false || childs = 3 then
        tmp.(i).(j) <- List.mem childs [2;3];
      if cells.(i).(j).is_filled = false && childs = 3 then begin
        cells.(i).(j).col <- Colors.avarage (neib |> 
          List.filter (fun (a, b) -> cells.(a).(b).is_filled <> false) |>
          List.map (fun (a, b) -> cells.(a).(b).col));
          visit_cell i j
      end
    done
  done;
  for i = 0 to rows - 1 do
    for j = 0 to columns - 1 do
      cells.(i).(j).is_filled <- tmp.(i).(j)
    done
  done;
  generation := !generation + 1;;

let prompt () = 
  "Generation: " ^ (string_of_int !generation) ^ 
  "   Unpopulated: " ^ (string_of_int !unpopulated) ^ 
  "   Faster: F   Slower: S   Pause: P   Restart: R";;

(** Get coords x,y and print if there is a living cell over there. *)
let draw_cell x y = 
  if cells.(x).(y).is_filled <> false then
    Graphics.set_color (Colors.int_of_color cells.(x).(y).col)
  else
    Graphics.set_color (Colors.int_of_color visited.(x).(y).col);
  Graphics.fill_rect 
    (!cell_size*x + !offset + 2) (!cell_size*y + !offset + 2) 
    (!cell_size - 2) (!cell_size - 2);;

Graphics.open_graph "";;
Graphics.auto_synchronize false;;
inicialize_game ();

while true do
  if Graphics.button_down () then begin
    let mouse_x, mouse_y = Graphics.mouse_pos () in
    if mouse_x >= 0 && mouse_x < !cell_size * columns 
    && mouse_y >= 0 && mouse_y < !cell_size * rows then begin
      let cell_x, cell_y = mouse_x / !cell_size, mouse_y / !cell_size in
      let cur_col = Colors.HSL (Random.int 255, 255, 128) in
      cells.(cell_x).(cell_y).is_filled <- true;
      cells.(cell_x).(cell_y).col <- cur_col;
      visit_cell cell_x cell_y
      end;
  end;

  Graphics.set_color Graphics.black;
  Graphics.draw_rect (!offset) (!offset) 
    (rows * !cell_size) (columns * !cell_size);  
  Graphics.fill_rect 0 0 max_int max_int;

  for i = 0 to rows - 1 do
    for j = 0 to columns - 1 do
      draw_cell i j;
    done;
  done;
  
  Graphics.set_color (Graphics.rgb 40 40 40);
  Graphics.fill_rect (!offset) (!offset) 
    (rows * !cell_size) (!cell_size );

  Graphics.moveto (!offset) (!offset);
  Graphics.set_color Graphics.white;
  Graphics.draw_string (prompt ());

  if Graphics.key_pressed () then begin
    let c = Graphics.read_key () in 
    (* Zooming is unsupported: *)
    (* 
    if c = '+' || c = '='then begin 
        cell_size := !cell_size + cell_size_delta;
        offset := !offset - (cell_size_delta * rows) / 2
      end
      else if c = '-' || c = '_' then begin
        cell_size := !cell_size - cell_size_delta;
        offset := !offset + (cell_size_delta * rows )/ 2
      end
      else
    *)
    if c = 's' then 
      speed := let q = (!speed + 1) and max = Array.length frame_delta -1 in 
        if q > max then max else q
    else if c = 'f' then
      speed := let q = (!speed - 1) in if q < 0 then 0 else q
    else if c = 'p' then
      pause := not !pause
    else if c = 'r' then
      inicialize_game ();
  end;

  Graphics.synchronize ();

  if not !pause then
    Unix.sleepf frame_delta.(!speed)
  else 
    Unix.sleepf frame_delta.(0);
  Graphics.clear_graph ();
  if not !pause then 
    generate_next ()
done;;