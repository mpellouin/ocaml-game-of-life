type cell = Alive | Dead

let print_cell = function
  | Alive -> print_string "■"
  | Dead -> print_string " "

let print_row row =
  Array.iter print_cell row;
  print_newline ()

let print_table table =
  Array.iter print_row table

(* let print_gen_separator width =
  print_string (String.make (width) '-');
  print_newline () *)

let init_table width height =
  Array.init height (fun _ -> Array.make width Dead)

let create_table width height =
  let table = init_table width height in
  table.(3).(1) <- Alive;
  table.(3).(2) <- Alive;
  table.(3).(3) <- Alive;
  table.(2).(3) <- Alive;
  table.(1).(2) <- Alive;
  table

let neighbours table x y =
  let width = Array.length table.(0) in
  let height = Array.length table in
  let neighbours = ref 0 in
  for i = -1 to 1 do
    for j = -1 to 1 do
      let nx = x + j in
      let ny = y + i in
      if (nx >= 0 && nx < width && ny >= 0 && ny < height) && not (i = 0 && j = 0) then
        if table.(ny).(nx) = Alive then
          incr neighbours
    done
  done;
  !neighbours


let next_state table x y =
  let n = neighbours table x y in
  match table.(y).(x) with
  | Alive when n < 2 -> Dead
  | Alive when n > 3 -> Dead
  | Dead when n = 3 -> Alive
  | cell -> cell

let next_generation table =
  let width = Array.length table.(0) in
  let height = Array.length table in
  let new_table = init_table width height in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      new_table.(y).(x) <- next_state table x y
    done
  done;
  new_table

let rec game_of_life table =
  print_table table;
  Unix.sleepf 0.2;
  ignore (Sys.command "clear");
  let table = next_generation table in
  game_of_life table

let () = 
  let table = create_table 40 40 in
  game_of_life table
