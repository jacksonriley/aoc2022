open Base
open Aoc2022.Lib

type 'a mat = 'a array array
type treemap = int array array
type direction = Up | Down | Left | Right [@@deriving enumerate]

let parsed : treemap =
  read_input_from_stdin |> String.split_lines |> Array.of_list
  |> Array.map ~f:(fun s ->
         explode_string s |> List.map ~f:Char.get_digit_exn |> Array.of_list)

(* Return a map of how tall a tree would have to be in order to be visible from
   a certain direction *)
let compute_direction_visibility (trees : treemap) (d : direction) : treemap =
  let num_rows = Array.length trees in
  let num_cols = Array.length trees.(0) in
  let result =
    Array.init num_rows ~f:(fun _ -> Array.init num_cols ~f:(fun _ -> 0))
  in
  let trees =
    match d with
    | Left -> trees
    | Right -> Array.map trees ~f:Array.rev
    | Up -> Array.transpose_exn trees
    | Down -> Array.transpose_exn trees |> Array.map ~f:Array.rev
  in
  for r = 0 to num_rows - 1 do
    let visible_height = ref (-1) in
    for c = 0 to num_cols - 1 do
      result.(r).(c) <- !visible_height;
      visible_height := max !visible_height trees.(r).(c)
    done
  done;
  let final =
    match d with
    | Left -> result
    | Right -> Array.map result ~f:Array.rev
    | Up -> Array.transpose_exn result
    | Down -> Array.map result ~f:Array.rev |> Array.transpose_exn
  in
  (* Stdio.print_endline @@ show_direction d;
     Stdio.print_endline @@ show_treemap final; *)
  final

let compute_visible (trees : treemap) (vs : treemap list) : bool mat =
  let num_rows = Array.length trees in
  let num_cols = Array.length trees.(0) in
  let result =
    Array.init num_rows ~f:(fun _ -> Array.init num_cols ~f:(fun _ -> false))
  in
  for r = 0 to num_rows - 1 do
    for c = 0 to num_cols - 1 do
      let this_height = trees.(r).(c) in
      let requirement =
        vs
        |> List.map ~f:(fun v -> v.(r).(c))
        |> List.fold_left ~f:min ~init:Int.max_value
      in
      result.(r).(c) <- this_height > requirement
    done
  done;
  result

let calculate_scenic_score (trees : treemap) (r : int) (c : int) : int =
  let num_rows = Array.length trees in
  let num_cols = Array.length trees.(0) in
  let current_height = trees.(r).(c) in
  if
    phys_equal r 0
    || phys_equal r (num_rows - 1)
    || phys_equal c 0
    || phys_equal c (num_cols - 1)
  then 0
  else
    let trees_trans = Array.transpose_exn trees in
    let right = List.drop (Array.to_list trees.(r)) (c + 1) in
    let left =
      List.drop (trees.(r) |> Array.rev |> Array.to_list) (num_cols - c)
    in
    let down = List.drop (trees_trans.(c) |> Array.to_list) (r + 1) in
    let up =
      List.drop (trees_trans.(c) |> Array.rev |> Array.to_list) (num_rows - r)
    in
    let num_vis l =
      min (List.length l)
      @@ 1
         + (List.take_while ~f:(fun h -> h < current_height) l |> List.length)
    in
    num_vis right * num_vis left * num_vis up * num_vis down

let part1 (parsed : treemap) : int =
  let visible =
    all_of_direction
    |> List.map ~f:(compute_direction_visibility parsed)
    |> compute_visible parsed
  in
  Array.fold ~init:0
    ~f:(fun acc row ->
      acc + Array.fold ~init:0 ~f:(fun acc' x -> acc' + if x then 1 else 0) row)
    visible

let part2 (parsed : treemap) : int =
  let num_rows = Array.length parsed in
  let num_cols = Array.length parsed.(0) in
  let scores =
    Array.init num_rows ~f:(fun _ -> Array.init num_cols ~f:(fun _ -> 0))
  in
  for r = 0 to num_rows - 1 do
    for c = 0 to num_cols - 1 do
      scores.(r).(c) <- calculate_scenic_score parsed r c
    done
  done;
  Array.fold ~init:0
    ~f:(fun acc row ->
      max acc @@ Array.fold ~init:0 ~f:(fun acc' x -> max acc' x) row)
    scores

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
