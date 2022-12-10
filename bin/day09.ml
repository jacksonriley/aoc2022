open Base
open Aoc2022.Lib

type pos = int * int [@@deriving show]
type move = Up | Down | Left | Right

let move_delta (m : move) : pos =
  match m with
  | Up -> (0, 1)
  | Down -> (0, -1)
  | Left -> (-1, 0)
  | Right -> (1, 0)

let pos_add ((x1, y1) : pos) ((x2, y2) : pos) : pos = (x1 + x2, y1 + y2)
let pos_sub ((x1, y1) : pos) ((x2, y2) : pos) : pos = (x1 - x2, y1 - y2)

let is_touching ((x1, y1) : pos) ((x2, y2) : pos) : bool =
  x1 - 1 <= x2 && x1 + 1 >= x2 && y1 - 1 <= y2 && y1 + 1 >= y2

let adjust (pos_h : pos) ((x_t, y_t) as pos_t : pos) : pos =
  if is_touching pos_h pos_t then pos_t
  else
    let dx, dy = pos_sub pos_h pos_t in
    let clamp i = min 1 @@ max (-1) i in
    (x_t + clamp dx, y_t + clamp dy)

let adjust_all (ps : pos list) : pos list =
  List.fold (List.drop ps 1)
    ~init:[ List.hd_exn ps ]
    ~f:(fun acc tl ->
      let hd = List.hd_exn acc in
      let new_tl = adjust hd tl in
      new_tl :: acc)
  |> List.rev

let make_move (ps : pos list) (m : move) : pos list =
  let hd = List.hd_exn ps in
  let tl = List.tl_exn ps in
  let new_h = pos_add hd (move_delta m) in
  adjust_all @@ (new_h :: tl)

let parse_line (l : string) : move * int =
  match String.split ~on:' ' l with
  | [ "U"; num ] -> (Up, Int.of_string num)
  | [ "D"; num ] -> (Down, Int.of_string num)
  | [ "L"; num ] -> (Left, Int.of_string num)
  | [ "R"; num ] -> (Right, Int.of_string num)
  | _ -> failwith "unexpected line"

let repeat elem num =
  let rec helper elem num acc =
    match num with 0 -> acc | n -> helper elem (n - 1) (elem :: acc)
  in
  helper elem num []

let parsed : move list =
  read_input_from_stdin |> String.split_lines |> List.map ~f:parse_line
  |> List.fold ~init:[] ~f:(fun acc (m, num) -> repeat m num @ acc)
  |> List.rev

let do_part (rope_len : int) (inp : move list) =
  List.fold inp
    ~init:[ repeat (0, 0) rope_len ]
    ~f:(fun acc m -> make_move (List.hd_exn acc) m :: acc)

let part1 = do_part 2
let part2 = do_part 10

let print_part part_fn part_num =
  parsed |> part_fn
  |> List.map ~f:(fun l -> show_pos @@ List.last_exn l)
  |> Set.of_list (module String)
     (* This crime is because it's a pain to define custom comparator witnesses *)
  |> Set.length |> Int.to_string
  |> ( ^ ) (Printf.sprintf "Part %d: " part_num)
  |> Stdio.print_endline

let () = print_part part1 1
let () = print_part part2 2
