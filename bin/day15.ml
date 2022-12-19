open Base
open Aoc2022.Lib

type pos = int * int

let part1_row = 2000000
let part2_bound = 4000000

let parse_line (l : string) : pos * pos =
  let spl = l |> String.split_on_chars ~on:[ '='; ','; ':' ] |> Array.of_list in
  ( (Int.of_string spl.(1), Int.of_string spl.(3)),
    (Int.of_string spl.(5), Int.of_string spl.(7)) )

let manhattan ((x1, y1) : pos) ((x2, y2) : pos) : int =
  abs (x1 - x2) + abs (y1 - y2)

let parsed =
  read_input_from_stdin |> String.split_lines |> List.map ~f:parse_line

let get_segment (y : int) ((((x_s, y_s) as pos_s), pos_b) : pos * pos) :
    (int * int) option =
  let dist = manhattan pos_s pos_b in
  if abs (y_s - y) > dist then None
  else
    let half_width = dist - abs (y_s - y) in
    Some (x_s - half_width, x_s + half_width)

let merge_segments (segs : (int * int) list) : (int * int) list =
  let sorted =
    segs |> List.sort ~compare:(fun (l1, _) (l2, _) -> Poly.compare l1 l2)
  in
  sorted
  |> List.fold ~init:[] ~f:(fun acc (l, u) ->
         match acc with
         | [] -> [ (l, u) ]
         | (l', u') :: tl ->
             if u' >= l then (l', max u u') :: tl else (l, u) :: (l', u') :: tl)

let overlaps ((l1, u1) : int * int) ((l2, u2) : int * int) : bool =
  not (l1 > u2 || l2 > u1)

let check_row (y : int) (pairs : (pos * pos) list) : pos option =
  let merged =
    pairs
    |> List.map ~f:(get_segment y)
    |> List.filter_map ~f:(fun x -> x)
    |> List.filter ~f:(fun s -> overlaps s (0, part2_bound))
    |> merge_segments |> List.rev
  in
  match merged with [ (_, u1); _ ] -> Some (u1 + 1, y) | _ -> None

let part1 (inp : (pos * pos) list) : int =
  let merged =
    inp
    |> List.map ~f:(get_segment part1_row)
    |> List.filter_map ~f:(fun x -> x)
    |> merge_segments |> List.rev
  in
  let num_visible =
    merged
    |> List.map ~f:(fun (l, u) -> u - l + 1)
    |> List.fold ~init:0 ~f:( + )
  in
  let num_beacons_on_row =
    inp
    |> List.filter_map ~f:(fun (_, (x, y)) ->
           if phys_equal y part1_row then Some x else None)
    |> Set.of_list (module Int)
    |> Set.length
  in
  num_visible - num_beacons_on_row

let part2 (inp : (pos * pos) list) : int =
  match
    List.init (part2_bound + 1) ~f:(fun i -> i)
    |> List.map ~f:(fun y -> check_row y inp)
    |> List.filter_map ~f:(fun x -> x)
  with
  | [ (x, y) ] -> (x * part2_bound) + y
  | [] -> failwith "Didn't find beacon"
  | _ -> failwith "Found too many beacons!"

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
