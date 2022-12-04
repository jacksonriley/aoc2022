open Base
open Aoc2022.Lib

type range = int * int
type pair = range * range

let parse_range (s : string) : range =
  match String.split ~on:'-' s with
  | [ l; h ] -> (Int.of_string l, Int.of_string h)
  | other -> failwith ""

let parse_pair (s : string) : pair =
  match String.split ~on:',' s with
  | [ e1; e2 ] -> (parse_range e1, parse_range e2)
  | other -> failwith ""

let fully_contains (((l1, h1), (l2, h2)) : pair) : bool =
  (l1 <= l2 && h1 >= h2) || (l2 <= l1 && h2 >= h1)

let overlap_at_all (((l1, h1), (l2, h2)) as p : pair) : bool =
  (l1 <= l2 && l2 <= h1) || (l1 <= h2 && h2 <= h1) || fully_contains p

let input : pair list =
  read_input_from_stdin |> String.split ~on:'\n' |> List.map ~f:parse_pair

let part1 (inp : pair list) : int =
  List.filter inp ~f:fully_contains |> List.length

let part2 (inp : pair list) : int =
  List.filter inp ~f:overlap_at_all |> List.length

let () =
  input |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  input |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
