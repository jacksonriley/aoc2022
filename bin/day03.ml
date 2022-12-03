open Base
open Aoc2022.Lib

type charset = (char, Char.comparator_witness) Set.t
type rucksack = charset * charset

let to_rucksack (line : string) : rucksack =
  let len = String.length line in
  let charlist = explode_string line in
  let first_half = List.take charlist (len / 2) in
  let second_half = List.drop charlist (len / 2) in
  (Set.of_list (module Char) first_half, Set.of_list (module Char) second_half)

let find_single_intersection (rs : charset list) : char =
  let singleton =
    match List.reduce rs ~f:Set.inter with
    | Some cs -> Set.to_list cs
    | None -> failwith "Didn't expect to be called with an empty list"
  in
  match singleton with
  | c :: [] -> c
  | l ->
      String.of_char_list l
      |> Printf.sprintf "Expected single intersection, got %s"
      |> failwith

let priority (c : char) : int =
  let baseline =
    if Char.is_uppercase c then Char.to_int 'A' - 27 else Char.to_int 'a' - 1
  in
  Char.to_int c - baseline

let input : string list = read_input_from_stdin |> String.split ~on:'\n'

let part1 (inp : string list) : int =
  inp |> List.map ~f:to_rucksack
  |> List.map ~f:(fun (f, s) -> find_single_intersection [ f; s ])
  |> List.map ~f:priority
  |> List.fold_left ~f:( + ) ~init:0

let part2 (inp : string list) : int =
  inp
  |> List.map ~f:(fun l -> Set.of_list (module Char) @@ explode_string l)
  |> List.chunks_of ~length:3
  |> List.map ~f:find_single_intersection
  |> List.map ~f:priority
  |> List.fold_left ~f:( + ) ~init:0

let () =
  input |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  input |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
