open Base
open Aoc2022.Lib

let input = read_input_from_stdin |> explode_string

let is_start_of_packet (l : char list) (start_of_packet_len : int) : bool =
  let s = List.take l start_of_packet_len |> Set.of_list (module Char) in
  phys_equal (Set.length s) start_of_packet_len

let do_part (start_of_packet_len : int) (inp : char list) : int =
  let rec helper inp n =
    if is_start_of_packet inp start_of_packet_len then n
    else helper (List.tl_exn inp) (n + 1)
  in
  start_of_packet_len + helper inp 0

let part1 = do_part 4
let part2 = do_part 14

let () =
  input |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  input |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
