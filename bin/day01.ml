open Base
open Aoc2022.Lib
open Stdio

let input : string = read_whole_file "input/01"
let elves : string list = input |> Str.split (Str.regexp "\n\n")

let indiv_calories : int list list =
  elves
  |> List.map ~f:(fun s -> List.map ~f:Int.of_string (String.split ~on:'\n' s))

let total_calories : int list =
  indiv_calories |> List.map ~f:(List.fold_left ~f:( + ) ~init:0)

let part1 : int = total_calories |> List.fold_left ~f:max ~init:0

let part2 : int =
  List.take (total_calories |> List.sort ~compare |> List.rev) 3
  |> List.fold_left ~f:( + ) ~init:0

let () = Stdio.print_endline ("Part 1: " ^ Int.to_string part1)
let () = Stdio.print_endline ("Part 2: " ^ Int.to_string part2)
