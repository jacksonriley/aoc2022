open Base
open Aoc2022.Lib

let elves (inp : string) : string list = inp |> Str.split (Str.regexp "\n\n")

let calories (e : string list) : int list =
  e
  |> List.map ~f:(fun s -> List.map ~f:Int.of_string (String.split ~on:'\n' s))
  |> List.map ~f:(List.fold_left ~f:( + ) ~init:0)

let topn (n : int) (cals : int list) =
  cals |> List.sort ~compare |> List.rev |> flip2 List.take n
  |> List.fold_left ~f:( + ) ~init:0

let part1 (inp : string) : int = inp |> elves |> calories |> topn 1
let part2 (inp : string) : int = inp |> elves |> calories |> topn 3
let input : string = read_input_from_stdin

let () =
  input |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  input |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
