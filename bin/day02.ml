open Base
open Aoc2022.Lib

type throw = Rock | Paper | Scissors
type aim = Win | Lose | Draw
type game1 = throw * throw
type game2 = throw * aim

let input : string list = read_input_from_stdin |> String.split ~on:'\n'

let make_throw (c : char) : throw =
  match c with
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> Printf.sprintf "Unrecognised character: '%c'!" c |> failwith

let make_aim (c : char) : aim =
  match c with
  | 'X' -> Lose
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> Printf.sprintf "Unrecognised character: '%c'!" c |> failwith

let make_game1 (l : string) : game1 =
  match explode_string l with
  | [ them; ' '; us ] -> (make_throw them, make_throw us)
  | _ -> failwith "Unrecognised game!"

let make_game2 (l : string) : game2 =
  match explode_string l with
  | [ them; ' '; a ] -> (make_throw them, make_aim a)
  | _ -> failwith "Unrecognised game!"

let shape_score (t : throw) : int =
  match t with Rock -> 1 | Paper -> 2 | Scissors -> 3

let winner (t : throw) : throw =
  match t with Rock -> Paper | Paper -> Scissors | Scissors -> Rock

let loser (t : throw) : throw =
  match t with Rock -> Scissors | Paper -> Rock | Scissors -> Paper

let calculate_our_throw ((their_throw, aim) : game2) : throw =
  match aim with
  | Draw -> their_throw
  | Win -> winner their_throw
  | Lose -> loser their_throw

let game1_score ((_, our_throw) as game : game1) : int =
  shape_score our_throw
  +
  match game with
  | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3
  | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6
  | _ -> 0

let game2_score ((their_throw, _) as game : game2) : int =
  let our_throw = calculate_our_throw game in
  game1_score (their_throw, our_throw)

let calculate_part maker scorer input =
  List.map ~f:maker input |> List.map ~f:scorer
  |> List.fold_left ~f:( + ) ~init:0

let part1 = calculate_part make_game1 game1_score
let part2 = calculate_part make_game2 game2_score

let () =
  input |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  input |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
