open Base
open Aoc2022.Lib
open List
open String

type throw = Rock | Paper | Scissors
type game = throw * throw

let input : string list = read_whole_file "input/02" |> String.split ~on:'\n'

let make_throw (c : char) =
  match c with
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> Printf.sprintf "Unrecognised character: '%c'!" c |> failwith

let make_game (l : string) =
  match explode_string l with
  | [ them; ' '; us ] -> (make_throw them, make_throw us)
  | _ -> failwith "Unrecognised game!"

let shape_score (_, us) = match us with Rock -> 1 | Paper -> 2 | Scissors -> 3

let game_score game =
  match game with
  | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3
  | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6
  | _ -> 0

let part1 =
  List.map ~f:make_game input
  |> List.map ~f:(fun g -> shape_score g + game_score g)
  |> List.fold_left ~f:( + ) ~init:0

let () = Stdio.print_endline ("Part 1: " ^ Int.to_string part1)
