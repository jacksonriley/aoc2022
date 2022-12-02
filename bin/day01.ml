#use "topfind";;
#thread;;
#camlp4o;;
#require "core.top";;
#require "core.syntax";;
#require "str"

open Core

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let input : string = read_whole_file "input/01"
let elves : string list = input |> Str.split (Str.regexp "\n\n")

let indiv_calories : int list list =
  elves
  |> List.map ~f:(fun s -> List.map ~f:int_of_string (String.split ~on:'\n' s))

let total_calories : int list =
  indiv_calories |> List.map ~f:(List.fold_left ~f:( + ) ~init:0)

let part1 : int = total_calories |> List.fold_left ~f:max ~init:0

let part2 : int =
  List.take (total_calories |> List.sort ~compare |> List.rev) 3
  |> List.fold_left ~f:( + ) ~init:0
