open Base
open Aoc2022.Lib

type operation = Noop | Add of int

let parse_line (s : string) : operation =
  match String.split ~on:' ' s with
  | [ "noop" ] -> Noop
  | [ "addx"; num ] -> Add (Int.of_string num)
  | _other -> failwith @@ "Unexpected line" ^ s

let parsed : operation list =
  read_input_from_stdin |> String.split_lines |> List.map ~f:parse_line

let compute_register_values (ops : operation list) : int array =
  ops
  |> List.fold ~init:[ 1 ] ~f:(fun values op ->
         let prev = List.hd_exn values in
         match op with
         | Noop -> prev :: values
         | Add x -> (prev + x) :: prev :: values)
  |> Array.of_list |> Array.rev

let calculate_signal_strength (values : int array) (idx : int) : int =
  idx * values.(idx - 1)

let is_lit (values : int array) (position : int) : bool =
  (* pos 40 -> 0 *)
  let sprite_pos = values.(position) in
  let x_pos = position % 40 in
  x_pos >= sprite_pos - 1 && x_pos <= sprite_pos + 1

let part1 (inp : operation list) : int =
  let values = inp |> compute_register_values in
  [ 20; 60; 100; 140; 180; 220 ]
  |> List.map ~f:(calculate_signal_strength values)
  |> List.fold ~init:0 ~f:( + )

let part2 (inp : operation list) : string =
  let values = inp |> compute_register_values in
  List.init (Array.length values) ~f:(fun i ->
      if is_lit values i then "#" else ".")
  |> List.chunks_of ~length:40 |> List.map ~f:String.concat
  |> String.concat ~sep:"\n"

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () = parsed |> part2 |> ( ^ ) "Part 2:\n" |> Stdio.print_endline
