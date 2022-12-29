open Base
open Aoc2022.Lib

let int_of_snafu (snafu : string) : int =
  let rec go bits multiplier so_far =
    match bits with
    | [] -> so_far
    | hd :: tl ->
        let bit =
          match hd with
          | '2' -> 2
          | '1' -> 1
          | '0' -> 0
          | '-' -> -1
          | '=' -> -2
          | _ -> failwith @@ Printf.sprintf "Unexpected character: %c" hd
        in
        go tl (multiplier * 5) (so_far + (multiplier * bit))
  in
  go (snafu |> String.to_list_rev) 1 0

let int_to_snafu (num : int) : string =
  let rec go current so_far =
    if phys_equal current 0 then so_far
    else
      let d = (current + 2) / 5 in
      let m = (current + 2) % 5 in
      let bit =
        match m with
        | 0 -> '='
        | 1 -> '-'
        | 2 -> '0'
        | 3 -> '1'
        | 4 -> '2'
        | _ -> failwith "Unreachable"
      in

      go d (bit :: so_far)
  in
  go num [] |> String.of_char_list

let part1 (inp : string list) : string =
  inp |> List.map ~f:int_of_snafu |> List.reduce_exn ~f:( + ) |> int_to_snafu

let parsed = read_input_from_stdin |> String.split_lines
let () = parsed |> part1 |> ( ^ ) "Part 1: " |> Stdio.print_endline
