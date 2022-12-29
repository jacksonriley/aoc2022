open Base
open Aoc2022.Lib

let int_of_snafu (snafu : string) : int =
  let rec go bits multiplier so_far =
    match bits with
    | [] ->
        Printf.sprintf "%s is %d" snafu so_far |> Stdio.print_endline;
        so_far
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
  let rec go position so_far =
    if (5 ** position) / 2 >= num then so_far
    else
      let bit =
        match
          ((num % (5 ** (position + 1))) + ((5 ** position) / 2))
          / (5 ** position)
        with
        | 0 -> '0'
        | 1 -> '1'
        | 2 -> '2'
        | 3 -> '='
        | 4 -> '-'
        | 5 -> '0'
        | bad ->
            failwith
            @@ Printf.sprintf
                 "Unreachable: %d with position: %d, num: %d, so_far: %s" bad
                 position num
                 (String.of_char_list so_far)
      in

      go (position + 1) (bit :: so_far)
  in
  go 0 [] |> String.of_char_list

let part1 (inp : string list) : string =
  inp |> List.map ~f:int_of_snafu |> List.reduce_exn ~f:( + ) |> int_to_snafu

let parsed = read_input_from_stdin |> String.split_lines
let () = parsed |> part1 |> ( ^ ) "Part 1: " |> Stdio.print_endline
