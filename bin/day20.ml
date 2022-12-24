open Base
open Aoc2022.Lib

let decrypt_key = 811589153

let parse_input (s : string) : int array =
  s |> String.split_lines |> List.map ~f:Int.of_string |> Array.of_list

let mix (a : int array) (times : int) : int array =
  let len = Array.length a in
  let m =
    a
    |> Array.mapi ~f:(fun i x -> (i, (i, x)))
    |> Array.to_list
    |> Hashtbl.of_alist_exn (module Int)
  in
  for _time = 1 to times do
    for i = 0 to len - 1 do
      let current_idx, value = Hashtbl.find_exn m i in
      let value' = value % (len - 1) in
      let wrap_offset = (current_idx + value') / len in
      let new_idx = (current_idx + value' + wrap_offset) % len in
      Hashtbl.map_inplace m ~f:(fun (c, v) ->
          let move = if current_idx < c then -1 else 0 in
          (c + move, v));
      Hashtbl.map_inplace m ~f:(fun (c, v) ->
          let move = if new_idx <= c then 1 else 0 in
          (c + move, v));
      Hashtbl.set m ~key:i ~data:(new_idx, value)
    done
  done;
  let final = m |> Hashtbl.data |> Map.of_alist_exn (module Int) in
  Array.init len ~f:(fun i -> Map.find_exn final i)

let do_part (inp : int array) (times : int) : int =
  let mixed = mix inp times in
  let len = Array.length mixed in
  let zero_pos, _ = Array.findi_exn mixed ~f:(fun _ v -> phys_equal v 0) in
  [ 1000; 2000; 3000 ]
  |> List.map ~f:(fun i -> mixed.((zero_pos + i) % len))
  |> List.reduce_exn ~f:( + )

let part1 (inp : int array) = do_part inp 1

let part2 (inp : int array) =
  do_part (inp |> Array.map ~f:(fun x -> x * decrypt_key)) 10

let parsed = read_input_from_stdin |> parse_input

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
