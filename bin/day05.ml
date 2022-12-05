open Base
open Aoc2022.Lib

type move = { from_stack : int; to_stack : int; count : int }
type yard = char list Array.t

let rec transpose (xss : 'a list list) : 'a list list =
  match xss with
  | [] | [] :: _ -> []
  | rows ->
      List.map ~f:List.hd_exn rows :: (transpose @@ List.map ~f:List.tl_exn rows)

(* Essentially:
   - make a grid of characters
   - transpose it
   - take the 2nd, 6th, 10th... rows (just the characters)
   - take just the characters
   - zip with [1; 2; 3; ...]
   - turn into a map
*)
let parse_yard (s : string) : yard =
  let grid = List.map ~f:explode_string @@ String.split_lines s in
  let transposed = transpose grid in
  let relevant =
    transposed |> List.chunks_of ~length:4
    |> List.map ~f:(fun l -> (Array.of_list l).(1))
  in
  let char_lists =
    List.map ~f:(fun l -> List.filter ~f:Char.is_alpha l) relevant
  in
  Array.of_list char_lists

let parse_move (s : string) : move =
  (* move 3 from 2 to 1 *)
  match String.split_on_chars ~on:[ ' ' ] s with
  | [ _; count; _; from_stack; _; to_stack ] ->
      {
        from_stack = Int.of_string from_stack;
        to_stack = Int.of_string to_stack;
        count = Int.of_string count;
      }
  | _ -> Printf.sprintf "Unexpected move line: %s" s |> failwith

let parse_input (s : string) : yard * move list =
  match s |> Str.split (Str.regexp "\n\n") with
  | [ yard_s; moves_s ] ->
      (parse_yard yard_s, List.map ~f:parse_move @@ String.split_lines moves_s)
  | _ -> Printf.sprintf "Unexpected input: %s" s |> failwith

let input = read_input_from_stdin

let rec do_move (m : move) (y : yard) : yard =
  match m.count with
  | 0 -> y
  | n -> (
      match y.(m.from_stack - 1) with
      | moving_char :: remaining ->
          y.(m.to_stack - 1) <- moving_char :: y.(m.to_stack - 1);
          y.(m.from_stack - 1) <- remaining;
          do_move
            { from_stack = m.from_stack; to_stack = m.to_stack; count = n - 1 }
            y
      | [] -> failwith "Tried to move from empty stack!")

let do_move2 (m : move) (y : yard) : yard =
  y.(m.to_stack - 1) <-
    List.concat [ List.take y.(m.from_stack - 1) m.count; y.(m.to_stack - 1) ];
  y.(m.from_stack - 1) <- List.drop y.(m.from_stack - 1) m.count;
  y

let do_part mover ((y, ms) : yard * move list) : string =
  let rec helper y ms =
    match ms with
    | m :: rest -> helper (mover m y) rest
    | [] -> Array.map ~f:List.hd_exn y
  in
  helper y ms |> Array.to_list |> String.of_char_list

let part1 (s : string) = s |> parse_input |> do_part do_move
let part2 (s : string) = s |> parse_input |> do_part do_move2
let () = input |> part1 |> ( ^ ) "Part 1: " |> Stdio.print_endline
let () = input |> part2 |> ( ^ ) "Part 2: " |> Stdio.print_endline
