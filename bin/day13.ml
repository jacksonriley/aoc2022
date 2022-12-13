open Base
open Aoc2022.Lib

type packet = PList of packet list | PInt of int
type ordering = Right | NotRight | Same

let rec parse_packet (cs : char list) : char list * packet =
  match cs with
  | '[' :: ']' :: rest -> (rest, PList [])
  | '[' :: rest ->
      let rest', first = parse_packet rest in
      let rest'', l = parse_packet_list rest' [ first ] in
      (rest'', PList l)
  | other ->
      let ds, rest = List.split_while ~f:Char.is_digit other in
      (rest, PInt (ds |> String.of_char_list |> Int.of_string))

and parse_packet_list (cs : char list) (so_far : packet list) :
    char list * packet list =
  match cs with
  | ']' :: rest -> (rest, List.rev so_far)
  | ',' :: rest ->
      let rest', next = parse_packet rest in
      parse_packet_list rest' (next :: so_far)
  | other -> other |> String.of_char_list |> failwith

let parse_line (l : string) : packet =
  let _, p = l |> explode_string |> parse_packet in
  p

let rec order (left : packet) (right : packet) : ordering =
  match (left, right) with
  | PInt i1, PInt i2 ->
      if i1 < i2 then Right else if i1 > i2 then NotRight else Same
  | PInt i, PList l -> order (PList [ PInt i ]) (PList l)
  | PList l, PInt i -> order (PList l) (PList [ PInt i ])
  | PList l1, PList l2 -> (
      match (l1, l2) with
      | [], [] -> Same
      | [], _ -> Right
      | _, [] -> NotRight
      | l_hd :: l_tl, r_hd :: r_tl -> (
          match order l_hd r_hd with
          | Same -> order (PList l_tl) (PList r_tl)
          | result -> result))

let parse_input (s : string) : (packet * packet) list =
  s
  |> Str.split (Str.regexp "\n\n")
  |> List.map ~f:(fun pair_string ->
         match pair_string |> String.split_lines |> List.map ~f:parse_line with
         | [ fst; snd ] -> (fst, snd)
         | _other -> failwith "Expected two packets per pair")

let parsed = read_input_from_stdin |> parse_input

let part1 (inp : (packet * packet) list) : int =
  inp
  |> List.filter_mapi ~f:(fun i (l, r) ->
         match order l r with Right -> Some (i + 1) | _ -> None)
  |> List.fold ~init:0 ~f:( + )

let part2 (inp : (packet * packet) list) : int =
  let div1 = parse_line "[[2]]" in
  let div2 = parse_line "[[6]]" in
  let flat =
    div1 :: div2 :: (inp |> List.map ~f:(fun (l, r) -> [ l; r ]) |> List.concat)
  in
  let sorted =
    flat
    |> List.sort ~compare:(fun p1 p2 ->
           match order p1 p2 with Right -> -1 | NotRight -> 1 | Same -> 0)
  in
  sorted
  |> List.foldi ~init:[] ~f:(fun i acc p ->
         match (order p div1, order p div2) with
         | Same, _ | _, Same -> (i + 1) :: acc
         | _ -> acc)
  |> List.fold ~init:1 ~f:( * )

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
