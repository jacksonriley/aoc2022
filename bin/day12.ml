open Base
open Aoc2022.Lib

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

type pos = Pos.t
type map = { start : pos; finish : pos; grid : int array array }
type spot = Start of int | Finish of int | Regular of int
type pos_set = (pos, Pos.comparator_witness) Set.t

let parse_spot (c : char) : spot =
  match c with
  | 'S' -> Start 0
  | 'E' -> Finish (Char.to_int 'z' - Char.to_int 'a')
  | c -> Regular (Char.to_int c - Char.to_int 'a')

let parse_input (s : string) : map =
  let start = ref (0, 0) in
  let finish = ref (0, 0) in
  let grid =
    String.split_lines s
    |> List.mapi ~f:(fun row l ->
           l |> explode_string
           |> List.mapi ~f:(fun col c ->
                  let parsed = parse_spot c in
                  match parsed with
                  | Start height ->
                      start := (row, col);
                      height
                  | Finish height ->
                      finish := (row, col);
                      height
                  | Regular height -> height)
           |> Array.of_list)
    |> Array.of_list
  in
  { start = !start; finish = !finish; grid }

let can_move (current_height : int) (dest_height : int) : bool =
  dest_height <= current_height + 1

let neighbours ((r, c) : pos) : pos list =
  [ (r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1) ]

let rec bfs (grid : int array array) (seen : pos_set) (steps_so_far : int)
    (frontier : pos_set) (finish : pos) : int =
  let new_frontier =
    frontier |> Set.to_list
    |> List.map ~f:(fun ((pr, pc) as p) ->
           p |> neighbours
           |> List.filter ~f:(fun ((nr, nc) as n) ->
                  try
                    can_move grid.(pr).(pc) grid.(nr).(nc)
                    && not (Set.mem seen n)
                  with _ -> false))
    |> List.concat
    |> Set.of_list (module Pos)
  in
  let new_seen = Set.union seen new_frontier in
  if Set.mem new_frontier finish then steps_so_far + 1
  else bfs grid new_seen (steps_so_far + 1) new_frontier finish

let parsed = read_input_from_stdin |> parse_input

let find_all_zeroes (grid : int array array) : pos_set =
  grid |> Array.to_list
  |> List.mapi ~f:(fun r l ->
         l
         |> Array.filter_mapi ~f:(fun c height ->
                if phys_equal height 0 then Some (r, c) else None))
  |> Array.concat
  |> Set.of_array (module Pos)

let part1 (inp : map) : int =
  let seen = Set.singleton (module Pos) inp.start in
  let frontier = seen in
  bfs inp.grid seen 0 frontier inp.finish

let part2 (inp : map) : int =
  let all_starts = inp.grid |> find_all_zeroes in
  let seen = all_starts in
  let frontier = all_starts in
  bfs inp.grid seen 0 frontier inp.finish

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
