open Base
open Aoc2022.Lib

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

type bliz_map = {
  min_r : int;
  max_r : int;
  min_c : int;
  max_c : int;
  period : int;
  bzs : (Pos.t, Pos.t list, Pos.comparator_witness) Map.t;
}

type bliz_maps = (int, bliz_map, Int.comparator_witness) Map.t

let calc_lcm (x : int) (y : int) : int =
  let m = ref x in
  while not (phys_equal 0 (!m % y)) do
    m := !m + x
  done;
  !m

let parse_input (s : string) : bliz_map =
  let lines = String.split_lines s in
  let min_r, max_r = (0, List.length lines - 1) in
  let min_c, max_c = (0, String.length (List.hd_exn lines) - 1) in
  let period = calc_lcm (max_r - min_r - 1) (max_c - min_c - 1) in
  let bzs =
    lines
    |> List.mapi ~f:(fun row l ->
           l |> explode_string
           |> List.filter_mapi ~f:(fun col ch ->
                  match ch with
                  | '<' -> Some ((row, col), [ (0, -1) ])
                  | '>' -> Some ((row, col), [ (0, 1) ])
                  | '^' -> Some ((row, col), [ (-1, 0) ])
                  | 'v' -> Some ((row, col), [ (1, 0) ])
                  | '.' | '#' -> None
                  | _ -> failwith "Unexpected char"))
    |> List.concat
    |> Map.of_alist_reduce (module Pos) ~f:( @ )
  in
  { min_r; max_r; min_c; max_c; period; bzs }

let step_blizzards (bzs : bliz_map) : bliz_map =
  let new_bzs =
    bzs.bzs |> Map.to_alist
    |> List.map ~f:(fun ((r, c), dirs) ->
           dirs
           |> List.map ~f:(fun (dr, dc) ->
                  let poss_new_r = r + dr in
                  let poss_new_c = c + dc in
                  let new_r =
                    if poss_new_r <= bzs.min_r then bzs.max_r - 1
                    else if poss_new_r >= bzs.max_r then bzs.min_r + 1
                    else poss_new_r
                  in
                  let new_c =
                    if poss_new_c <= bzs.min_c then bzs.max_c - 1
                    else if poss_new_c >= bzs.max_c then bzs.min_c + 1
                    else poss_new_c
                  in
                  ((new_r, new_c), [ (dr, dc) ])))
    |> List.concat
    |> Map.of_alist_reduce (module Pos) ~f:( @ )
  in
  { bzs with bzs = new_bzs }

let compute_bliz_maps (bzs : bliz_map) : bliz_maps =
  let _, maps =
    List.init bzs.period ~f:(fun i -> i)
    |> List.fold
         ~init:(bzs, Map.empty (module Int))
         ~f:(fun (last_map, acc) minute ->
           let new_map = step_blizzards last_map in
           (new_map, Map.set acc ~key:minute ~data:last_map))
  in
  maps

let pos_equals (r, c) (r', c') : bool = phys_equal r r' && phys_equal c c'

let traverse (bzs : bliz_map) (start : Pos.t) (finish : Pos.t)
    (start_minute : int) : int =
  let all_maps = compute_bliz_maps bzs in
  let rec bfs frontier minute =
    if Set.mem frontier finish then minute
    else
      let current_map = Map.find_exn all_maps ((minute + 1) % bzs.period) in
      let new_frontier =
        frontier |> Set.to_list
        |> List.map ~f:(fun (r, c) ->
               [ (r, c); (r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1) ]
               |> List.filter ~f:(fun (r', c') ->
                      if pos_equals (r', c') start || pos_equals (r', c') finish
                      then true
                      else if
                        r' <= bzs.min_r || r' >= bzs.max_r || c' <= bzs.min_c
                        || c' >= bzs.max_c
                      then false
                      else
                        match Map.find current_map.bzs (r', c') with
                        | Some _ -> false
                        | None -> true))
        |> List.concat
        |> Set.of_list (module Pos)
      in
      bfs new_frontier (minute + 1)
  in

  bfs (Set.of_list (module Pos) [ start ]) start_minute

let part1 (inp : bliz_map) : int =
  let start = (inp.min_r, inp.min_c + 1) in
  let finish = (inp.max_r, inp.max_c - 1) in
  traverse inp start finish 0

let part2 (inp : bliz_map) : int =
  let start = (inp.min_r, inp.min_c + 1) in
  let finish = (inp.max_r, inp.max_c - 1) in
  let first_leg = traverse inp start finish 0 in
  let back_for_snacks = traverse inp finish start first_leg in
  traverse inp start finish back_for_snacks

let parsed = read_input_from_stdin |> parse_input

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
