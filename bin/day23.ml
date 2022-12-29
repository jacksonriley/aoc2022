open Base
open Aoc2022.Lib

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

type dir = North | South | West | East
type map = (Pos.t, Pos.comparator_witness) Set.t

let parse_input (s : string) : map =
  s |> String.split_lines
  |> List.mapi ~f:(fun row l ->
         l |> explode_string
         |> List.filter_mapi ~f:(fun col ch ->
                match ch with
                | '.' -> None
                | '#' -> Some (row, col)
                | _ -> failwith "Unexpected char"))
  |> List.concat
  |> Set.of_list (module Pos)

let neighbours ((r, c) : Pos.t) (d : dir option) : Pos.t list =
  let north = [ (-1, 0); (-1, -1); (-1, 1) ] in
  let south = [ (1, 0); (1, -1); (1, 1) ] in
  let west = [ (0, -1); (1, -1); (-1, -1) ] in
  let east = [ (0, 1); (1, 1); (-1, 1) ] in
  let all = north @ south @ [ (0, -1); (0, 1) ] in
  let deltas =
    match d with
    | Some North -> north
    | Some South -> south
    | Some West -> west
    | Some East -> east
    | None -> all
  in
  deltas |> List.map ~f:(fun (dr, dc) -> (r + dr, c + dc))

let propose_move ((r, c) : Pos.t) (m : map) (dir_order : dir list) :
    Pos.t option =
  let checks = None :: (dir_order |> List.map ~f:(fun x -> Some x)) in
  let check_positions =
    checks |> List.map ~f:(fun check -> (check, neighbours (r, c) check))
  in
  let choices =
    check_positions
    |> List.map ~f:(fun (check, positions) ->
           let blocked =
             positions
             |> List.map ~f:(fun pos -> Set.mem m pos)
             |> List.reduce_exn ~f:( || )
           in
           (check, not blocked))
  in
  match List.filter choices ~f:(fun (_, x) -> x) with
  | [] -> None
  | (d, _) :: _ -> (
      match d with
      | None -> None
      | Some North -> Some (r - 1, c)
      | Some South -> Some (r + 1, c)
      | Some West -> Some (r, c - 1)
      | Some East -> Some (r, c + 1))

let do_step (m : map) (dir_order : dir list) : map =
  let potential_moves = Hashtbl.create (module Pos) in
  m |> Set.to_list
  |> List.iter ~f:(fun (r, c) ->
         match propose_move (r, c) m dir_order with
         | None -> ()
         | Some new_pos -> (
             match Hashtbl.find potential_moves new_pos with
             | Some _ ->
                 (* Duplicate - neither should move! This can only happen
                    head-on, so no need to worry about further duplicates *)
                 Hashtbl.remove potential_moves new_pos
             | None -> Hashtbl.set potential_moves ~key:new_pos ~data:(r, c)));
  let resolved_moves =
    potential_moves |> Hashtbl.to_alist
    |> List.map ~f:(fun (to_pos, from_pos) -> (from_pos, to_pos))
    |> Map.of_alist_exn (module Pos)
  in
  m |> Set.to_list
  |> List.map ~f:(fun from_pos ->
         match Map.find resolved_moves from_pos with
         | Some to_pos -> to_pos
         | None -> from_pos)
  |> Set.of_list (module Pos)

let extreme (f : int -> int -> bool) (xs : int list) : int =
  match xs |> List.reduce ~f:(fun acc x -> if f acc x then acc else x) with
  | Some x -> x
  | None -> failwith "empty list!"

let do_n_steps (m : map) (n : int option) : map * int =
  let rec go m' dir_order step_no =
    let new_m = do_step m' dir_order in
    let new_dir_order = List.tl_exn dir_order @ [ List.hd_exn dir_order ] in
    match n with
    | Some limit ->
        if phys_equal (step_no + 1) limit then (new_m, step_no + 1)
        else go new_m new_dir_order (step_no + 1)
    | None ->
        if Set.equal m' new_m then (new_m, step_no + 1)
        else go new_m new_dir_order (step_no + 1)
  in

  let start_dir_order = [ North; South; West; East ] in
  go m start_dir_order 0

let count_empty_squares (m : map) : int =
  let rows = m |> Set.to_list |> List.map ~f:(fun (r, _) -> r) in
  let cols = m |> Set.to_list |> List.map ~f:(fun (_, c) -> c) in
  let min_r = rows |> extreme ( < ) in
  let max_r = rows |> extreme ( > ) in
  let min_c = cols |> extreme ( < ) in
  let max_c = cols |> extreme ( > ) in
  let total_area = (max_r - min_r + 1) * (max_c - min_c + 1) in
  total_area - Set.length m

let part1 (inp : map) : int =
  let final_map, _ = do_n_steps inp (Some 10) in
  count_empty_squares final_map

let part2 (inp : map) : int =
  let _, num_steps = do_n_steps inp None in
  num_steps

let parsed = read_input_from_stdin |> parse_input

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
