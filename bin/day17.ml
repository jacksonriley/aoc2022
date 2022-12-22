open Base
open Aoc2022.Lib

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module StateKey = struct
  module T = struct
    type t = { rock_idx : int; jet_idx : int; profile : int list }
    [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

type pos = Pos.t
type pos_set = (pos, Pos.comparator_witness) Set.t
type state = (StateKey.t, int * int, StateKey.comparator_witness) Map.t

let rocks : pos list array =
  [
    (* #### *)
    [ (2, 0); (3, 0); (4, 0); (5, 0) ];
    (* .#. *)
    (* ### *)
    (* .#. *)
    [ (3, 2); (2, 1); (3, 1); (4, 1); (3, 0) ];
    (* ..# *)
    (* ..# *)
    (* ### *)
    [ (4, 2); (4, 1); (2, 0); (3, 0); (4, 0) ];
    (* # *)
    (* # *)
    (* # *)
    (* # *)
    [ (2, 0); (2, 1); (2, 2); (2, 3) ];
    (* ## *)
    (* ## *)
    [ (2, 0); (3, 0); (2, 1); (3, 1) ];
  ]
  |> Array.of_list

let parse_input (s : string) : int array =
  s |> explode_string
  |> List.map ~f:(fun c ->
         match c with
         | '>' -> 1
         | '<' -> -1
         | _ -> failwith "Unexpected direction")
  |> Array.of_list

let maxlist (l : int list) : int =
  l |> List.fold ~init:0 ~f:(fun acc x -> if acc > x then acc else x)

let calc_height (p : pos_set) : int =
  p |> Set.to_list |> List.map ~f:(fun (_, y) -> y) |> maxlist

let drop_rock (rock : pos list) (existing : pos_set) (jet_idx_start : int)
    (js : int array) : int * pos_set =
  let rec go jet_idx r =
    (* Try to move according to jet *)
    let naive_blown_r =
      r |> List.map ~f:(fun (x, y) -> (x + js.(jet_idx), y))
    in
    let blown_r =
      if
        naive_blown_r
        |> List.filter ~f:(fun (x, _) -> x < 0 || x > 6)
        |> List.length > 0
        || naive_blown_r
           |> Set.of_list (module Pos)
           |> Set.inter existing |> Set.length > 0
      then r
      else naive_blown_r
    in
    let naive_dropped_r = blown_r |> List.map ~f:(fun (x, y) -> (x, y - 1)) in
    let at_rest =
      naive_dropped_r
      |> Set.of_list (module Pos)
      |> Set.inter existing |> Set.length > 0
    in
    if at_rest then
      ( (jet_idx + 1) % Array.length js,
        Set.union existing (blown_r |> Set.of_list (module Pos)) )
    else go ((jet_idx + 1) % Array.length js) naive_dropped_r
  in
  let top = existing |> calc_height in
  let shifted_rock = rock |> List.map ~f:(fun (x, y) -> (x, y + top + 4)) in
  go jet_idx_start shifted_rock

let drop_n_rocks ?(existing : pos_set option = None) ?(rock_idx = 0)
    ?(jet_idx = 0) (n : int) (js : int array) : int =
  let rec go nth_rock rock_idx jet_idx existing =
    if phys_equal nth_rock n then existing |> calc_height
    else
      let jet_idx', existing' =
        drop_rock rocks.(rock_idx % Array.length rocks) existing jet_idx js
      in
      go (nth_rock + 1) (rock_idx + 1) jet_idx' existing'
  in
  let floor =
    match existing with
    | Some x -> x
    | None -> List.init 7 ~f:(fun x -> (x, 0)) |> Set.of_list (module Pos)
  in
  go 0 rock_idx jet_idx floor

let find_cycle_length (js : int array) :
    int * int * int * int * int * int * pos_set =
  let rec go idx rock_idx jet_idx existing (states : state) =
    let profile_abs =
      List.init 7 ~f:(fun i -> i)
      |> List.map ~f:(fun x ->
             existing |> Set.to_list
             |> List.filter_map ~f:(fun (x', y) ->
                    if phys_equal x x' then Some y else None)
             |> maxlist)
    in
    let topline = profile_abs |> maxlist in
    let profile = profile_abs |> List.map ~f:(fun y -> topline - y) in
    let height = existing |> calc_height in
    match Map.find states { rock_idx; jet_idx; profile } with
    | Some (prev_idx, prev_height) ->
        ( idx - prev_idx,
          prev_height,
          prev_idx,
          height - prev_height,
          rock_idx,
          jet_idx,
          existing )
    | None ->
        let states' =
          Map.add_exn states
            ~key:{ rock_idx; jet_idx; profile }
            ~data:(idx, height)
        in
        let jet_idx', existing' =
          drop_rock rocks.(rock_idx) existing jet_idx js
        in
        go (idx + 1)
          ((rock_idx + 1) % Array.length rocks)
          jet_idx' existing' states'
  in
  let floor = List.init 7 ~f:(fun x -> (x, 0)) |> Set.of_list (module Pos) in
  go 0 0 0 floor (Map.empty (module StateKey))

let part1 (inp : int array) : int = drop_n_rocks 2022 inp

let part2 (inp : int array) : int =
  let ( cycle_len,
        start_height,
        start_rocks,
        cycle_height,
        rock_idx,
        jet_idx,
        existing ) =
    find_cycle_length inp
  in
  let num_rocks = 1000000000000 - start_rocks in
  let num_cycles = num_rocks / cycle_len in
  let remainder = num_rocks % cycle_len in
  let cycle_portion_height = num_cycles * cycle_height in
  let end_portion_height =
    drop_n_rocks remainder inp ~existing:(Some existing) ~rock_idx ~jet_idx
    - calc_height existing
  in
  start_height + cycle_portion_height + end_portion_height

let parsed = read_input_from_stdin |> parse_input

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
