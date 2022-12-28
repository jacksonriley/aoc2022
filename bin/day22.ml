open Base
open Aoc2022.Lib

type spot = Wall | Open
type direction = Left | Right
type instruction = Go of int | Turn of direction

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

module PosPair = struct
  module T = struct
    type t = Pos.t * Pos.t [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

type map = (Pos.t, spot, Pos.comparator_witness) Map.t
type adjacencies = (PosPair.t, PosPair.t) Hashtbl.t

let parse_instructions (l : string) : instruction list =
  let rec go is so_far =
    match is with
    | [] -> so_far |> List.rev
    | 'R' :: tl -> go tl (Turn Right :: so_far)
    | 'L' :: tl -> go tl (Turn Left :: so_far)
    | is' ->
        let digits, rest = is' |> List.split_while ~f:Char.is_digit in
        go rest (Go (digits |> String.of_char_list |> Int.of_string) :: so_far)
  in

  go (explode_string l) []

let parse_map (s : string) : map =
  s |> String.split_lines
  |> List.mapi ~f:(fun r l ->
         l |> explode_string
         |> List.filter_mapi ~f:(fun col ch ->
                match ch with
                | ' ' -> None
                | '.' -> Some ((r + 1, col + 1), Open)
                | '#' -> Some ((r + 1, col + 1), Wall)
                | _ -> failwith "Unexpected char"))
  |> List.concat
  |> Map.of_alist_exn (module Pos)

let parse_input (s : string) : map * instruction list =
  match s |> Str.split (Str.regexp "\n\n") with
  | [ m; is ] -> (parse_map m, parse_instructions is)
  | _ -> failwith "Unexpected input format"

let rotate (facing : Pos.t) (dir : direction) : Pos.t =
  let fr, fc = facing in
  let perms = [| (1, 0); (0, -1); (-1, 0); (0, 1) |] in
  let i, _ =
    Array.findi_exn
      ~f:(fun _ (pr, pc) -> phys_equal fr pr && phys_equal fc pc)
      perms
  in
  match dir with Right -> perms.((i + 1) % 4) | Left -> perms.((i - 1) % 4)

let reverse ((dr, dc) : Pos.t) : Pos.t = (-1 * dr, -1 * dc)

let find_opposite (s : Pos.t) (facing : Pos.t) (m : map) : spot * Pos.t * Pos.t
    =
  let dr, dc = reverse facing in
  let rec go ((r, c) as s) =
    match Map.find m (r + dr, c + dc) with
    | Some _ -> go (r + dr, c + dc)
    | None -> (Map.find_exn m s, s, facing)
  in
  go s

let find_start_c (m : map) : int =
  m |> Map.keys
  |> List.filter_map ~f:(fun (r, c) -> if phys_equal r 1 then Some c else None)
  |> List.reduce_exn ~f:(fun acc x -> if acc < x then acc else x)

let find_inside_corners (m : map) : (Pos.t * Pos.t * Pos.t) list =
  let start_c = find_start_c m in
  let rec go ((r, c) as pos) ((dr, dc) as dir) ((out_r, out_c) as out_dir)
      so_far =
    if phys_equal r 1 && phys_equal c start_c then so_far
    else
      let diagonal = (r + dr + out_r, c + dc + out_c) in
      let straight_ahead = (r + dr, c + dc) in
      match Map.find m diagonal with
      | Some _ ->
          go diagonal out_dir (reverse dir)
            ((straight_ahead, reverse dir, out_dir) :: so_far)
      | None -> (
          match Map.find m straight_ahead with
          | Some _ -> go straight_ahead dir out_dir so_far
          | None -> go pos (reverse out_dir) dir so_far)
  in
  let start = (1, start_c + 1) in
  let start_dir = (0, 1) in
  let start_out_dir = (-1, 0) in
  go start start_dir start_out_dir []

let build_cube_adjacencies (m : map) : adjacencies =
  (* Idea is to start from inner corners and walk with a pair along the edge. *)
  (* This terminates when both of the walkers turn a corner at the same time. *)
  let result = Hashtbl.create (module PosPair) in
  let rec go (r1, c1) ((dr1, dc1) as dir1) ((or1, oc1) as out1) (r2, c2)
      ((dr2, dc2) as dir2) ((or2, oc2) as out2) =
    Hashtbl.add_exn result
      ~key:((r1, c1), (or1, oc1))
      ~data:((r2, c2), reverse (or2, oc2));
    Hashtbl.add_exn result
      ~key:((r2, c2), (or2, oc2))
      ~data:((r1, c1), reverse (or1, oc1));
    let diag1 = (r1 + dr1 + or1, c1 + dc1 + oc1) in
    let diag2 = (r2 + dr2 + or2, c2 + dc2 + oc2) in
    let straight1 = (r1 + dr1, c1 + dc1) in
    let straight2 = (r2 + dr2, c2 + dc2) in
    let turn1 =
      match (Map.find m diag1, Map.find m straight1) with
      | None, Some _ -> false
      | _ -> true
    in
    let turn2 =
      match (Map.find m diag2, Map.find m straight2) with
      | None, Some _ -> false
      | _ -> true
    in

    match (turn1, turn2) with
    | true, true -> ()
    | false, false ->
        go (r1 + dr1, c1 + dc1) dir1 out1 (r2 + dr2, c2 + dc2) dir2 out2
    | true, false -> (
        match Map.find m diag1 with
        | None -> go (r1, c1) (reverse out1) dir1 (r2 + dr2, c2 + dc2) dir2 out2
        | Some _ -> failwith "Should be impossible to turn inside here")
    | false, true -> (
        match Map.find m diag2 with
        | None -> go (r1 + dr1, c1 + dc1) dir1 out1 (r2, c2) (reverse out2) dir2
        | Some _ -> failwith "Should be impossible to turn inside here")
  in
  let inner_corners = find_inside_corners m in
  inner_corners
  |> List.iter ~f:(fun ((r, c), ((dr1, dc1) as dir1), ((dr2, dc2) as dir2)) ->
         go (r + dr1, c + dc1) dir1 dir2 (r + dr2, c + dc2) dir2 dir1);
  result

let rec travel ((r, c) as s : Pos.t) ((dr, dc) as facing : Pos.t) (m : map)
    (is : instruction list) go_over_edge : Pos.t * Pos.t =
  match is with
  | [] -> (s, facing)
  | i :: tl -> (
      match i with
      | Turn dir -> travel s (rotate facing dir) m tl go_over_edge
      | Go 0 -> travel s facing m tl go_over_edge
      | Go num -> (
          match Map.find m (r + dr, c + dc) with
          | Some Wall -> travel s facing m tl go_over_edge
          | Some Open ->
              travel (r + dr, c + dc) facing m (Go (num - 1) :: tl) go_over_edge
          | None -> (
              match go_over_edge s facing m with
              | Wall, _, _ -> travel s facing m tl go_over_edge
              | Open, teleported_pos, teleported_facing ->
                  travel teleported_pos teleported_facing m
                    (Go (num - 1) :: tl)
                    go_over_edge)))

let calc_score ((r, c) : Pos.t) (facing : Pos.t) : int =
  let facing_score =
    match facing with
    | 0, 1 -> 0
    | 1, 0 -> 1
    | 0, -1 -> 2
    | -1, 0 -> 3
    | _ -> failwith "Unreachable"
  in
  (1000 * r) + (4 * c) + facing_score

let part1 ((m, is) : map * instruction list) : int =
  let start_c = find_start_c m in
  let end_pos, end_facing = travel (1, start_c) (0, 1) m is find_opposite in
  calc_score end_pos end_facing

let part2 ((m, is) : map * instruction list) : int =
  let start_c = find_start_c m in
  let adjacencies = build_cube_adjacencies m in
  let go_over_edge (s : Pos.t) (facing : Pos.t) (m : map) : spot * Pos.t * Pos.t
      =
    let teleported_pos, teleported_facing =
      Hashtbl.find_exn adjacencies (s, facing)
    in
    (Map.find_exn m teleported_pos, teleported_pos, teleported_facing)
  in
  let end_pos, end_facing = travel (1, start_c) (0, 1) m is go_over_edge in
  calc_score end_pos end_facing

let parsed = read_input_from_stdin |> parse_input

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
