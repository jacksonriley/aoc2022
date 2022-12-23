open Base
open Aoc2022.Lib

module Pos = struct
  module T = struct
    type t = int * int * int [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

type pos = Pos.t
type pos_set = (Pos.t, Pos.comparator_witness) Set.t

module Face = struct
  module T = struct
    type t = Pos.t * Pos.t [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

let parse_input (s : string) : pos list =
  s |> String.split_lines
  |> List.map ~f:(fun l ->
         match l |> String.split ~on:',' |> List.map ~f:Int.of_string with
         | [ x; y; z ] -> (x, y, z)
         | _ -> failwith @@ "Unexpected line: " ^ l)

let neighbours ((x, y, z) : pos) : pos list =
  [
    (x + 1, y, z);
    (x - 1, y, z);
    (x, y + 1, z);
    (x, y - 1, z);
    (x, y, z + 1);
    (x, y, z - 1);
  ]

let faces (p : pos) : Face.t list =
  neighbours p |> List.map ~f:(fun n -> [ (n, p); (p, n) ]) |> List.concat

let parsed = read_input_from_stdin |> parse_input

let part1 (inp : pos list) : int =
  (inp
  |> List.map ~f:(fun p -> faces p)
  |> List.concat
  |> List.map ~f:(fun f -> (f, 1))
  |> Map.of_alist_reduce (module Face) ~f:(fun v1 v2 -> v1 + v2)
  |> Map.to_alist
  |> List.filter ~f:(fun (_, count) -> phys_equal count 1)
  |> List.length)
  / 2

let extreme (f : int -> int -> bool) (xs : int list) : int =
  match xs |> List.reduce ~f:(fun acc x -> if f acc x then acc else x) with
  | Some x -> x
  | None -> failwith "empty list!"

let find_bounding_cube (ps : pos list) : int * int * int * int * int * int =
  ( (ps |> List.map ~f:(fun (x, _, _) -> x) |> extreme ( > )) + 1,
    (ps |> List.map ~f:(fun (x, _, _) -> x) |> extreme ( < )) - 1,
    (ps |> List.map ~f:(fun (_, y, _) -> y) |> extreme ( > )) + 1,
    (ps |> List.map ~f:(fun (_, y, _) -> y) |> extreme ( < )) - 1,
    (ps |> List.map ~f:(fun (_, _, z) -> z) |> extreme ( > )) + 1,
    (ps |> List.map ~f:(fun (_, _, z) -> z) |> extreme ( < )) - 1 )

let fill (start : pos) (can_fill : pos -> bool) : pos_set =
  let rec go frontier seen =
    let new_frontier =
      frontier |> Set.to_list
      |> List.map ~f:(fun p ->
             p |> neighbours
             |> List.filter ~f:(fun n -> can_fill n && not (Set.mem seen n)))
      |> List.concat
      |> Set.of_list (module Pos)
    in
    let new_seen = Set.union seen new_frontier in
    if phys_equal (Set.length new_frontier) 0 then seen
    else go new_frontier new_seen
  in
  go (Set.of_list (module Pos) [ start ]) (Set.empty (module Pos))

let make_cube (max_x, min_x, max_y, min_y, max_z, min_z) : pos list =
  let xs = List.range min_x (max_x + 1) in
  let ys = List.range min_y (max_y + 1) in
  let zs = List.range min_z (max_z + 1) in
  xs
  |> List.map ~f:(fun x ->
         ys |> List.map ~f:(fun y -> zs |> List.map ~f:(fun z -> (x, y, z))))
  |> List.concat |> List.concat

let find_inner (inp : pos list) : pos list =
  let ps = inp |> Set.of_list (module Pos) in
  let ((max_x, min_x, max_y, min_y, max_z, min_z) as bs) =
    find_bounding_cube inp
  in
  let all_ps = make_cube bs |> Set.of_list (module Pos) in
  let outer =
    fill (max_x, max_y, max_z) (fun ((x, y, z) as p) ->
        x <= max_x && x >= min_x && y <= max_y && y >= min_y && z <= max_z
        && z >= min_z
        && not (Set.mem ps p))
  in
  let not_inner = Set.union ps outer in
  Set.diff all_ps not_inner |> Set.to_list

let part2 (inp : pos list) : int =
  let all_sa = part1 inp in
  let inner_sa = inp |> find_inner |> part1 in
  all_sa - inner_sa

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
