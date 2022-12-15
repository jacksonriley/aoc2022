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
type lines = pos list
type spot = Sand | Air | Rock
type grid = spot array array

let parse_line (l : string) : lines =
  l
  |> Str.split (Str.regexp " -> ")
  |> List.map ~f:(fun p ->
         match String.split ~on:',' p with
         | [ x; y ] -> (Int.of_string x, Int.of_string y)
         | _ -> failwith @@ "Unexpected position: " ^ p)

let unwrap (x : 'a option) : 'a =
  match x with Some inner -> inner | None -> failwith "Couldn't unwrap None"

(* Create the grid such that in part 2 the floor will be the last row *)
let parse_input (s : string) : grid =
  let all_lines = s |> String.split_lines |> List.map ~f:parse_line in
  let _, max_y =
    all_lines |> List.concat
    |> List.max_elt ~compare:(fun (_, y1) (_, y2) -> Poly.compare y1 y2)
    |> unwrap
  in
  let max_x, _ =
    all_lines |> List.concat
    |> List.max_elt ~compare:(fun (x1, _) (x2, _) -> Poly.compare x1 x2)
    |> unwrap
  in
  let g =
    Array.init
      (max_y + 1 + 2)
      ~f:(fun _ -> Array.init (max_x + 1000) ~f:(fun _ -> Air))
  in
  all_lines
  |> List.iter ~f:(fun ls ->
         List.zip_exn (List.drop_last_exn ls) (List.tl_exn ls)
         |> List.iter ~f:(fun ((x1, y1), (x2, y2)) ->
                match phys_equal x1 x2 with
                | true ->
                    for y = min y1 y2 to max y1 y2 do
                      g.(y).(x1) <- Rock
                    done
                | false ->
                    for x = min x1 x2 to max x1 x2 do
                      g.(y1).(x) <- Rock
                    done));
  g

(*
   If the sand comes to rest at x, y, mutate g accordingly and return true
   If the sand comes to rest somewhere below, return true.
   Otherwise, return false.
   *)
let rec sand_comes_to_rest ((x, y) : pos) (g : grid) : bool =
  match g.(y).(x) with
  | Sand | Rock -> false
  | Air -> (
      try
        match (g.(y + 1).(x - 1), g.(y + 1).(x), g.(y + 1).(x + 1)) with
        | _, Air, _ -> sand_comes_to_rest (x, y + 1) g
        | Air, _, _ -> sand_comes_to_rest (x - 1, y + 1) g
        | _, _, Air -> sand_comes_to_rest (x + 1, y + 1) g
        | _ ->
            g.(y).(x) <- Sand;
            true
      with _ -> false)

let input = read_input_from_stdin

let do_part (inp : string) (set_floor : bool) : int =
  let g = inp |> parse_input in
  if set_floor then
    let max_y = Array.length g - 1 in
    let x_dim = Array.length g.(0) in
    g.(max_y) <- Array.init x_dim ~f:(fun _ -> Rock)
  else ();
  let num_came_to_rest = ref 0 in
  while sand_comes_to_rest (500, 0) g do
    num_came_to_rest := !num_came_to_rest + 1
  done;
  !num_came_to_rest

let part1 (inp : string) : int = do_part inp false
let part2 (inp : string) : int = do_part inp true

let () =
  input |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  input |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
