open Base
open Aoc2022.Lib

type links = (string, int) Hashtbl.t
type cave = { name : string; flow_rate : int; neighbours : links }
type map = (string, cave, String.comparator_witness) Map.t
type string_set = (string, String.comparator_witness) Set.t

module StateKey = struct
  module T = struct
    type t = {
      position : string;
      time_remaining : int;
      valves_on : string;
      remaining_players : int;
    }
    [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

type state = (StateKey.t, int) Hashtbl.t

let parse_neighbours (l : string) : string list =
  let rec go (current : string list) (toks : string list) : string list =
    match toks with
    | "valve" :: _ -> current
    | "valves" :: _ -> current
    | neighbour :: rest ->
        go
          (String.strip ~drop:(fun c -> phys_equal c ',') neighbour :: current)
          rest
    | _ -> failwith ""
  in
  l |> String.split ~on:' ' |> List.rev |> go []

let parse_cave (line : string) : cave =
  let name = (line |> String.split ~on:' ' |> Array.of_list).(1) in
  let flow_rate =
    (line |> String.split ~on:'=' |> Array.of_list).(1)
    |> String.split ~on:';' |> List.hd_exn |> Int.of_string
  in
  let neighbours = parse_neighbours line in
  {
    name;
    flow_rate;
    neighbours =
      neighbours
      |> List.map ~f:(fun n -> (n, 1))
      |> Hashtbl.of_alist_exn (module String);
  }

let maxlist (l : int list) : int =
  l |> List.fold ~init:0 ~f:(fun acc x -> if x > acc then x else acc)

(* Replace zero flow caves with longer links between caves
   This doesn't actually fully work in the case of zero flow rate cycles but it
   does make things better :shrug:
*)
let prune_map (m : map) : unit =
  let zero_flow_rate_caves =
    m |> Map.to_alist
    |> List.filter_map ~f:(fun (_, c) ->
           if phys_equal c.flow_rate 0 then Some c else None)
  in
  zero_flow_rate_caves
  |> List.iter ~f:(fun c ->
         let neighbours = (Map.find_exn m c.name).neighbours in
         neighbours
         |> Hashtbl.iteri ~f:(fun ~key:n1 ~data:d1 ->
                (* Adjust this neighbour to link to all of _these_ neighbours *)
                let n1links = (Map.find_exn m n1).neighbours in
                Hashtbl.remove n1links c.name;
                neighbours
                |> Hashtbl.iteri ~f:(fun ~key:n2 ~data:d2 ->
                       if phys_equal n1 n2 then ()
                       else
                         match Hashtbl.find n1links n2 with
                         | Some d12 ->
                             Hashtbl.set n1links ~key:n2
                               ~data:(min d12 (d1 + d2))
                         | None -> Hashtbl.set n1links ~key:n2 ~data:(d1 + d2))));
  ()

let parse_input (s : string) : map =
  let parsed = s |> String.split_lines |> List.map ~f:parse_cave in
  let m =
    parsed
    |> List.map ~f:(fun c -> (c.name, c))
    |> Map.of_alist_exn (module String)
  in
  prune_map m;
  m

let rec calculate_best_flow (position : string) (time_remaining : int)
    (valves_on : string_set) (m : map) (s : state) (remaining_players : int) :
    int =
  if time_remaining <= 1 then
    if remaining_players > 0 then calculate_best_flow "AA" 26 valves_on m s 0
    else 0
  else
    let key : StateKey.t =
      {
        position;
        time_remaining;
        valves_on =
          valves_on |> Set.to_list
          |> List.sort ~compare:Poly.compare
          |> String.concat;
        remaining_players;
      }
    in
    match Hashtbl.find s key with
    | Some x -> x
    | None ->
        let cave = Map.find_exn m position in
        let children_off =
          cave.neighbours |> Hashtbl.to_alist
          |> List.map ~f:(fun (n, d) ->
                 calculate_best_flow n (time_remaining - d) valves_on m s
                   remaining_players)
        in
        let max_children_off = maxlist children_off in
        let result =
          if Set.mem valves_on position || phys_equal cave.flow_rate 0 then
            max_children_off
          else
            (* Turn on *)
            let this_total_flow = cave.flow_rate * (time_remaining - 1) in
            let valves_on' = Set.add valves_on position in
            let children_on =
              cave.neighbours |> Hashtbl.to_alist
              |> List.map ~f:(fun (n, d) ->
                     this_total_flow
                     + calculate_best_flow n
                         (time_remaining - 1 - d)
                         valves_on' m s remaining_players)
            in
            let max_children_on = maxlist children_on in
            let max_on_off = max max_children_off max_children_on in
            max_on_off
        in
        Hashtbl.set s ~key ~data:result;
        result

let part1 (m : map) : int =
  let start_state = Hashtbl.create (module StateKey) in
  calculate_best_flow "AA" 30 (Set.empty (module String)) m start_state 0

let part2 (m : map) : int =
  let start_state = Hashtbl.create (module StateKey) in
  calculate_best_flow "AA" 26 (Set.empty (module String)) m start_state 1

let parsed : map = read_input_from_stdin |> parse_input

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
