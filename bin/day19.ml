open Base
open Aoc2022.Lib

module Mat = struct
  module T = struct
    type t = Ore | Clay | Obsidian | Geode [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

type mat = Mat.t
type cost = (int * mat) list
type robot_costs = (mat, cost, Mat.comparator_witness) Map.t
type blueprint = { number : int; costs : robot_costs }

module StateKey = struct
  module T = struct
    type t = {
      time_left : int;
      ore : int;
      ore_robots : int;
      clay : int;
      clay_robots : int;
      obsidian : int;
      obsidian_robots : int;
      geode : int;
      geode_robots : int;
    }
    [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

let parse_bluprint (l : string) : blueprint =
  match
    l |> String.split ~on:' '
    |> List.map ~f:(String.strip ~drop:(fun c -> phys_equal ':' c))
    |> List.filter_map ~f:(fun tok ->
           try Some (Int.of_string tok) with _ -> None)
  with
  | [
   number;
   ore_ore;
   clay_ore;
   obsidian_ore;
   obsidian_clay;
   geode_ore;
   geode_obsidian;
  ] ->
      let costs =
        [
          (Ore, [ (ore_ore, Mat.Ore) ]);
          (Clay, [ (clay_ore, Ore) ]);
          (Obsidian, [ (obsidian_ore, Ore); (obsidian_clay, Clay) ]);
          (Geode, [ (geode_ore, Ore); (geode_obsidian, Obsidian) ]);
        ]
        |> Map.of_alist_exn (module Mat)
      in
      { number; costs }
  | _ -> failwith @@ "Unexpected number of ints in line: " ^ l

let parse_input (s : string) : blueprint list =
  s |> String.split_lines |> List.map ~f:parse_bluprint

let can_make_robot (b : blueprint) (state : StateKey.t) (m : mat) : bool =
  Map.find_exn b.costs m
  |> List.map ~f:(fun (num, m') ->
         match m' with
         | Mat.Ore -> num <= state.ore
         | Clay -> num <= state.clay
         | Obsidian -> num <= state.obsidian
         | _ -> failwith "Nothing should require geodes to be made")
  |> List.fold ~init:true ~f:( && )

let update_state (b : blueprint) (s : StateKey.t) (robot : mat) : StateKey.t =
  let get_new_mat m =
    Map.find_exn b.costs robot
    |> List.map ~f:(fun (c, m') -> if phys_equal m m' then c else 0)
    |> List.reduce_exn ~f:( + )
  in
  let ore_cost = get_new_mat Mat.Ore in
  let clay_cost = get_new_mat Mat.Clay in
  let obsidian_cost = get_new_mat Mat.Obsidian in
  let new_ore_robot = if phys_equal robot Mat.Ore then 1 else 0 in
  let new_clay_robot = if phys_equal robot Mat.Clay then 1 else 0 in
  let new_obsidian_robot = if phys_equal robot Mat.Obsidian then 1 else 0 in
  let new_geode_robot = if phys_equal robot Mat.Geode then 1 else 0 in
  {
    time_left = s.time_left;
    ore = s.ore - ore_cost;
    ore_robots = s.ore_robots + new_ore_robot;
    clay = s.clay - clay_cost;
    clay_robots = s.clay_robots + new_clay_robot;
    obsidian = s.obsidian - obsidian_cost;
    obsidian_robots = s.obsidian_robots + new_obsidian_robot;
    geode = s.geode;
    geode_robots = s.geode_robots + new_geode_robot;
  }

let maxlist (l : int list) : int =
  l |> List.fold ~init:0 ~f:(fun acc x -> if x > acc then x else acc)

let get_max_per_minute (b : blueprint) (m : mat) : int =
  b.costs |> Map.data |> List.concat
  |> List.filter_map ~f:(fun (c, m') ->
         if phys_equal m m' then Some c else None)
  |> maxlist

let get_max_need (b : blueprint) (s : StateKey.t) (m : mat) : int =
  let max_per_minute = get_max_per_minute b m in
  s.time_left * max_per_minute

let prune_state (b : blueprint) (s : StateKey.t) : StateKey.t =
  (* No point having more resources than you can possibly spend *)
  {
    s with
    ore = min s.ore (get_max_need b s Mat.Ore);
    clay = min s.clay (get_max_need b s Mat.Clay);
    obsidian = min s.obsidian (get_max_need b s Mat.Obsidian);
  }

let useful_to_make_robot (b : blueprint) (s : StateKey.t) (robot : mat) =
  (* No point making robots of a particular type if you're already producing more per minute than you can consume *)
  let max_per_minute = get_max_per_minute b robot in
  match robot with
  | Mat.Ore -> max_per_minute > s.ore_robots
  | Mat.Clay -> max_per_minute > s.clay_robots
  | Mat.Obsidian -> max_per_minute > s.obsidian_robots
  | Mat.Geode -> true

let score_blueprint (b : blueprint) (start_minutes : int) : int =
  let states = Hashtbl.create (module StateKey) in
  let rec go (state : StateKey.t) =
    match Hashtbl.find states state with
    | Some x -> x
    | None ->
        if phys_equal state.time_left 0 then state.geode
        else
          let state_after_mining : StateKey.t =
            {
              time_left = state.time_left - 1;
              ore = state.ore + state.ore_robots;
              ore_robots = state.ore_robots;
              clay = state.clay + state.clay_robots;
              clay_robots = state.clay_robots;
              obsidian = state.obsidian + state.obsidian_robots;
              obsidian_robots = state.obsidian_robots;
              geode = state.geode + state.geode_robots;
              geode_robots = state.geode_robots;
            }
          in
          let robot_to_make =
            [ None; Some Mat.Ore; Some Clay; Some Obsidian; Some Geode ]
            |> List.filter ~f:(fun r ->
                   match r with
                   | Some r' ->
                       can_make_robot b state r'
                       && useful_to_make_robot b state r'
                   | None -> true)
          in
          let children =
            robot_to_make
            |> List.map ~f:(fun r ->
                   let state_after_new_robot =
                     match r with
                     | None -> state_after_mining
                     | Some r' -> update_state b state_after_mining r'
                   in
                   prune_state b state_after_new_robot |> go)
          in
          let result = children |> maxlist in
          Hashtbl.set states ~key:state ~data:result;
          result
  in
  go
    {
      time_left = start_minutes;
      ore = 0;
      ore_robots = 1;
      clay = 0;
      clay_robots = 0;
      obsidian = 0;
      obsidian_robots = 0;
      geode = 0;
      geode_robots = 0;
    }

let parsed = read_input_from_stdin |> parse_input

let part1 (inp : blueprint list) : int =
  inp
  |> List.map ~f:(fun b -> b.number * score_blueprint b 24)
  |> List.reduce_exn ~f:( + )

let part2 (inp : blueprint list) : int =
  List.take inp 3
  |> List.map ~f:(fun b -> score_blueprint b 32)
  |> List.reduce_exn ~f:( * )

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
