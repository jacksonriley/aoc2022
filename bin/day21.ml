open Base
open Aoc2022.Lib

type number = Just of int | Op of string * string * string
type monkey = { name : string; shout : number }
type solve_res = (string, int) Hashtbl.t
type dependents_type = (string, string list) Hashtbl.t
type mmap = (string, monkey, String.comparator_witness) Map.t
type string_set = (string, String.comparator_witness) Set.t
type branch = Left of int | Right of int

let parse_monkey (l : string) : monkey =
  match l |> Str.split (Str.regexp ": ") with
  | [ name; shout_line ] -> (
      match shout_line |> String.split ~on:' ' with
      | [ m1; op; m2 ] -> { name; shout = Op (m1, m2, op) }
      | [ value ] -> { name; shout = Just (Int.of_string value) }
      | _ -> failwith @@ "Unexpected shout line: " ^ shout_line)
  | _ -> failwith @@ "Unexpected monkey line: " ^ l

let parse_input (s : string) : mmap =
  s |> String.split_lines |> List.map ~f:parse_monkey
  |> List.map ~f:(fun m -> (m.name, m))
  |> Map.of_alist_exn (module String)

let calculate_dependents (ms : mmap) : dependents_type =
  let dependents =
    ms |> Map.data
    |> List.map ~f:(fun m -> (m.name, []))
    |> Hashtbl.of_alist_exn (module String)
  in
  ms |> Map.data
  |> List.iter ~f:(fun m ->
         match m.shout with
         | Just _ -> ()
         | Op (m1, m2, _) ->
             Hashtbl.set dependents ~key:m1
               ~data:(List.append [ m.name ] (Hashtbl.find_exn dependents m1));
             Hashtbl.set dependents ~key:m2
               ~data:(List.append [ m.name ] (Hashtbl.find_exn dependents m2));
             ());
  dependents

let do_op (op : string) (v1 : int) (v2 : int) : int =
  match op with
  | "*" -> v1 * v2
  | "/" -> v1 / v2
  | "+" -> v1 + v2
  | "-" -> v1 - v2
  | _ -> failwith @@ "Invalid operation: " ^ op

let solve (ms : mmap) : solve_res =
  let completed =
    ms |> Map.data
    |> List.filter_map ~f:(fun m ->
           match m.shout with Just v -> Some (m.name, v) | Op _ -> None)
    |> Hashtbl.of_alist_exn (module String)
  in
  (* Map of monkey to monkeys which depend on it *)
  let dependents = calculate_dependents ms in
  let frontier =
    completed |> Hashtbl.keys
    |> List.map ~f:(fun name -> Hashtbl.find_exn dependents name)
    |> List.concat
    |> Set.of_list (module String)
  in
  let rec go f =
    let f' =
      f |> Set.to_list
      |> List.filter_map ~f:(fun name ->
             let m = Map.find_exn ms name in
             match m.shout with
             | Just _ -> failwith "Didn't expect a Just monkey in the frontier"
             | Op (m1, m2, op) -> (
                 match
                   (Hashtbl.find completed m1, Hashtbl.find completed m2)
                 with
                 | Some v1, Some v2 ->
                     Hashtbl.set completed ~key:name ~data:(do_op op v1 v2);
                     Some (Hashtbl.find_exn dependents name)
                 | _ -> None))
      |> List.concat
      |> Set.of_list (module String)
    in
    let incomplete_frontier =
      Set.diff f' (completed |> Hashtbl.keys |> Set.of_list (module String))
    in
    if phys_equal (Set.length incomplete_frontier) 0 then completed
    else go incomplete_frontier
  in
  go frontier

let find_humn_path (ms : mmap) : string_set =
  let dependents = calculate_dependents ms in
  let rec go name so_far =
    if String.equal name "root" then so_far
    else
      let next =
        match Hashtbl.find_exn dependents name with
        | [ n ] -> n
        | _ -> failwith "Expected a single parent"
      in
      go next (name :: so_far)
  in
  go "humn" [] |> Set.of_list (module String)

let invert_op (op : string) : int -> int -> int =
  match op with
  | "*" -> ( / )
  | "/" -> ( * )
  | "+" -> ( - )
  | "-" -> ( + )
  | _ -> failwith @@ "Invalid operation: " ^ op

let infer_other (we_know : branch) (op : string) (should_equal : int) : int =
  match we_know with
  | Right m2 ->
      (* m1 + m2 should equal v => m1 = v - m2 *)
      (* m1 - m2 should equal v => m1 = v + m2 *)
      (* m1 * m2 should equal v => m1 = v / m2 *)
      (* m1 / m2 should equal v => m1 = v * m2 *)
      (invert_op op) should_equal m2
  | Left m1 -> (
      (* m1 + m2 should equal v => m2 = v - m1 *)
      (* m1 - m2 should equal v => m2 = m1 - v *)
      (* m1 * m2 should equal v => m2 = v / m1 *)
      (* m1 / m2 should equal v => m2 = m1 / v *)
      match op with
      | "+" -> should_equal - m1
      | "-" -> m1 - should_equal
      | "*" -> should_equal / m1
      | "/" -> m1 / should_equal
      | _ -> failwith @@ "Invalid operation: " ^ op)

let part1 (inp : mmap) : int =
  let completed = solve inp in
  Hashtbl.find_exn completed "root"

let part2 (inp : mmap) : int =
  let completed = solve inp in
  let humn_path = find_humn_path inp in
  let r_m = Map.find_exn inp "root" in
  let m1, m2 =
    match r_m.shout with
    | Just _ -> failwith "root is not a Just"
    | Op (m1, m2, _) -> (m1, m2)
  in
  let should_equal, name_unknown =
    if Set.mem humn_path m1 then (Hashtbl.find_exn completed m2, m1)
    else (Hashtbl.find_exn completed m1, m2)
  in
  let rec go parent must_be =
    if String.equal "humn" parent then must_be
    else
      let m = Map.find_exn inp parent in
      let m1, m2, op =
        match m.shout with
        | Just _ -> failwith "Not humn, so can't be a Just"
        | Op (m1, m2, op) -> (m1, m2, op)
      in
      let we_know =
        if Set.mem humn_path m1 then Right (Hashtbl.find_exn completed m2)
        else Left (Hashtbl.find_exn completed m1)
      in
      let should_equal = infer_other we_know op must_be in
      let name_unknown = match we_know with Left _ -> m2 | Right _ -> m1 in
      go name_unknown should_equal
  in

  go name_unknown should_equal

let parsed = read_input_from_stdin |> parse_input

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
