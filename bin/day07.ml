open Base
open Aoc2022.Lib

type directory = {
  mutable size : int;
  mutable contents : node list;
  parent : directory option;
  name : string;
}

and node = Directory of directory | File of int

let parse_ls_line (parent : directory) (s : string) : node =
  match String.split ~on:' ' s with
  | [ "dir"; name ] ->
      Directory
        {
          size = 0;
          contents = [];
          parent = Some parent;
          name = String.strip name;
        }
  | [ filesize; _filename ] -> File (Int.of_string filesize)
  | other -> other |> String.concat ~sep:" " |> failwith

(* Takes the current directory, and either mutates it if the command was ls, or
   returns the new directory if the command was cd *)
let parse_command (s : string) (current : directory) : directory =
  let lines = String.split ~on:'\n' s |> List.map ~f:String.strip in
  match List.hd_exn lines |> String.split ~on:' ' with
  | [ "cd"; destination_dir ] -> (
      match
        List.find current.contents ~f:(fun n ->
            match n with
            | Directory d -> String.equal d.name destination_dir
            | File _ -> false)
      with
      | Some n -> (
          match n with
          | Directory d -> d
          | File _ ->
              failwith "Not possible - the predicate returns false for files")
      | None ->
          if String.equal destination_dir ".." then
            match current.parent with
            | Some p -> p
            | None -> failwith "Can't cd above root"
          else failwith @@ "Couldn't cd to " ^ destination_dir)
  | [ "ls" ] ->
      let contents = List.map (List.tl_exn lines) ~f:(parse_ls_line current) in
      current.contents <- contents;
      current
  | other ->
      failwith @@ "Unexpected command"
      ^ String.concat ~sep:" " other
      ^ "String was: " ^ s

let parse_input (s : string) : directory =
  let commands =
    String.split ~on:'$' s
    |> List.tl_exn (* Remove the empty string before the first $ *)
    |> List.tl_exn (* Remove the cd / *) |> List.map ~f:String.strip
  in
  let root = { size = 0; contents = []; parent = None; name = "<root>" } in
  let _ =
    List.fold ~init:root
      ~f:(fun current command -> parse_command command current)
      commands
  in
  root

let rec compute_size (n : node) : int =
  match n with
  | File size -> size
  | Directory d ->
      let total_size =
        d.contents |> List.map ~f:compute_size |> List.fold ~init:0 ~f:( + )
      in
      d.size <- total_size;
      total_size

let parsed = read_input_from_stdin |> parse_input

(* let _ = compute_size (Directory parsed); *)

let rec sum_smallish (n : node) : int =
  match n with
  | File _ -> 0
  | Directory d ->
      let self_component = if d.size <= 100000 then d.size else 0 in
      self_component
      + (d.contents |> List.map ~f:sum_smallish |> List.fold ~init:0 ~f:( + ))

let rec get_directory_sizes (d : directory) : int list =
  let to_dir n = match n with Directory d -> Some d | File _ -> None in
  let below =
    List.filter_map d.contents ~f:to_dir
    |> List.map ~f:get_directory_sizes
    |> List.concat
  in
  d.size :: below

let rec find_smallest_above_limit (n : int) (l : int list) : int =
  let sorted = List.sort l ~compare:(fun a b -> if a > b then 1 else -1) in
  match sorted with
  | hd :: tl -> if hd >= n then hd else find_smallest_above_limit n tl
  | [] -> failwith "No directories were big enough to free up space"

let part1 d =
  let _ = compute_size (Directory d) in
  sum_smallish (Directory d)

let part2 d =
  let free_space = 70000000 - d.size in
  let required_space = 30000000 in
  let required_deletion = required_space - free_space in
  let all_dir_sizes = get_directory_sizes d in
  find_smallest_above_limit required_deletion all_dir_sizes

let () =
  parsed |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  parsed |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
