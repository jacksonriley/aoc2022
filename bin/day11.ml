open Base
open Aoc2022.Lib

type monkey = {
  number : int;
  mutable items : int array;
  operation : int -> int;
  test_divisor : int;
  true_monkey : int;
  false_monkey : int;
  mutable num_items_inspected : int;
}

type monkey_map = (int, monkey, Int.comparator_witness) Map.t

let get_last_int (s : string) : int =
  s |> String.split ~on:' ' |> List.last_exn |> Int.of_string

let parse_operation (s : string) : int -> int =
  match List.drop (s |> String.split ~on:' ') 6 with
  | [ "+"; "old" ] -> fun o -> o + o
  | [ "*"; "old" ] -> fun o -> o * o
  | [ "+"; num ] -> fun o -> o + Int.of_string num
  | [ "*"; num ] -> fun o -> o * Int.of_string num
  | _other -> failwith @@ Printf.sprintf "Unexpected operation: %s" s

let parse_monkey (s : string) : monkey =
  let lines = String.split_lines s in
  match lines with
  | [ num_l; items_l; op_l; test_l; true_l; false_l ] ->
      let number =
        num_l |> explode_string
        |> List.filter ~f:Char.is_digit
        |> String.of_char_list |> Int.of_string
      in
      let items =
        List.drop (String.split items_l ~on:' ') 4
        |> List.map ~f:(String.strip ~drop:(fun c -> phys_equal c ','))
        |> List.map ~f:Int.of_string |> Array.of_list
      in
      let operation = parse_operation op_l in
      let test_divisor = get_last_int test_l in
      let true_monkey = get_last_int true_l in
      let false_monkey = get_last_int false_l in
      {
        number;
        items;
        operation;
        test_divisor;
        true_monkey;
        false_monkey;
        num_items_inspected = 0;
      }
  | _other -> failwith @@ Printf.sprintf "Unexpected monkey: %s" s

let input = read_input_from_stdin

let parse_input (inp : string) : monkey_map =
  let monkeys =
    inp
    |> Str.split (Str.regexp "\n\n")
    |> List.map ~f:parse_monkey
    |> List.map ~f:(fun m -> (m.number, m))
  in
  Map.of_alist_exn (module Int) monkeys

let run_turn (mmap : monkey_map) (monkey_num : int) (post_op_adj : int -> int) :
    unit =
  let monkey = Map.find_exn mmap monkey_num in
  Array.iter monkey.items ~f:(fun item ->
      (* Inspect *)
      monkey.num_items_inspected <- monkey.num_items_inspected + 1;
      (* Do operation *)
      let operated = monkey.operation item in
      (* Adjust - either //3 or %lcd *)
      let adjusted = post_op_adj operated in
      (* Test *)
      let target_monkey_num =
        if phys_equal 0 (adjusted % monkey.test_divisor) then monkey.true_monkey
        else monkey.false_monkey
      in
      (* Send *)
      let target_monkey = Map.find_exn mmap target_monkey_num in
      let old_items = target_monkey.items in
      target_monkey.items <- Array.append old_items [| adjusted |]);
  monkey.items <- [||]

let run_round (num_monkeys : int) (mmap : monkey_map) (post_op_adj : int -> int)
    : unit =
  for i = 0 to num_monkeys - 1 do
    run_turn mmap i post_op_adj
  done

let calculate_monkey_business (mmap : monkey_map) : int =
  match
    Map.to_alist mmap
    |> List.map ~f:(fun (_, m) -> m.num_items_inspected)
    |> List.sort ~compare:Poly.compare
    |> List.rev
  with
  | n1 :: n2 :: _ -> n1 * n2
  | _other -> failwith "Not enough monkeys"

let do_part (parsed : monkey_map) (num_rounds : int) (post_op_adj : int -> int)
    : int =
  let num_monkeys = Map.length parsed in
  List.init ~f:(fun i -> i) num_rounds
  |> List.iter ~f:(fun _ -> run_round num_monkeys parsed post_op_adj);
  calculate_monkey_business parsed

let part1 (inp : string) : int =
  let parsed = parse_input inp in
  do_part parsed 20 (fun x -> x / 3)

let part2 (inp : string) : int =
  let parsed = parse_input inp in
  (* Point here is to keep the worry levels small by taking the modulo after
     the monkey operation with respect to the lowest common multiple of all of
     the test divisors.
     This doesn't affect the result as the only thing we care about is what
     items get thrown to which monkeys - i.e. what the number is modulo the
     test divisor, and
      - (a mod bn) mod n = a mod n.
      - (c + (a mod bn)) mod n = (a + c) mod n.
      - (c * (a mod bn)) mod n = (a * c) mod n.
  *)
  (* The test divisors are all prime so the lcm is just their product *)
  let lcm =
    parsed |> Map.to_alist
    |> List.map ~f:(fun (_, m) -> m.test_divisor)
    |> List.fold ~init:1 ~f:(fun acc x -> acc * x)
  in
  do_part parsed 10000 (fun x -> x % lcm)

let () =
  input |> part1 |> Int.to_string |> ( ^ ) "Part 1: " |> Stdio.print_endline

let () =
  input |> part2 |> Int.to_string |> ( ^ ) "Part 2: " |> Stdio.print_endline
