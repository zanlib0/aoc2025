open Aoc2025.Utils

let example_input = {|123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  |}

type op = Add | Mul

let split_on_whitespace str = Str.split (Str.regexp " +") str

let pop arr =
  let len = Array.length arr in
  if len = 0 then failwith "empty array"
  else
    (Array.sub arr 0 (len - 1), arr.(len - 1))

let op_of_string str = match str with
  | "+" -> Add
  | "*" -> Mul
  | _ -> failwith "wrong op"

let prep input =
  let matrix = input
  |> String.trim
  |> split_lines
  |> Array.of_list
  |> Array.map (String.trim >> split_on_whitespace >> Array.of_list) in
  let (nums_str, ops_str) = pop matrix in
  let ops = Array.map op_of_string ops_str in
  let nums = mapi_2d_array (fun _ _ cell -> int_of_string cell) nums_str in
  (nums, ops)

let get_col col = Array.map (fun row -> row.(col))

let solve input =
  let (nums, ops) = prep input in
  Array.mapi (fun col op -> 
    get_col col nums
    |> Array.fold_left (if op = Add then (+) else ( * )) (if op = Add then 0 else 1)
  ) ops
  |> Array.fold_left (+) 0

let () =
  let input = read_input 6 in
  Printf.printf "Example 1: %d\n" (solve example_input);
  Printf.printf "Part 1: %d\n" (solve input);
