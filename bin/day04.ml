open Aoc2025.Utils

let example_input = {|..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.|}

type field = Paper | Empty

let field_of_char = function
  | '.' -> Empty
  | '@' -> Paper
  | _ -> failwith "unknown field"

let sum_2d_array (arr: int array array) =
  arr
  |> Array.map (Array.fold_left (+) 0)
  |> Array.fold_left (+) 0

let prep input =
  input
  |> split_lines
  |> Array.of_list
  |> Array.map (String.to_seq >> Seq.map field_of_char >> Array.of_seq)

let check_field_empty fields row col = match get_safe fields row col with
  | None -> Empty
  | Some Empty -> Empty
  | Some Paper -> Paper

let is_accessible (fields : field list) = 
  List.map(function | Empty -> 0 | Paper -> 1) fields
  |> List.fold_left (+) 0
  |> (fun paper_fields -> match paper_fields < 4 with | true -> 1 | false -> 0)

let check_neighbouring_fields fields row col =
  [(row - 1, col - 1); (row - 1, col); (row - 1, col + 1);
  (row, col - 1); (row, col + 1);
  (row + 1, col - 1); (row + 1, col); (row + 1, col + 1)]
  |> List.map (fun (row, col) -> check_field_empty fields row col)
  |> is_accessible

let get_accessible_fields fields =
  mapi_2d_array (fun row col _cell ->
    match check_field_empty fields row col with
    | Empty -> 0
    | Paper -> check_neighbouring_fields fields row col
  ) fields

let solve input =
  let fields = prep input in
  get_accessible_fields fields
  |> sum_2d_array

(* stage 2 *)

let remove_accessible_step fields accessible_fields =
  mapi_2d_array (fun row col cell ->
    match accessible_fields.(row).(col) with
    | 0 -> cell
    | 1 -> Empty
    | _ -> failwith "wrong accessible_fields value"
  ) fields

let rec remove_accessible fields =
  let accessible_fields = get_accessible_fields fields in
  let next_fields = remove_accessible_step fields accessible_fields in
  if next_fields = fields then next_fields else remove_accessible next_fields

let paper_difference fields final_fields =
  fields |> mapi_2d_array (fun row col cell ->
    if final_fields.(row).(col) = cell then 0 else 1
  )

let solve' input =
  let fields = prep input in
  let final_fields = remove_accessible fields in
  paper_difference fields final_fields
  |> sum_2d_array

let () =
  let input = read_input 4 in
  Printf.printf "Example 1: %d\n" (solve example_input);
  Printf.printf "Part 1: %d\n" (solve input);
  Printf.printf "Example 2: %d\n" (solve' example_input);
  Printf.printf "Part 2: %d\n" (solve' input);
