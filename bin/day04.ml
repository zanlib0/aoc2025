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

let accessible_fields fields =
  Array.mapi (fun row row_value -> Array.mapi (fun col _ -> 
    match check_field_empty fields row col with
    | Empty -> 0
    | Paper -> check_neighbouring_fields fields row col
  ) row_value) fields

let solve input =
  let fields = prep input in
  accessible_fields fields
  |> Array.map (Array.fold_left (+) 0)
  |> Array.fold_left (+) 0

let () =
  let input = read_input 4 in
  Printf.printf "Example 1: %d\n" (solve example_input);
  Printf.printf "Part 1: %d\n" (solve input);
