open Aoc2025.Utils

let example_input = {|987654321111111
811111111111119
234234234234278
818181911112111|}

let prep input =
  input
  |> split_lines
  |> List.map (fun line -> line |> String.to_seq |> List.of_seq |> List.map ((String.make 1) >> int_of_string))

let rec find_largest_first_digit_index ?(digit=9) list =
  if digit < 0 then invalid_arg "digit must be >= 0";
  let len = List.length list in
  let index = List.find_index (fun el -> el = digit) list in
  let index' = match index with
    | Some i -> if i = (len - 1) then None else Some i
    | None -> None in
  match index' with
  | None -> find_largest_first_digit_index ~digit:(digit - 1) list
  | Some i -> i

let rec find_largest_digit_index ?(digit=9) list =
  if digit < 0 then invalid_arg "digit must be >= 0";
  let index = List.find_index (fun el -> el = digit) list in
  match index with
  | None -> find_largest_digit_index ~digit:(digit - 1) list
  | Some i -> i

let find_row_joltage row =
  let largest_first_digit_index = find_largest_first_digit_index row in
  let remainder = List.drop (largest_first_digit_index + 1) row in
  let largest_second_digit_index = find_largest_digit_index remainder in
  let first_digit = List.nth row largest_first_digit_index in
  let second_digit = List.nth remainder largest_second_digit_index in
  int_of_string (string_of_int first_digit ^ string_of_int second_digit)

let solve input =
  input
  |> prep
  |> List.map find_row_joltage
  |> List.fold_left (+) 0

let () =
  let input = read_input 3 in
  Printf.printf "Example 1: %d\n" (solve example_input);
  Printf.printf "Part 1: %d\n" (solve input);
  (* Printf.printf "Example 2: %d\n" (solve' example_input); *)
  (* Printf.printf "Part 2: %d\n" (solve' input); *)
