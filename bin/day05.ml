open Aoc2025.Utils

let example_input = {|3-5
10-14
16-20
12-18

1
5
8
11
17
32|}

type range = int * int

let in_range n ((a, z): range) = n >= a && n <= z

let prep input =
  let (ranges_str, ingredients_str) = input
    |> String.trim
    |> Str.split (Str.regexp "\n\n")
    |> function
    | [ranges_str; ingredients_str] -> (ranges_str, ingredients_str)
    | _ -> failwith "parsing failed on split \n\n" in
  let ranges_lines = split_lines ranges_str in
  let ingredients_lines = split_lines ingredients_str in
  let ranges = List.map (fun range_str ->
    range_str
    |> String.split_on_char '-'
    |> (function
    | [a; z] -> (int_of_string a, int_of_string z)
    | _ -> failwith "wrong range")
  ) ranges_lines in
  let ingredients = List.map int_of_string ingredients_lines in
  (ranges, ingredients)

let solve input =
  let (ranges, ingredients) = prep input in
  List.filter (fun ingredient -> List.exists (in_range ingredient) ranges) ingredients
  |> List.length

let () =
  let input = read_input 5 in
  Printf.printf "Example 1: %d\n" (solve example_input);
  Printf.printf "Part 1: %d\n" (solve input);
