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

(* stage 2 *)

let disjoint (a, b) (c, d) = c > b || a > d
let merge (a, b) (c, d) = ((min a c), (max b d))
let is_empty (a, b) = a > b
let count (a, b) = b - a + 1

let clean ranges =
  ranges
  |> List.sort compare
  |> List.fold_left (fun acc r ->
    match acc with
    | [] -> [r]
    | current :: tail -> if (disjoint r current) then r :: current :: tail else (merge r current) :: tail
  ) []
  |> List.filter (Fun.negate is_empty)
  |> List.rev

let solve' input =
  let (ranges, _) = prep input in
  clean ranges
  |> List.map count
  |> List.fold_left (+) 0

let () =
  let input = read_input 5 in
  Printf.printf "Example 1: %d\n" (solve example_input);
  Printf.printf "Part 1: %d\n" (solve input);
  Printf.printf "Example 2: %d\n" (solve' example_input);
  Printf.printf "Part 2: %d\n" (solve' input);
