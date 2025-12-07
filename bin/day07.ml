open Aoc2025.Utils

let example_input = {|.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............|}

type field = Start | Split | Empty

let field_of_char = function
  | 'S' -> Start
  | '^' -> Split
  | '.' -> Empty
  | _ -> invalid_arg "field_of_string: invalid arg"

module IntSet = Set.Make(Int)

let prep_line = String.to_seq >> Seq.map field_of_char >> List.of_seq

let prep input = 
  input
  |> String.trim
  |> split_lines
  |> List.map prep_line
  |> (function
    | [] -> failwith "empty"
    | h :: t -> (h, t))

let find_starting_index head =
  match List.find_index ((=) Start) head with
  | Some n -> n
  | None -> failwith "wrong head"

let get_splitter_indices line = 
  line
  |> List.mapi (fun index value ->
    match value with
    | Split -> [index]
    | _ -> []) 
  |> List.concat

let split_beam beams splitter = beams |> IntSet.remove splitter |> IntSet.add (splitter - 1) |> IntSet.add (splitter + 1)

let get_next_beam_indices (beams, count) splitters =
  List.fold_left (fun (beams, count) splitter -> 
    if IntSet.mem splitter beams
    then (split_beam beams splitter, count + 1)
    else (beams, count)
  ) (beams, count) splitters

let check_line beams_tuple line =
  let splitters = get_splitter_indices line in
  get_next_beam_indices beams_tuple splitters

let fold_lines lines starting_index = 
  let start = IntSet.add starting_index IntSet.empty in
  List.fold_left check_line (start, 0) lines

let solve input =
  let (head, lines) = prep input in
  let starting_index = find_starting_index head in
  let (_final_beams, count) = fold_lines lines starting_index in
  count

let () =
  let input = read_input 7 in
  Printf.printf "Example 1: %d\n" (solve example_input);
  Printf.printf "Part 1: %d\n" (solve input);
