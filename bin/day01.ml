open Aoc2025.Utils

let example_input = {|L68
L30
R48
L5
R60
L55
L1
L99
R14
L82|}

type instruction =
  | TurnLeft of int
  | TurnRight of int

let parse_turn instruction =
  Scanf.sscanf instruction "%c%d" (fun dir steps  ->
    match dir with
    | 'L' -> TurnLeft steps
    | 'R' -> TurnRight steps
    | _ -> failwith "Invalid direction"
  )

(* OCaml keeps sign of dividend when doing mod instead of wrapping. *)
let wrap n =
  let r = n mod 100 in
  if r < 0 then r + 100 else r

let step (pos, tally) instruction =
  let new_pos = 
    match instruction with
    | TurnLeft steps -> pos - (steps mod 100)
    | TurnRight steps -> pos + (steps mod 100)
    |> wrap in
  let new_tally = if new_pos = 0 then tally + 1 else tally in
  (wrap new_pos, new_tally)

let solve input =
  let lines = split_lines input in
  let instructions = List.map parse_turn lines in
  let (_final_pos, tally) = List.fold_left step (50, 0) instructions in
  tally

let () =
  let input = read_input 1 in
  Printf.printf "Example: %d\n" (solve example_input);
  Printf.printf "Part 1: %d\n" (solve input)
