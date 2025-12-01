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

let dial_starter = 50

let parse_turn instruction =
  Scanf.sscanf instruction "%c%d" (fun dir steps  ->
    match dir with
    | 'L' -> TurnLeft steps
    | 'R' -> TurnRight steps
    | _ -> failwith "Invalid direction"
  )

let solve input =
  let lines = split_lines input in
  let instructions = List.map parse_turn lines in
  instructions
