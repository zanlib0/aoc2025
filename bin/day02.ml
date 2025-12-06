open Aoc2025.Utils

let example_input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

let parse_ranges input =
  String.split_on_char ',' (String.trim input)
  |> List.map (fun range_str ->
    match String.split_on_char '-' range_str with
      | [start; finish] -> (int_of_string start, int_of_string finish)
      | _ -> failwith "Invalid range format")

let remove_odds =
  Seq.filter (fun num ->
    num
    |> string_of_int
    |> String.length
    |> (fun len -> len mod 2)
    |> (=) 0
  )

let filter_doubles =
  Seq.filter (fun num ->
    let str = string_of_int num in
    let len = String.length str in
    let front_half = String.sub str 0 (len / 2) in
    let back_half = String.sub str (len / 2) (len / 2) in
    front_half = back_half
  )

let prep input =
  let ranges = parse_ranges input in
  let seqs = List.map lazy_range ranges in
  seqs

    
let solve input =
  prep input
  |> List.concat_map (remove_odds >> filter_doubles >> List.of_seq)
  |> List.fold_left (+) 0

(* stage 2 *)

let get_substrings num =
  let str = string_of_int num in
  let limit = (String.length str) / 2 in
  lazy_range (1, limit)
  |> Seq.map(fun n -> String.sub str 0 n)
  |> List.of_seq

let drop n str =
  let len = String.length str in
  let n' = min n len in
  String.sub str n' (len - n')

let rec consists_of_substring str substr =
  let substr_len = String.length substr in

  if str = substr then true
  else if (substr_len > String.length str) then false
  else if (String.sub str 0 substr_len) <> substr then false
  else consists_of_substring (drop substr_len str) substr

let check_number num =
  get_substrings num
  |> List.exists (fun substr -> consists_of_substring (string_of_int num) substr)

let filter_seq = Seq.filter check_number

let solve' input =
  prep input
  |> List.concat_map (filter_seq >> List.of_seq)
  |> List.fold_left (+) 0

let () =
  let input = read_input 2 in
  Printf.printf "Example 1: %d\n" (solve example_input);
  Printf.printf "Part 1: %d\n" (solve input);
  Printf.printf "Example 2: %d\n" (solve' example_input);
  Printf.printf "Part 2: %d\n" (solve' input);
