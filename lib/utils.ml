let read_input day =
  let filename = Printf.sprintf "resources/day%02d.txt" day in
  In_channel.with_open_text filename In_channel.input_all

let split_lines str = String.split_on_char '\n' str |> List.filter (fun s -> s <> "")

let unlines strs = String.concat "\n" strs
