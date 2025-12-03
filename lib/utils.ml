let read_input day =
  let filename = Printf.sprintf "resources/day%02d.txt" day in
  In_channel.with_open_text filename In_channel.input_all
  |> String.trim

let split_lines str = String.split_on_char '\n' str |> List.filter (fun s -> s <> "")

let unlines strs = String.concat "\n" strs

let (>>) f g x = g(f x)

let print_list lst =
  List.iter (fun x -> Printf.printf "%d " x) lst;
  print_newline ()
