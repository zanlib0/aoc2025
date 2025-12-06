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

let get_safe array row col =
  if row >= 0 && row < Array.length array &&
    col >= 0 && col < Array.length array.(row)
  then Some array.(row).(col)
  else None

let mapi_2d_array f arr =
  arr |> Array.mapi(fun row row' ->
    row' |> Array.mapi(fun col cell ->
      f row col cell
    )
  )

let lazy_range (start, finish) =
  Seq.unfold (fun n ->
    if n > finish then None
    else Some (n, n+1)
  ) start

