(**
   @param a a first number
   @param b a second number
   @param c a third number
   @param n This one on a new line
   @param onePerLine an integer list, one per line
*)
let manualFormat a b c n onePerLine =
  (** TODO From the function perspective, this is just 4 integers *)
  ()

let () =
  let[@warning "-8"] [a; b; c] = read_line () |> String.split_on_char ' '|> List.map int_of_string [@warning "+8"] in 
  let n = read_int () in
  let onePerLine = List.init 3 (fun _ -> read_int ()) in
  manualFormat a b c n onePerLine
