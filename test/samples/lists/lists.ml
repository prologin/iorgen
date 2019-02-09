(**
   @param n the first list's size
   @param listInt a list containing ints
   @param size an other size
   @param listChar a list of char
   @param listString4 a list of strings of size 4
   @param matrix a matrix of int
*)
let lists n listInt size listChar listString4 matrix =
  (** TODO Aren't these lists beautifull? *)
  ()

let () =
  let n = read_int () in
  let listInt = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string in
  let size = read_int () in
  let listChar = List.init size (String.get (read_line ())) in
  let listString4 = List.init size (fun _ -> read_line ()) in
  let matrix = List.init size (fun _ -> read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string) in
  lists n listInt size listChar listString4 matrix
