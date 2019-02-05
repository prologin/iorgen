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
  let listinit n f =
    let rec aux i = if i >= n then [] else let r = f i in r :: aux (i+1) in
    aux 0 in

  let n = Scanf.scanf "%d\n" (fun x -> x) in
  let listInt = Scanf.scanf "%s@\n" (fun x -> List.map int_of_string (String.split_on_char ' ' x)) in
  let size = Scanf.scanf "%d\n" (fun x -> x) in
  let listChar = Scanf.scanf "%s@\n" (fun x -> listinit size (String.get x)) in
  let listString4 = listinit size (fun _ -> Scanf.scanf "%s@\n" (fun x -> x)) in
  let matrix = listinit size (fun _ -> Scanf.scanf "%s@\n" (fun x -> if String.equal "" x then [] else List.map int_of_string (String.split_on_char ' ' x))) in
  lists n listInt size listChar listString4 matrix
