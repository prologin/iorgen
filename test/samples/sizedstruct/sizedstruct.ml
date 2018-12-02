(** contains a list *)
type list_ = {
  size1 : int; (** the list's size *)
  intList : int list; (** the integer list *)
}

(** contains a string *)
type string_ = {
  size2 : int; (** the list's size *)
  stringList : string; (** the string list *)
}

(** contains a matrix *)
type matrix = {
  size3 : int; (** the list's size *)
  listList : int list list; (** the list list *)
}

(** this is not a 'sized struct', but a regular one! *)
type not_a_sized_struct = {
  size4 : int; (** not the list's size *)
  intListN : int list; (** the integer list *)
}

(**
   @param n the size of the lists
   @param lists a list of list of different sizes
   @param strings a list of strings of different sizes
   @param matrices a list of matrices of different sizes
   @param same a list of list of same sizes
*)
let sizedStruct n lists strings matrices same =
  (** TODO The is a special case. *)
  ()

let () =
  let n = Scanf.scanf "%d\n" (fun x -> x) in
  let lists = List.init n (fun _ -> let size1 = Scanf.scanf "%d\n" (fun x -> x) in let intList = Scanf.scanf "%s@\n" (fun x -> if String.equal "" x then [] else List.map int_of_string (String.split_on_char ' ' x)) in {size1 = size1; intList = intList}) in
  let strings = List.init n (fun _ -> let size2 = Scanf.scanf "%d\n" (fun x -> x) in let stringList = Scanf.scanf "%s@\n" (fun x -> x) in {size2 = size2; stringList = stringList}) in
  let matrices = List.init 2 (fun _ -> let size3 = Scanf.scanf "%d\n" (fun x -> x) in let listList = List.init size3 (fun _ -> Scanf.scanf "%s@\n" (fun x -> List.map int_of_string (String.split_on_char ' ' x))) in {size3 = size3; listList = listList}) in
  let same = List.init n (fun _ -> let size4 = Scanf.scanf "%d\n" (fun x -> x) in let intListN = Scanf.scanf "%s@\n" (fun x -> if String.equal "" x then [] else List.map int_of_string (String.split_on_char ' ' x)) in {size4 = size4; intListN = intListN}) in
  sizedStruct n lists strings matrices same
