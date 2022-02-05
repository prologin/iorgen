(** A simple struct *)
type struct_1 = {
  foo : int; (** a field *)
  bar : int; (** a field *)
}

(** Represents a position *)
type position = {
  x : int; (** X *)
  y : int; (** Y *)
  z : int; (** Z *)
}

(** A point's name and position *)
type point = {
  name : char; (** the point's name (single character) *)
  description : string; (** the point's description *)
  pos : position; (** the point's position *)
}

(** a struct of chars *)
type chars = {
  firstChar : char; (** a first char *)
  secondChar : char; (** a second char *)
  thirdChar : char; (** a third char *)
}

(** contains a big list inside *)
type with_list = {
  int : int; (** int *)
  bigList : int list list list; (** list nested 3 times! *)
}

(**
   @param struct_ a struct 1 instance
   @param n a number
   @param structList a list a struct 1
   @param triangle a triangle
   @param structChars a struct of chars
   @param bigListStruct the big list struct
*)
let structs struct_ n structList triangle structChars bigListStruct =
  (** TODO Look at them structs. *)
  ()

let () =
  let struct_ = Scanf.sscanf (read_line ()) "%d %d" (fun foo bar -> {foo; bar}) in
  let n = read_int () in
  let structList = List.init n (fun _ -> Scanf.sscanf (read_line ()) "%d %d" (fun foo bar -> {foo; bar})) in
  let triangle = List.init 3 (fun _ -> let name = (read_line ()).[0] in let description = read_line () in let pos = Scanf.sscanf (read_line ()) "%d %d %d" (fun x y z -> {x; y; z}) in {name; description; pos}) in
  let structChars = Scanf.sscanf (read_line ()) "%c %c %c" (fun firstChar secondChar thirdChar -> {firstChar; secondChar; thirdChar}) in
  let bigListStruct = let int = read_int () in let bigList = List.init 2 (fun _ -> List.init 2 (fun _ -> read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev)) in {int; bigList} in
  structs struct_ n structList triangle structChars bigListStruct
