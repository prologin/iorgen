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
  pos : position; (** the point's position *)
}

(**
   @param struct_ a struct 1 instance
   @param n a number
   @param structList a list a struct 1
   @param triangle a triangle
*)
let structs struct_ n structList triangle =
  (** TODO Look at them structs. *)
  ()

let () =
  let struct_ = Scanf.scanf "%d %d\n" (fun foo bar -> {foo = foo; bar = bar}) in
  let n = Scanf.scanf "%d\n" (fun x -> x) in
  let structList = List.init n (fun _ -> Scanf.scanf "%d %d\n" (fun foo bar -> {foo = foo; bar = bar})) in
  let triangle = List.init 3 (fun _ -> let name = Scanf.scanf "%c\n" (fun x -> x) in let pos = Scanf.scanf "%d %d %d\n" (fun x y z -> {x = x; y = y; z = z}) in {name = name; pos = pos}) in
  structs struct_ n structList triangle
