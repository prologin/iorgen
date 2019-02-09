module List = struct
  include List

  let init n f =
    let rec aux i =
      if i >= n then [] else
        let r = f i in
        r :: aux (i+1) in
    aux 0
end

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

(** a struct of chars *)
type chars = {
  firstChar : char; (** a first char *)
  secondChar : char; (** a second char *)
  thirdChar : char; (** a third char *)
}

(**
   @param struct_ a struct 1 instance
   @param n a number
   @param structList a list a struct 1
   @param triangle a triangle
   @param structChars a struct of chars
*)
let structs struct_ n structList triangle structChars =
  (** TODO Look at them structs. *)
  ()

let () =
  let struct_ = Scanf.sscanf (read_line ()) "%d %d" (fun foo bar -> {foo; bar}) in
  let n = read_int () in
  let structList = List.init n (fun _ -> Scanf.sscanf (read_line ()) "%d %d" (fun foo bar -> {foo; bar})) in
  let triangle = List.init 3 (fun _ -> let name = (read_line ()).[0] in let pos = Scanf.sscanf (read_line ()) "%d %d %d" (fun x y z -> {x; y; z}) in {name; pos}) in
  let structChars = Scanf.sscanf (read_line ()) "%c %c %c" (fun firstChar secondChar thirdChar -> {firstChar; secondChar; thirdChar}) in
  structs struct_ n structList triangle structChars
