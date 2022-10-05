(** Represents coordinates *)
type coordinates = {
  x : float; (** X *)
  y : float; (** Y *)
  z : float; (** Z *)
}

(** Mix of fields that go on one line *)
type inlined_mix = {
  integer : int; (** an integer *)
  char : char; (** a char *)
  float : float; (** a float *)
}

(** a struct of chars *)
type multiline_mix = {
  integer2 : int; (** an other integer *)
  string : string; (** a string of size 5 *)
  float2 : float; (** an other float *)
}

(**
   @param f a float
   @param g a float, greater than f
   @param point some coordinates
   @param n a number
   @param floatList a list of floats
   @param otherList a list of floats
   @param inlined some inlined structs
   @param multiline a multiline struct
*)
let floats f g point n floatList otherList inlined multiline =
  (** TODO Parsing is often easy, reprint mode is harder *)
  ()

let () =
  let f = read_float () in
  let g = read_float () in
  let point = Scanf.sscanf (read_line ()) "%g %g %g" (fun x y z -> {x; y; z}) in
  let n = read_int () in
  let floatList = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map float_of_string |> List.rev in
  let otherList = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map float_of_string |> List.rev in
  let inlined = List.init 3 (fun _ -> Scanf.sscanf (read_line ()) "%d %c %g" (fun integer char float -> {integer; char; float})) in
  let multiline = let integer2 = read_int () in let string = read_line () in let float2 = read_float () in {integer2; string; float2} in
  floats f g point n floatList otherList inlined multiline
