(** may conflict in c# *)
type console = {
  a : int; (** the first letter of the alphabet *)
  static : int; (** an integer *)
}

(** may conflict in c# *)
type system = {
  return : int; (** not the end of the function *)
  void : int list; (** not nothing *)
}

(** not the main function *)
type main = {
  int : system; (** not an integer *)
  ifTrue : int; (** should not cause conflict *)
}

(**
   @param if_ not a condition
   @param class_ not a class
   @param i just a string
   @param in_ not in
   @param for_ not a loop
   @param words contains lots of things
*)
let keywords if_ class_ i in_ for_ words =
  (** TODO If this compiles, it is already a good step! *)
  ()

let () =
  let if_ = read_int () in
  let class_ = (read_line ()).[0] in
  let i = read_line () in
  let in_ = Scanf.sscanf (read_line ()) "%d %d" (fun a static -> {a; static}) in
  let for_ = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string in
  let words = List.init 2 (fun _ -> let int = let return = read_int () in let void = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string in {return; void} in let ifTrue = read_int () in {int; ifTrue}) in
  keywords if_ class_ i in_ for_ words
