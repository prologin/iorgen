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
  let if_ = Scanf.scanf "%d\n" (fun x -> x) in
  let class_ = Scanf.scanf "%c\n" (fun x -> x) in
  let i = Scanf.scanf "%s@\n" (fun x -> x) in
  let in_ = Scanf.scanf "%d %d\n" (fun a static -> {a = a; static = static}) in
  let for_ = List.init if_ (fun _ -> Scanf.scanf "%d " (fun x -> x)) in
  let words = List.init 2 (fun _ -> let int = let return = Scanf.scanf "%d\n" (fun x -> x) in let void = List.init 3 (fun _ -> Scanf.scanf "%d " (fun x -> x)) in {return = return; void = void} in let ifTrue = Scanf.scanf "%d\n" (fun x -> x) in {int = int; ifTrue = ifTrue}) in
  keywords if_ class_ i in_ for_ words
