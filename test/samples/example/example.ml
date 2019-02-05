(** A struct for the example *)
type a_struct = {
  integer : int; (** an integer *)
  character : char; (** a char *)
}

(**
   @param n a number, used as a size
   @param list_ a list of structs
*)
let example n list_ =
  (** TODO In a real life scenario, you will describe here what you want the
  end user to do with this generated code *)
  ()

let () =
  let listinit n f =
    let rec aux i = if i >= n then [] else let r = f i in r :: aux (i+1) in
    aux 0 in

  let n = Scanf.scanf "%d\n" (fun x -> x) in
  let list_ = listinit n (fun _ -> Scanf.scanf "%d %c\n" (fun integer character -> {integer = integer; character = character})) in
  example n list_
