(** a char struct *)
type struct_with_a_char = {
  char1 : char; (** a char *)
  int2 : int; (** an integer *)
}

(** a struct *)
type a = {
  listInStruct : int list; (** a list in a struct *)
  structInStruct : struct_with_a_char; (** a struct in a struct *)
}

(** a sized struct *)
type sized_struct = {
  size : int; (** the size *)
  stringInStruct : string; (** the string *)
}

(**
   @param emptyList an empty list
   @param bufferString here to check correct parsing of empty line above
   @param n an integer, will be 0 in the sample input
   @param emptyInSample an empty list (only in the sample)
   @param emptyString an empty string
   @param main an other buffer string
   @param emptyCharList an empty char list
   @param nonEmptyCharList an char list, non empty
   @param structWithEmptyLine a struct containing an empty line, then a struct
   @param aSizedStruct a sized struct containing an empty line
   @param finish a string to finish
*)
let emptyLines emptyList bufferString n emptyInSample emptyString main emptyCharList nonEmptyCharList structWithEmptyLine aSizedStruct finish =
  (** TODO Wow, lots of empty lines! *)
  ()

let () =
  let emptyList = Scanf.scanf "%s@\n" (fun x -> if String.equal "" x then [] else List.map int_of_string (String.split_on_char ' ' x)) in
  let bufferString = Scanf.scanf "%s@\n" (fun x -> x) in
  let n = Scanf.scanf "%d\n" (fun x -> x) in
  let emptyInSample = Scanf.scanf "%s@\n" (fun x -> if String.equal "" x then [] else List.map int_of_string (String.split_on_char ' ' x)) in
  let emptyString = Scanf.scanf "%s@\n" (fun x -> x) in
  let main = Scanf.scanf "%s@\n" (fun x -> x) in
  let emptyCharList = Scanf.scanf "%s@\n" (fun x -> List.init 0 (String.get x)) in
  let nonEmptyCharList = Scanf.scanf "%s@\n" (fun x -> List.init 5 (String.get x)) in
  let structWithEmptyLine = let listInStruct = Scanf.scanf "%s@\n" (fun x -> if String.equal "" x then [] else List.map int_of_string (String.split_on_char ' ' x)) in let structInStruct = Scanf.scanf "%c %d\n" (fun char1 int2 -> {char1 = char1; int2 = int2}) in {listInStruct = listInStruct; structInStruct = structInStruct} in
  let aSizedStruct = let size = Scanf.scanf "%d\n" (fun x -> x) in let stringInStruct = Scanf.scanf "%s@\n" (fun x -> x) in {size = size; stringInStruct = stringInStruct} in
  let finish = Scanf.scanf "%s@\n" (fun x -> x) in
  emptyLines emptyList bufferString n emptyInSample emptyString main emptyCharList nonEmptyCharList structWithEmptyLine aSizedStruct finish
