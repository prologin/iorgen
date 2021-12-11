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
  let emptyList = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  let bufferString = read_line () in
  let n = read_int () in
  let emptyInSample = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  let emptyString = read_line () in
  let main = read_line () in
  let emptyCharList = List.init 0 (String.get (read_line ())) in
  let nonEmptyCharList = List.init 5 (String.get (read_line ())) in
  let structWithEmptyLine = let listInStruct = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in let structInStruct = Scanf.sscanf (read_line ()) "%c %d" (fun char1 int2 -> {char1; int2}) in {listInStruct; structInStruct} in
  let aSizedStruct = let size = read_int () in let stringInStruct = read_line () in {size; stringInStruct} in
  let finish = read_line () in
  emptyLines emptyList bufferString n emptyInSample emptyString main emptyCharList nonEmptyCharList structWithEmptyLine aSizedStruct finish
