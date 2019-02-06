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
  let listinit n f =
    let rec aux i = if i >= n then [] else let r = f i in r :: aux (i+1) in
    aux 0 in

  let stringsplit_on_char sep s = (* OCaml 4.04: String.split_on_char *)
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if String.unsafe_get s i = sep then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    String.sub s 0 !j :: !r in

  let emptyList = Scanf.scanf "%s@\n" (fun x -> if String.length x == 0 then [] else List.map int_of_string (stringsplit_on_char ' ' x)) in
  let bufferString = Scanf.scanf "%s@\n" (fun x -> x) in
  let n = Scanf.scanf "%d\n" (fun x -> x) in
  let emptyInSample = Scanf.scanf "%s@\n" (fun x -> if String.length x == 0 then [] else List.map int_of_string (stringsplit_on_char ' ' x)) in
  let emptyString = Scanf.scanf "%s@\n" (fun x -> x) in
  let main = Scanf.scanf "%s@\n" (fun x -> x) in
  let emptyCharList = Scanf.scanf "%s@\n" (fun x -> listinit 0 (String.get x)) in
  let nonEmptyCharList = Scanf.scanf "%s@\n" (fun x -> listinit 5 (String.get x)) in
  let structWithEmptyLine = let listInStruct = Scanf.scanf "%s@\n" (fun x -> if String.length x == 0 then [] else List.map int_of_string (stringsplit_on_char ' ' x)) in let structInStruct = Scanf.scanf "%c %d\n" (fun char1 int2 -> {char1 = char1; int2 = int2}) in {listInStruct = listInStruct; structInStruct = structInStruct} in
  let aSizedStruct = let size = Scanf.scanf "%d\n" (fun x -> x) in let stringInStruct = Scanf.scanf "%s@\n" (fun x -> x) in {size = size; stringInStruct = stringInStruct} in
  let finish = Scanf.scanf "%s@\n" (fun x -> x) in
  emptyLines emptyList bufferString n emptyInSample emptyString main emptyCharList nonEmptyCharList structWithEmptyLine aSizedStruct finish
