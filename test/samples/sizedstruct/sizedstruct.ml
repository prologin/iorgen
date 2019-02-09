module List = struct
  include List

  let init n f =
    let rec aux i =
      if i >= n then [] else
        let r = f i in
        r :: aux (i+1) in
    aux 0
end

module String = struct
  include String

  let split_on_char sep s = (* OCaml 4.04: String.split_on_char *)
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if String.unsafe_get s i = sep then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    String.sub s 0 !j :: !r
end

(** contains a list *)
type list_ = {
  size1 : int; (** the list's size *)
  intList : int list; (** the integer list *)
}

(** contains a string *)
type string_ = {
  size2 : int; (** the list's size *)
  stringList : string; (** the string list *)
}

(** contains a matrix *)
type matrix = {
  size3 : int; (** the list's size *)
  listList : int list list; (** the list list *)
}

(** this is not a 'sized struct', but a regular one! *)
type not_a_sized_struct = {
  size4 : int; (** not the list's size *)
  intListN : int list; (** the integer list *)
}

(**
   @param n the size of the lists
   @param lists a list of list of different sizes
   @param strings a list of strings of different sizes
   @param matrices a list of matrices of different sizes
   @param same a list of list of same sizes
*)
let sizedStruct n lists strings matrices same =
  (** TODO The is a special case. *)
  ()

let () =
  let n = read_int () in
  let lists = List.init n (fun _ -> let size1 = read_int () in let intList = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string in {size1; intList}) in
  let strings = List.init n (fun _ -> let size2 = read_int () in let stringList = read_line () in {size2; stringList}) in
  let matrices = List.init 2 (fun _ -> let size3 = read_int () in let listList = List.init size3 (fun _ -> read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string) in {size3; listList}) in
  let same = List.init n (fun _ -> let size4 = read_int () in let intListN = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.map int_of_string in {size4; intListN}) in
  sizedStruct n lists strings matrices same
