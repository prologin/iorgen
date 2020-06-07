(* Emulate List.init from OCaml 4.06 *)
module List = struct
  include List

  let init n f =
    let rec aux i =
      if i >= n then [] else
        let r = f i in
        r :: aux (i+1) in
    aux 0
end

(* Copy String.split_on_char from OCaml 4.04 *)
module String = struct
  include String

  let split_on_char sep s =
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

(**
   @param n the first list's size
   @param listInt a list containing ints
   @param size an other size
   @param listChar a list of char
   @param listString4 a list of strings of size 4
   @param matrix a matrix of int
*)
let lists n listInt size listChar listString4 matrix =
  (** TODO Aren't these lists beautifull? *)
  ()

let () =
  let n = read_int () in
  let listInt = read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev in
  let size = read_int () in
  let listChar = List.init size (String.get (read_line ())) in
  let listString4 = List.init size (fun _ -> read_line ()) in
  let matrix = List.init size (fun _ -> read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev) in
  lists n listInt size listChar listString4 matrix
