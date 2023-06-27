let hash_string n s =
  let rec aux r = function
    | -1 -> r
    | i -> aux ((r + Int.shift_left (int_of_char (s.[i])) (8 * i)) mod n) (i - 1)
  in aux 0 (String.length s - 1)

let add_b = Big_int.add_big_int
let pow = Big_int.shift_left_big_int
let str a = Big_int.big_int_of_int (int_of_char a)

let hash_string_big n s =
    let rec aux r = function
      | -1 -> r
      | i -> aux (add_b r (pow (str s.[i]) (8 * i))) (i - 1)
in Big_int.mod_big_int (aux Big_int.zero_big_int (String.length s - 1)) n

type 'a hashtable = {hash: 'a -> int; data: 'a list array} (* Structure d'ensemble *)

let create_ht f n = {hash = f; data = Array.make n []}
let member_ht a ht = List.mem a ht.data.(ht.hash a)
let add_ht a ht = ht.data.(ht.hash a) <- a::ht.data.(ht.hash a)
let remove_ht a ht = ht.data.(ht.hash a) <- List.filter (fun x -> x <> a) ht.data.(ht.hash a)

type ('a, 'b) hashdict = {hash: 'a -> int; data: ('a * 'b) list array}
let create_dict f n = {hash = f; data = Array.make n []}
let find_dict a hd = List.assoc_opt a hd.data.(hd.hash a)
let add_dict a b hd = hd.data.(hd.hash a) <- (a, b)::hd.data.(hd.hash a)
let remove_dict a hd = hd.data.(hd.hash a) <- List.filter (fun x -> fst x <> a) hd.data.(hd.hash a) 

type ('a, 'b) dyn_hashtable = {hash: int -> 'a -> int; mutable size: int; mutable data: ('a * 'b) list array}
let dyn_create n f = {hash = f; size = 0; data = Array.make n []}
let dyn_find a dyn_ht = List.assoc_opt a dyn_ht.data.(dyn_ht.hash (Array.length dyn_ht.data) a)
let dyn_redim dyn_ht =
  dyn_ht.data <- Array.append dyn_ht.data (Array.make (Array.length dyn_ht.data) [])
let rec dyn_add a b dyn_ht = match dyn_ht.size with
  |n when 3 * n < Array.length dyn_ht.data ->
    dyn_ht.size <- dyn_ht.size + 1;
    dyn_ht.data.(dyn_ht.hash dyn_ht.size a) <- (a,b)::dyn_ht.data.(dyn_ht.hash dyn_ht.size a)
  |_ -> dyn_redim dyn_ht; dyn_add a b dyn_ht
let dyn_remove a dyn_ht =
  dyn_ht.data.(dyn_ht.hash dyn_ht.size a) <- List.filter (fun x -> fst x <> a) dyn_ht.data.(dyn_ht.hash dyn_ht.size a);
  dyn_ht.size <- dyn_ht.size - 1
