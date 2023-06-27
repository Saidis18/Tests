type 'a fifo_inter =
  |Nil
  |F of 'a * ('a fifo_inter ref) * ('a fifo_inter ref) (* (head, tail, last element of the tail) *)

type 'a fifo = Fifo of 'a fifo_inter ref

let pop (Fifo(l)) = match !l with
  |Nil -> failwith "Empty fifo"
  |F(a,f,_) ->
    l := !f;
    a

let add (Fifo(l)) c = match !l with
    |Nil ->
      let le = ref Nil in
      l := F(c,le,le) (* le: last element *)
    |F(a,f,le) ->
      let new_le = ref Nil in
      le := F(c,new_le,new_le); (* new_le: new last element,
         ce qui est astucieux c'est que, comme c'est une référence, on repercute dans toutes les sous-fifo *)
      l := F(a,f,new_le)