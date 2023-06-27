open Tests.Lib

let ignore _ = ()

let l = id 1000

let t0 = Unix.time ()
let () = ignore (tous_distincts1 l)
let t1 = Unix.time ()
let () = ignore (tous_distincts2 l)
let t2 = Unix.time ()

let () =
  print_string "Temps diviser pour régner: ";
  print_float (t1 -. t0);
  print_string "s";
  print_newline ();
  print_string "Temps classique: ";
  print_float (t2 -. t1);
  print_string "s";
  print_newline()


open Tests.Fifo
open Tests.Graph
let l = Fifo(ref Nil)
  
let () = for i = 10 downto 0 do
  add l i;
done
  
let () = for _ = 0 to 10 do
  print_int (pop l);
  print_string " ";
done


let rec
  l1 = N(0,'a',[l3;l2]) and
  l2 = N(1,'b',[l3]) and
  l3 = N(2,'c',[l4]) and
  l4 = N(3,'d',[l1])  
let g = G(4,[l1;l2;l3;l4])


let () = print_newline ()
let () = parcoursBFS print_char g 0
let () = print_newline ()
let () = parcoursDFS print_char g 0


open Tests.Hash
let h = dyn_create 1 hash_string

let () = dyn_add "Pommes de terre" 5 h
let () = print_int (Array.length h.data); print_newline()
let () = dyn_add "Savon" 2 h
let () = print_int (Array.length h.data); print_newline()
let () = dyn_add "Lait" 1 h
let () = print_int (Array.length h.data); print_newline()
let () = dyn_add "Jambon" 4 h
let () = print_int (Array.length h.data); print_newline()
let () = dyn_add "Playstation" 500 h
let () = print_int (Array.length h.data); print_newline()
let () = dyn_add "Oeufs" 4 h
let () = print_int (Array.length h.data); print_newline()

let r1 = Array.make 1000 1
let _ = Thread.create (Array.mapi (fun i _ -> i)) r1

let () = Thread.delay 1.

let () = for i = 0 to 999 do
  print_int r1.(i)
done
