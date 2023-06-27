let rec diff l1 = function
	|[] -> true
	|h2::t2 ->
		let rec aux = function
			|[] -> diff l1 t2
			|h1::_ when h1 = h2 -> false
			|_::t2 -> aux t2 in
		aux l1

let div l =
	let rec aux l1 l2 = function
		|[] -> l1,l2
		|[x] -> x::l1,l2
		|x::y::t -> aux (x::l1) (y::l2) t in
	aux [] [] l

let rec tous_distincts1 = function
	|[] |[_] -> true
	|l ->
		let l1,l2 = div l in
		tous_distincts1 l1 && tous_distincts1 l2 && diff l1 l2

let rec tous_distincts2 = function
	|[] |[_] -> true
	|h::t -> (not (List.mem h t)) && tous_distincts2 t

let id n =
	let rec aux l = function
		|0 -> l
		|k -> aux (k::l) (k-1) in 
	aux [] n

let f1 = function `A x when x = 1 -> 1 | `A _ -> 0 | `B -> 1 | `C -> 0
let f2 = function `A x when x = "a" -> 1 | `A _ -> 0 | `B -> 1
let f x = f1 x + f2 x








