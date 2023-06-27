type formule =
	| Var of char
	| Non of formule
	| Et of formule * formule
	| Ou of formule * formule
	| Imp of formule * formule
	| Equ of formule * formule

let rec evalue f env = match f with
  | Var a -> List.assoc a env
  | Non f1 -> not (evalue f1 env)
  | Et (f1, f2) -> (evalue f1 env) && (evalue f2 env)
  | Ou (f1, f2) -> (evalue f1 env) || (evalue f2 env)
  | Imp (f1, f2) -> evalue (Ou (Non f1, f2)) env
  | Equ (f1, f2) -> evalue f1 env = evalue f2 env

let rec union l1 l2 = match l1 with
  | [] -> l2
  | h::t when List.mem h l2 -> union t l2
  | h::t -> h::union t l2


let rec liste_var = function
| Var a -> [a]
| Non f1 -> liste_var f1
| Et (f1, f2)
| Ou (f1, f2)
| Imp (f1, f2)
| Equ (f1, f2) -> union (liste_var f1) (liste_var f2)

let ajout_tous x = List.map (fun l -> x::l)

let rec toutes_valuations = function
  | [] -> [[]]
  | h::t ->
    let l = toutes_valuations t in
    (ajout_tous (h, true) l) @ (ajout_tous (h, false) l)

let liste_modeles f = f |> liste_var |> toutes_valuations |> List.filter (evalue f)

let liste_valuations f = f |> liste_var |> toutes_valuations

let satisfiable f = List.fold_left (||) false (f |> liste_valuations |> List.map (evalue f))

let tautologie f = List.fold_left (&&) true (f |> liste_valuations |> List.map (evalue f))

let equiv_semantique f1 f2 = tautologie (Equ (f1, f2))

let consequence f1 f2 = tautologie (Imp (f1, f2))
