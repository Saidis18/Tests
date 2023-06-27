type 'a sub_flot = Nf of int * 'a * (('a sub_flot * float) list) (* clé de l'élément, élément, voisins/fils de l'élément (avec capacité) *)
type 'a flot = Gf of int * 'a sub_flot list (* ordre du graphe, liste des sous graphes (assimilables aux élements) *)


let verif_flot (Gf(l,g)) =
  (* Vérifie si le graphe est bien construit:
	l'ordre len du graphe est bien celui indiqué en argument l et l'ensemble des clés est [[1, ..., len]]*)
	let len = List.length g in
	len = l && (
	
	let r = Array.make len (-1) in

	let rec get_keys = function
		| [] -> ()
		| Nf(k,_,_)::_ when k >= len -> ()
		| Nf(k,_,_)::t ->
			r.(k) <- k;
			get_keys t;
	in
	
	let id = Array.make len 0 in
	for i = 0 to len - 1 do
		id.(i) <- i
	done;
	get_keys g;
	r = id
	)


let ordre (Gf(n,_)) = n


let entry_point_sub (Nf(_,a,_)) = a


let entry_point (Gf(_,l)) = match l with
	(* Un premier point d'entrée (il n'y a pas de point privilégié) *)
	|[] -> failwith "Graphe vide"
	|h::_ -> entry_point_sub h


let n_eme_elmt (Gf(len,l)) n =
	(* Associe à une clé n l'unique élément dont c est la clé *)
	if n >= len then failwith "Clé non valide" else
	let rec aux k = function
		|[] -> failwith "Clé non valide"
		|h::_ when k = n -> entry_point_sub h
		|_::t -> aux (k + 1) t in
	aux 0 l


let rec degre (Gf(len,l)) p = match l with
	(* Degré du sommet p du graphe *)
  |[] -> failwith "Graphe vide"
  |Nf(_,a,l1)::_ when a = p -> List.length l1
  |_::t -> degre (Gf(len - 1, t)) p


let rec voisins (Gf(len,l)) p = match l with
	(* Liste des voisins du sommet p du graphe *)
  |[] -> failwith "Graphe vide"
  |Nf(_,a,l1)::_ when a = p ->
		let f a = entry_point_sub (fst a) in
		List.map f l1
  |_::t -> voisins (Gf(len - 1, t)) p


let ajout_elmt a (Gf(n,l)) = Gf(n + 1, Nf(n, a, [])::l)
(* L'ordre des éléments n'a aucune importance, on peut donc ajouter au début de la liste *)


let ajout_arete depart arrivee poids (Gf(n,l)) =
	let rec aux elmt_depart elmt_arrivee r = function
		(* On empile les éléments dans r jusqu'à trouver celui de départ, on modifie l'élément et on l'ajoute
		au début de la liste des éléments (puisque l'ordre des éléments n'a aucune importance) formée de ceux qu'on a 
		visité et ceux que'on a empilé dans r *)
		|[] -> failwith "Départ absent dans le graphe"
		|Nf(k,a,l1)::t when a = elmt_depart -> Nf(k,a,(elmt_arrivee, poids)::l1)::(r@t)
		|h::t -> aux elmt_depart elmt_arrivee (h::r) t in
	
	let rec trouver_arrivee elmt_arrivee = function
		|[] -> failwith "Arrivée non présente dans le graphe"
		|h::_ when entry_point_sub h = elmt_arrivee -> h
		|_::t -> trouver_arrivee elmt_arrivee t in
	
	let elmt_arrivee = trouver_arrivee arrivee l in
	Gf(n,aux depart elmt_arrivee [] l)


let parcoursDFS f (Gf(n,g)) s0 = (* f est une procédure à appliquer au graphe, s0 la clé du point de départ *)
	if not (verif_flot (Gf(n,g))) then failwith "Flot mal construit" else

	let n_eme_sub (Gf(len,l)) n = (* Associe à une clé n l'unique sous_graphe (élément) dont c est la clé *)
		if n >= len then failwith "Clé non valide" else
		let rec aux k = function
			|[] -> failwith "Clé non valide"
			|h::_ when k = n -> h
			|_::t -> aux (k + 1) t in
		aux 0 l in
	
	let visites = Array.make n false in
	
	let rec traitement s =
		let Nf(k,a,l) = s in
		if not visites.(k) then
		begin
			f a;
			print_newline();
			visites.(k) <- true;
			List.iter traitement (List.map fst l)
		end
	in
	traitement (n_eme_sub (Gf(n,g)) s0)


let parcoursBFS f (Gf(n,g)) s0 = (* f est une procedure à appliquer au graphe, s0 la clé du point de départ *)
	if not (verif_flot (Gf(n,g))) then failwith "Flot mal construit" else
	
	let n_eme_sub (Gf(len,l)) n = (* Associe à une clé n l'unique sous_graphe (élément) dont c est la clé *)
		if n >= len then failwith "Clé non valide" else
		let rec aux k = function
			|[] -> failwith "Clé non valide"
			|h::_ when k = n -> h
			|_::t -> aux (k + 1) t in
		aux 0 l in
	
	let visites = Array.make n false in
	let file = Queue.create() in
	
	let traitement s =
		let Nf(k,a,_) = s in
		if not visites.(k) then
		begin
			f a;
			print_newline();
			visites.(k) <- true;
			Queue.add s file;
		end
	in
	
	traitement (n_eme_sub (Gf(n,g)) s0) ;
	while not (Queue.is_empty file) do
		let s = Queue.take file in
		let Nf(_,_,l) = s in
		List.iter traitement (List.map fst l)
	done