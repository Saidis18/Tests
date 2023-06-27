type 'a sub_graph = N of int * 'a * 'a sub_graph list (* clé de l'élément, élément, voisins/fils de l'élément *)
type 'a graph = G of int * 'a sub_graph list (* ordre du graphe, liste des sous graphes (assimilables aux élements) *)

let verif_graph (G(l,g)) =
  (* Vérifie si le graphe est bien construit:
	l'ordre len du graphe est bien celui indiqué en argument l et l'ensemble des clés est [[1, ..., len]]*)
	let len = List.length g in
	len = l && (
	
	let r = Array.make len (-1) in

	let rec get_keys = function
		| [] -> ()
		| N(k,_,_)::_ when k >= len -> ()
		| N(k,_,_)::t ->
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

let ordre (G(n,_)) = n

let entry_point_sub (N(_,a,_)) = a

let entry_point (G(_,l)) = match l with
	(* Un premier point d'entrée (il n'y a pas de point privilégié) *)
	|[] -> failwith "Graphe vide"
	|h::_ -> entry_point_sub h

let n_eme_elmt (G(len,l)) n =
	(* Associe à une clé n l'unique élément dont c est la clé *)
	if n >= len then failwith "Clé non valide" else
	let rec aux k = function
		|[] -> failwith "Clé non valide"
		|h::_ when k = n -> entry_point_sub h
		|_::t -> aux (k + 1) t in
	aux 0 l

let rec degre (G(len,l)) p = match l with
	(* Degré du sommet p du graphe *)
  |[] -> failwith "Graphe vide"
  |N(_,a,l1)::_ when a = p -> List.length l1
  |_::t -> degre (G(len - 1, t)) p

let rec voisins (G(len,l)) p = match l with
	(* Liste des voisins du sommet p du graphe *)
  |[] -> failwith "Graphe vide"
  |N(_,a,l1)::_ when a = p -> List.map entry_point_sub l1
  |_::t -> voisins (G(len - 1, t)) p

let ajout_elmt a (G(n,l)) = G(n + 1, N(n, a, [])::l)
(* L'ordre des éléments n'a aucune importance, on peut donc ajouter au début de la liste *)

let ajout_arete depart arrivee (G(n,l)) =
	let rec aux elmt_depart elmt_arrivee r = function
		(* On empile les éléments dans r jusqu'à trouver celui de départ, on modifie l'élément et on l'ajoute
		au début de la liste des éléments (puisque l'ordre des éléments n'a aucune importance) formée de ceux que l'on n'a 
		visité et ceux que l'on a empilé dans r *)
		|[] -> failwith "Départ absent dans le graphe"
		|N(k,a,l1)::t when a = elmt_depart -> N(k,a,elmt_arrivee::l1)::(r@t)
		|h::t -> aux elmt_depart elmt_arrivee (h::r) t in
	
	let rec trouver_arrivee elmt_arrivee = function
		|[] -> failwith "Arrivée non présente dans le graphe"
		|h::_ when entry_point_sub h = elmt_arrivee -> h
		|_::t -> trouver_arrivee elmt_arrivee t in
	
	let elmt_arrivee = trouver_arrivee arrivee l in
	G(n,aux depart elmt_arrivee [] l)



let parcoursDFS f (G(n,g)) s0 = (* f est une procédure à appliquer au graphe, s0 la clé du point de départ *)
	if not (verif_graph (G(n,g))) then failwith "Graphe mal construit" else

	let n_eme_sub (G(len,l)) n = (* Associe à une clé n l'unique sous_graphe (élément) dont c est la clé *)
		if n >= len then failwith "Clé non valide" else
		let rec aux k = function
			|[] -> failwith "Clé non valide"
			|h::_ when k = n -> h
			|_::t -> aux (k + 1) t in
		aux 0 l in
	
	let visites = Array.make n false in
	
	let rec traitement s =
		let N(k,a,l) = s in
		if not visites.(k) then
		begin
			f a;
			print_newline();
			visites.(k) <- true;
			List.iter traitement l
		end
	in
	traitement (n_eme_sub (G(n,g)) s0)


let parcoursBFS f (G(n,g)) s0 = (* f est une procedure à appliquer au graphe, s0 la clé du point de départ *)
	if not (verif_graph (G(n,g))) then failwith "Graphe mal construit" else
	
	let n_eme_sub (G(len,l)) n = (* Associe à une clé n l'unique sous_graphe (élément) dont c est la clé *)
		if n >= len then failwith "Clé non valide" else
		let rec aux k = function
			|[] -> failwith "Clé non valide"
			|h::_ when k = n -> h
			|_::t -> aux (k + 1) t in
		aux 0 l in
	
	let visites = Array.make n false in
	let file = Queue.create() in
	
	let traitement s =
		let N(k,a,_) = s in
		if not visites.(k) then
		begin
			f a;
			print_newline();
			visites.(k) <- true;
			Queue.add s file;
		end
	in
	
	traitement (n_eme_sub (G(n,g)) s0) ;
	while not (Queue.is_empty file) do
		let s = Queue.take file in
		let N(_,_,l) = s in
		List.iter traitement l
	done