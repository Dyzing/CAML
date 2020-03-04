(* Dans ce projet, je n'ai "fait" que l'algorithme 1 (kosaraju).  Je n'ai fait que quelques fonctions pour la deuxième partie. *)


(**PARTIE UNE**)
let graphe1 = [(1,[6;7;8]); (2,[1;4]); (3, [2]); (4, [3;5]); (5, [1]); (6, [5;7]); (7, []); (8, [6;7])];;


(**1**) (* Cette fonction retourne la liste de tous les sommets. Elle a pour argument un graphe et retourne les sommets nommés "ns" dans une liste. *)

let rec liste_sommet_graphe graphe =
	match graphe with
	[] -> []
	| (ns, li)::reste -> ns :: liste_sommet_graphe reste;;



(**2**) (*Cette fonction retounr la liste des successeurs d'un sommets donné. Elle prend pour argument un graphe et un "elem" qui est un sommet. Elle retourne une liste des successeurs "li" de elem.*)
let rec liste_succ graphe elem =
	match graphe with 
	[] -> failwith ("ce sommet n'existe pas")
	| (ns, li)::reste -> if ns = elem then li else liste_succ reste elem;;
	

(**3**) (*inverser graphe1;;*)
(* Cette fonction inverse un graphe. Elle prend pour argument un graphe. Elle retourne le graphe inversé.
Je parcours chaque noeud du graphe. Pour chaque noeud, je recherche ce même noeud dans la liste des successeurs de chaque autre noeud.
Dès que je trouve ce noeud dans la liste des successeurs d'un autre noeud, je rentre dans une liste cet autre noeud.
Je fais ceci sur chaque noeuds du graphe. Et je retourne la liste des sommets avec leur liste de prédecesseurs (ns,[li]) dans "liste_finale".
 *)



let inverser graphe = (**fun**)
	let liste_finale = [] in 
	let graphe_de_base = graphe in
	
		let rec rec_inverser graphe liste_finale  = (**fun**) (* liste_finale est la liste du résultat de l'inverse de "graphe" *)
			match graphe with
				[] -> liste_finale
				|(ns1,li1)::reste1 -> let liste_succ_de_ns1 = [] in
				
				
						 let rec passer_au_noeud2_suivant graphe liste_succ_de_ns1 = (**fun**)  (* liste_succ_de_ns1 est la liste des successeurs du sommet actuel ns1 *)
								match graphe with
									[] -> rec_inverser reste1 (liste_finale@((ns1,liste_succ_de_ns1)::[])) 
									|(ns2,li2)::reste2 ->	if(ns2 = ns1)
												then passer_au_noeud2_suivant reste2 liste_succ_de_ns1	
												else let lisucc = liste_succ graphe ns2 in
												
												
													let rec passer_a_element2_suivant lisucc liste_succ_de_ns1  = (**fun**) (* lisucc est la liste des successeurs du noeud ns2 qui est celui dont on cherche si un de ses successeurs est ns1 *)
														match lisucc with
															[] -> passer_au_noeud2_suivant reste2 liste_succ_de_ns1
															|e2::r2 ->	if (e2 = ns1) 
																  	then passer_au_noeud2_suivant reste2 (liste_succ_de_ns1@(ns2::[])) 
																  	else passer_a_element2_suivant r2 liste_succ_de_ns1
																  	
																  	 
												  	in passer_a_element2_suivant lisucc liste_succ_de_ns1 
						 in passer_au_noeud2_suivant graphe_de_base liste_succ_de_ns1
		in rec_inverser graphe liste_finale ;;
		
		
(**4**)	(* parcours profondeur *)	 
(* Cette fonction effectue le parcours en profondeur du graphe.
Il retourne la liste des sommets parcouru dans l'ordre. Il prend en argument un graphe *)


let parcourir e liparcouru = (* Cette fonction fait rentrer dans une liste "liparcouru" le sommet "e" *)
	match liparcouru with
		[] -> e::liparcouru
		|ns::reste -> e::liparcouru;;
		
		
let rec estparcouru e liparcouru = (* Cette fonction fait verifie si un sommet "e" est dans une liste "liparcouru" *)
	match liparcouru with
		[] -> false
		|ns::reste -> if ns=e then true
				    else estparcouru e reste;;				    
				    
				    
let rec ajout_succ_de_succ liparcouru graphe elementsucc liste_succ_de_succ = (* Cette fonction ajoute les successeurs des successeurs d'un sommet du graphe dans la liste des parcourus du parcours profondeur *) (* "liparcouru" est la liste des parcouru du parcours profondeur, "graphe" est le graphe, "elementsucc" est un sucesseur d'un sommet du graphe, "liste_suc_de_succ" est la liste des sucesseurs des successeurs d'un sommet du graphe *)
	match liste_succ_de_succ with
  		[] -> parcourir elementsucc liparcouru
		|elementsuccdesucc::restesuccdesucc -> if(estparcouru elementsuccdesucc liparcouru)
						       then ajout_succ_de_succ liparcouru graphe elementsucc restesuccdesucc
						       else ajout_succ_de_succ (parcourir elementsuccdesucc liparcouru) graphe elementsucc (restesuccdesucc) ;;			
					
							
let rec ajout_de_succ lisucc ns liparcouru graphe = (* Cette fonction fait appel à "ajout_succ_de_succ" depuis un successeur d'un sommet du graphe *) (* "lisucc" est la liste des successeurs d'un sommet du graphe , "graphe" est le graphe, "ns" est un sommet du graphe, "liparcouru"  est la liste des parcouru du parcours profondeur *)
		match lisucc with 
			[] -> parcourir ns liparcouru
			|elementsucc::restesucc -> if (estparcouru elementsucc liparcouru)
						   then ajout_de_succ restesucc ns liparcouru graphe
				   		   else (let liste_succ_de_succ = liste_succ graphe elementsucc in
				   			ajout_de_succ restesucc ns (ajout_succ_de_succ liparcouru graphe elementsucc liste_succ_de_succ) graphe);;			    


				    
let rec rec_parcours_prof graphe liparcouru =  (*Cette fonction parcours le graphe et fait appel à "ajout_de_succ". Elle prend comme argument un graphe et une "liparcouru" qui est la liste des parcouru du parcours profondeur initialisée dans la fonction "parcours_prof". Elle retourne la liste des sommets parcourus selon le parcours profondeur. *)
	match graphe with
		[] -> liparcouru
		|(ns,li)::reste -> if (estparcouru ns liparcouru)
				   then rec_parcours_prof reste liparcouru
				   else 
					(let lisucc = liste_succ graphe ns in 
						rec_parcours_prof reste (ajout_de_succ lisucc ns liparcouru graphe));;
						
						

let parcours_prof graphe = (*Cette fonction lance le parcours profondeur et prend un graphe en argument. *)
	let liparcouru = [] in
		rec_parcours_prof graphe liparcouru;;
				    
				    
(**5**)	(*Cette fonction recherche les composantes fortements connexes dans un graphe en argument. Elle fait le parcours profondeur de l'inverse d'un graphe et retourne la liste des composantes fortements connexes. *)			    
(* kosaraju - connexe *)				    

let rec connexe graphe = parcours_prof (inverser graphe1);;
		

(** tests **) (* Ce sont les tests/appels des fonctions demandées. *)

liste_sommet_graphe graphe1;;

liste_sommet_graphe graphe1;;

liste_succ graphe1 6;;
			
parcours_prof graphe1;;	

inverser graphe1;;		

connexe graphe1;;




(** PARTIE 2 TARJAN **)


let rec pop pile = (* dépile la tête de pile et prend une pile en argument pour retourner une pile *)
	match pile with
		[] -> []
		|x::[] -> []
		|x::r -> x::(pop r);;
		
let rec tail pile = (* donne le dernier élément de la pile (le tail), prend en argument une pile et retourne uen pile *)
	match pile with
		[] -> failwith "pile vide"
		|x::[] -> x
		|x::r -> tail r;;
		
let empiler pile e = (* empile un élément e dans une pile et retourne la pile. *)
	pile@[e];;
	
let depiler pile e = (* dépile tous les éléments de la pile jusqu'à l'élément e, listedepile est la liste des éléments dépilés. *)
	let rec depiler_rec pile e listedepile = 
		match pile with 
			[] -> (listedepile, pile)
			|_ -> if tail pile = e 
				then (e::listedepile, pop pile)
				else depiler_rec (pop pile) e (tail pile::listedepile)
	in depiler_rec pile e [];;
				
