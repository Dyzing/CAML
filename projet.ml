(**PARTIE UNE**)
let graphe1 = [(1,[6;7;8]); (2,[1;4]); (3, [2]); (4, [3;5]); (5, [1]); (6, [5;7]); (7, []); (8, [6;7])];;

(**1**)
let rec liste_sommet_graphe graphe =
	match graphe with
	[] -> []
	| (ns, li)::reste -> ns :: liste_sommet_graphe reste;;
	
(*liste_sommet_graphe graphe1;;*)

(**2**)
let rec liste_succ graphe elem =
	match graphe with 
	[] -> failwith ("ce sommet n'existe pas")
	| (ns, li)::reste -> if ns = elem then li else liste_succ reste elem;;
	
(*liste_succ graphe1 1;;*)

(**3**)
(**
let rec ajoutelem graphe elem = 
	match graphe,elem with
	[],_ -> elem::graphe
	| (x,l)::reste, (ns,li) -> 
		if x = ns 
		then (x,l@li)::reste
		else (x,l) :: ajoutelem reste elem;;
	
let rec combiner g1 g2 =
	List.fold_left (ajoutelem) g1 g2;;
	
let rec inverser graphe =
	List.fold_left (fun base (s,lisuc) -> 
		combiner base  (List.fold_left (fun base2 ei -> ajoutelem base2 (ei,[s]) ) [] lisuc)
	) [] graphe;;

**)



(*inverser graphe1;;*)
	
(**4**)

(**
let rec appartient li e =
	match li with
	[] -> false
	| x :: r -> if e = x then true else appartient r e;;

let sommet noeud =
	match noeud with
	(x,li) -> x;;

let premierNoeud graphe =
	match graphe with
	[] -> failwith "pas de premier Noeud"
	| x::_ -> x;;

let rec getNode graphe sommet =
	match graphe with
	[] -> failwith "pas de sommet"
	| (s,li)::reste -> if s = sommet then (s,li) else getNode reste sommet;;

let parcours graphe = 
	let rec visite listeDejaVisit (s,li) =
		if appartient listeDejaVisit s
			then listeDejaVisit
			else List.fold_left (fun base ei -> visite (s::base) (getNode graphe ei)) listeDejaVisit li 
	in visite [] (getNode graphe 2);;
parcours graphe1;;

**)

let parcourir e liparcouru =
	match liparcouru with
		[] -> e::liparcouru
		|ns::reste -> e::liparcouru;;
		
		
let rec estparcouru e liparcouru =
	match liparcouru with
		[] -> false
		|ns::reste -> if ns=e then true
				    else estparcouru e reste;;				    
				    
				    
let rec ajout_succ_de_succ liparcouru graphe elementsucc liste_succ_de_succ =
	match liste_succ_de_succ with
  		[] -> parcourir elementsucc liparcouru
		|elementsuccdesucc::restesuccdesucc -> if(estparcouru elementsuccdesucc liparcouru)
						       then ajout_succ_de_succ liparcouru graphe elementsucc restesuccdesucc
						       else ajout_succ_de_succ (parcourir elementsuccdesucc liparcouru) graphe elementsucc (restesuccdesucc) ;;			
					
							
let rec ajout_de_succ lisucc ns liparcouru graphe = 
		match lisucc with 
			[] -> parcourir ns liparcouru
			|elementsucc::restesucc -> if (estparcouru elementsucc liparcouru)
						   then ajout_de_succ restesucc ns liparcouru graphe
				   		   else (let liste_succ_de_succ = liste_succ graphe elementsucc in
				   			ajout_de_succ restesucc ns (ajout_succ_de_succ liparcouru graphe elementsucc liste_succ_de_succ) graphe);;			    


				    
let rec rec_parcours_prof graphe liparcouru = 
	match graphe with
		[] -> liparcouru
		|(ns,li)::reste -> if (estparcouru ns liparcouru)
				   then rec_parcours_prof reste liparcouru
				   else 
					(let lisucc = liste_succ graphe ns in 
						rec_parcours_prof reste (ajout_de_succ lisucc ns liparcouru graphe));;
						
						

let parcours_prof graphe = 
	let liparcouru = [] in
		rec_parcours_prof graphe liparcouru;;
				    
		



let inverser graphe = (**fun**)
	let liste_finale = [] in 
	let graphe_de_base = graphe in
	
		let rec rec_inverser graphe liste_finale  = (**fun**)
			match graphe with
				[] -> liste_finale
				|(ns1,li1)::reste1 -> let liste_succ_de_ns1 = [] in
				
				
						 let rec passer_au_noeud2_suivant graphe liste_succ_de_ns1 = (**fun**)
								match graphe with
									[] -> rec_inverser reste1 (liste_finale@((ns1,liste_succ_de_ns1)::[])) 
									|(ns2,li2)::reste2 ->	if(ns2 = ns1)
												then passer_au_noeud2_suivant reste2 liste_succ_de_ns1	
												else let lisucc = liste_succ graphe ns2 in
												
												
													let rec passer_a_element2_suivant lisucc liste_succ_de_ns1  = (**fun**)
														match lisucc with
															[] -> passer_au_noeud2_suivant reste2 liste_succ_de_ns1
															|e2::r2 ->	if (e2 = ns1) 
																  	then passer_au_noeud2_suivant reste2 (liste_succ_de_ns1@(ns2::[])) 
																  	else passer_a_element2_suivant r2 liste_succ_de_ns1
																  	
																  	 
												  	in passer_a_element2_suivant lisucc liste_succ_de_ns1 
						 in passer_au_noeud2_suivant graphe_de_base liste_succ_de_ns1
		in rec_inverser graphe liste_finale ;;
		
		
		



			
parcours_prof graphe1;;	

inverser graphe1;;		

inverser graphe1;;		


let rec kosaraju graphe = parcours_prof (inverser graphe1);;

kosaraju graphe1;;

