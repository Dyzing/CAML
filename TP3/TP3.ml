(*1*)
type 'a arbre1 = F of 'a | N of ('a arbre1 * 'a arbre1);; 

(*2*)
let arbre1 = N(
		N(F 1, F 2),
		N(F 3, F 4)
		);;
		
let arbre2 = N(
		N(F 12, F 24),
		N(F 30, F 41)
		);;		

let arbre3 = N(
			N(
				N(F 1, F 2),
				F 3
			),
			F 4
		);;
				

(*3*)

let rec compter_noeud a =
	match a with
		F x -> 0
		|N(a1, a2) -> 1 + (compter_noeud a1) + (compter_noeud a2);; 

compter_noeud arbre1;;

(*4*)

let rec compter_feuille a =
	match a with
		F x -> 1
		|N(a1, a2) ->(compter_feuille a1) + (compter_feuille a2);; 

compter_feuille arbre1;;

(*5*)
let max a b =
	if a > b
	then a 
	else b;;


let rec profondeur a = 
	match a with
		F x -> 1
		|N(a1, a2) -> 1 + max (profondeur a1) (profondeur a2);;

profondeur arbre1;;

(*6*)

let rec meme_arbre (tree1, tree2) = 
	match (tree1, tree2) with
		(F x1, F x2) -> true
		|(F x1, N(a3, a4)) -> false
		|(N(a1, a2), F x2) -> false 
		|(N(a1, a2), N(a3, a4)) -> true && meme_arbre(a1, a3) && meme_arbre(a2, a4);;
		
		
	
meme_arbre (arbre1, arbre2);;
meme_arbre (arbre1, arbre3);;
meme_arbre (arbre3, arbre3);;

(*7*)

let rec valeur_feuilles a = 
	match a with
		F x -> x::[]
		|N(a1, a2) -> (valeur_feuilles a1)@(valeur_feuilles a2);;

valeur_feuilles arbre1;;

(*8*)

let incremente x  = x+1;;

let rec map_arbre a f =
	match a with
		F x -> F (f x)
		|N(a1, a2) -> N(map_arbre a1 f , map_arbre a2 f);;
		
map_arbre arbre1 incremente;;
	
	
(*-----------------------EXERCICE 2----------------------*)
type operateur_bin = Mult | Add;;
type operateur_un = Moins;;
type arbre = Const of int
	| Var of string
	| Noeud1 of (operateur_un * arbre)
	| Noeud2 of (operateur_bin * arbre * arbre);;
	
let rec chaine_de_arbre e = 
	match e with 
		Const x -> string_of_int x
		|Var x -> x
		|Noeud1 (Moins,x) -> "-"^chaine_de_arbre x
		|Noeud2 (Mult, e1, e2) ->"("^chaine_de_arbre e1^"*"^chaine_de_arbre e2^")"
		|Noeud2 (Add, e1, e2) -> "("^chaine_de_arbre e1^"+"^chaine_de_arbre e2^")";;
	
	
let ope = Noeud2(Add, 
		Noeud2(Mult, 
			Var"x", Const 3), 
		Var "y");;
	
chaine_de_arbre ope;;
