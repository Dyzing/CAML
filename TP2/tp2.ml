(* -------- exercie 1 ----------- *)

(*1*)

let rec longueur li = 
	match li with
		[] -> 0
		|_::r -> 1 + longueur r;;
longueur [1; 2; 3];;

(*2*)

let rec concat li lo =
	match li with
		[] -> lo
		|e::r -> e::(concat r lo);;
concat [1; 2; 3] [4; 5; 6];;

(*3*)

let rec nieme x li =
	match li with 
		[] -> failwith "nombre trop grand"
		|e::r -> if (x = 0) then e else nieme (x-1) r;;
nieme 0 [1; 2; 3; 4];;


(* -------- exercie 2 ----------- *)

(*1*)

let rec npremiers n li =
	match (n,li) with
		(0,_) -> []
		|(_,[]) -> failwith "nombre trop grand"
		|(_,e::r) -> e::(npremiers (n-1) r);; 
npremiers 3 [1; 2; 3; 4; 5];;

(*2*)

let rec met_a_plat li = 
	match li with
		[] -> []
		|e::r -> e@(met_a_plat r);;
met_a_plat [[1;2;3];[4;5;6];[7;8;9]];;

(*3*)

let rec paire_vers_liste (li,lo) = 
	match (li,lo) with
		([],[]) -> []
		|(x::y,[]) -> failwith "liste de differentes tailles"
		|([],e::r) -> failwith "liste de differentes tailles"
		|(x::y, e::r) -> (x,e)::(paire_vers_liste (y,r));;
paire_vers_liste ([1;2;3],[4;5;6]);;

(*4*)
let transformation (x,y) li lo =
	match (li,lo) with
		_ -> (x::li, y::lo);;


let  liste_vers_paire li = 
	match li with
		e::r -> transformation e [] [];;
		
liste_vers_paire [(1,2)];;
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
