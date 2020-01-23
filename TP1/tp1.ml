(* -------- exercie 1 ----------- *)

(*a*)

(**)
let ttc p = p *. 1.20;;

ttc 10.;;


(**)
let bissextile a = 
	let modu = a mod 4 in
		match modu with
		0 -> true
		|_ -> false;;

bissextile 2020;;
bissextile 2019;;

(**)
let minuscule c =
	if c<='z' && c>='a' then true
	else false;;
	
minuscule 'h';;
minuscule 'H';;

(*b*)

(**)
let moyenne a b = (a+.b)/.2.;;

moyenne 5. 15.;;

(**)
let quotient_reste a b = (a/b, a mod b);;
	
quotient_reste 50 4;;

(*c*)

(**)
let rec puissance4 a = 
	let puissance2 a = a*a in
		puissance2 (puissance2 a);;

puissance4 2;;

(**)
let upper c = 
	if minuscule c then char_of_int (int_of_char c + int_of_char 'A' - int_of_char 'a')
	else c;;
		
upper 'h';;

(*----------EXERCICE 2-------------*)

(*a*)
let rec fibo a = 
	match a with
	0 -> 0
	|1 -> 1
	|_ -> fibo (a-1) + fibo (a-2);;
	
fibo 8;;
		
(*b*)
let rec nsomme n = 
	match n with
	0 -> 0
	|_ -> n*n + nsomme (n-1);;
	
nsomme 4;;
		

(*----------EXERCICE 3------------*)

(*a*)

let rec sigma f n = 
	let res = 0 in 
		match n with
		0 -> res
		|_ -> res + f (n) + sigma f (n-1);;

let nsomme_sigma n =
	sigma (fun x -> x*x) n;;
	
nsomme_sigma 4;; 
		

(*b*)

let carre x = x*x;;

let rec rond f g x  = g (f x);;

rond carre nsomme_sigma 2;;


(*------------EXERCICE 3------------*)

(*a*)

let carref x = x*.x;;

let rec newton x y esp = 
	if (carref y < (x +.esp)) || (carref y > (x -. esp)) then y 
	else newton x ((y +. (x/.y))/.2.) esp;;
	
	
 

