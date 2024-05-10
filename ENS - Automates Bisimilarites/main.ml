(*

Automates Bisimilarités

*)


(* Définition de constantes globales données par le sujet *)
let max_u = 99999
let u_tab = Array.make max_u (-1)
let u_0 = 1
let u_def_index = ref 0
let a = 1103515245
let c = 12345
let m = 0b1000000000000000

(* Gestion du caractère Pseudo-Aléatoire via le tableau u_tab, Interface avec get_u *)

let get_u i =
	if (i >= max_u) then failwith "get_u -> Outside of Range!\n"
	else if (i <= !u_def_index) then u_tab.(i) else
	begin
		for k = !u_def_index + 1 to i do
			u_tab.(k) <- (a * u_tab.(k-1) + c) mod m
		done;
		u_def_index := i;
		u_tab.(i)
	end 


(* Question 1 *)

let q_1 i = 
		Printf.printf "   * Question 1 : u_%d = %d \n" i (get_u i)


(* Définition des types Automates, Alphabet, ...*)
(* Il n'est en soi pas nécessaire de développer ainsi, mais ceci rend plus clair le debug / verbose*)

type alphabet_t = int	(* Assimilé a [|0, n|]*)
type etat_t = int (* Assimilé a [|0, n|]*)
type transition_t = alphabet_t * etat_t
type list_transition_t = transition_t list array (* Chaque état possède sa liste de transitions, le sujet souhaite aborder la question des transition sous la forme de liste de triplets plutôt qu'en fonctionnel *)
type etat_init_t = etat_t list
type etat_fin_t = etat_t list
type automate_t = etat_t * alphabet_t * list_transition_t * etat_init_t * etat_fin_t


let transition_identity (t : transition_t) : transition_t = t

let transition_get_letter (t : transition_t) : alphabet_t = match t with
	|(l, n) -> l

let transition_get_next (t : transition_t): etat_t = match t with
	|(l, n) -> n


let aut_identity (a : automate_t) : automate_t = a

let aut_get_states (a : automate_t) : etat_t = match a with
	|(q, sigma, delta, i, f) -> q

let aut_get_alphabet (a : automate_t) : alphabet_t = match a with
	|(q, sigma, delta, i, f) -> sigma

let aut_get_trans_arr (a : automate_t) : list_transition_t = match a with
	|(q, sigma, delta, i, f) -> delta

let aut_get_init_state (a : automate_t) : etat_init_t = match a with
	|(q, sigma, delta, i, f) -> i

let aut_get_fin_state (a : automate_t) : etat_fin_t = match a with
	|(q, sigma, delta, i, f) -> f


type mot_t = alphabet_t list


let x (i : int) (j: int) (s: int) : int = 
	(271 * (get_u i) + 293 * (get_u j) + 283 * (get_u s)) mod 10000


let aut_gen_random (t : int) (n : int) (m : int) (d: int) : automate_t = 
	let transitions = Array.make n [] in
	
	let mod_expr n m d i = Float.to_int ((Float.of_int (n * n * m)) /. (Float.of_int (d * (n - i + 1))) +. 1.0) in
	
	let rec make_transition_list base_state letter new_state buffer_list =
		if (base_state >= n) then ()
		else if (new_state >= n) then begin transitions.(base_state) <- buffer_list; make_transition_list (base_state + 1) 0 0 [] end
		else if (letter >= m) then make_transition_list base_state 0 (new_state + 1) buffer_list
		else if ( ((get_u (10 * t + (x base_state new_state letter))) mod (mod_expr n m d (base_state))) = 0 )
			then make_transition_list base_state (letter + 1) new_state ((letter, new_state)::buffer_list)
		else make_transition_list base_state (letter + 1) new_state buffer_list

	in make_transition_list 0 0 0 [];
	(n, m, transitions, [0], [n-1])


(* Question 2 *)
let q_2 (t : int) (n : int) (m : int) (d : int) : unit = 
	let rand_aut = aut_gen_random t n m d in
	let trans_arr = aut_get_trans_arr rand_aut in

	let rec count_transitions count_buf cur_state trans_list = match trans_list with
		|[] -> if (cur_state >= n - 1) then count_buf else count_transitions count_buf (cur_state + 1) (trans_arr.(cur_state + 1))
		|h::t -> count_transitions (count_buf + 1) cur_state t

	in
	Printf.printf "   * Question 2 : A_%d(%d, %d, %d) contient %d transitions\n" t n m d (count_transitions 0 (-1) [])



(* 
	La présentation des automates avec liste de transitions nous invite à travailler avec des automates sous forme de Graphe 
	Il semble alors adapté de raisonner avec l'algorithmie des graphes usuelle. Les quelques fonctions suivantes ont pour effet de "transformer" un automate en graphe 	 
*)

type graphe_t = int * list_transition_t (* Graphe sous forme de Liste d'adjacence, 'pondéré' par les lettres *)

let graphe_identity (g : graphe_t) : graphe_t = g

let graphe_get_n (g : graphe_t) : int = match g with
	|(n, t) -> n

let graphe_get_transitions (g : graphe_t) : list_transition_t = match g with
	|(n, t) -> t

let aut_to_graph (a : automate_t) : graphe_t = match a with
	|(q, sigma, delta, i, f) -> (q, delta)


let graphe_etats_accessibles_from_i (g : graphe_t) (i : int) : bool array =  (* Renvoie les états accessibles depuis i *)
	let n, t = graphe_identity g in
	let tab_access = Array.make n false in

	let rec pp_recursif s = 
		tab_access.(s) <- true;
		parcours_voisins_list (t.(s))

	and parcours_voisins_list l = match l with
			|[] -> ()
			|head::tail -> if(not tab_access.(transition_get_next head)) then (pp_recursif (transition_get_next head)); parcours_voisins_list tail


	in pp_recursif i; tab_access


let aut_get_etats_accessibles (a : automate_t) : bool array =
		graphe_etats_accessibles_from_i (aut_to_graph a) 0 

(* Question 3 *)

let q_3 (t : int) (n : int) (m : int) (d : int) = 
	let rand_aut = aut_gen_random t n m d in
	let access_tab = aut_get_etats_accessibles rand_aut in
	Printf.printf "   * Question 3 : A_%d(%d, %d, %d) contient %d états accessibles\n" t n m d (Array.fold_left (fun count b -> if (b) then (count + 1) else count) 0 access_tab)


let _ =
	u_tab.(0) <- u_0;

	(* q_1 2; q_1 10; q_1 1000; q_1 11000; *)	(* Ok avec u_0 = 1 *)

	(* q_2 1 10 5 3; q_2 2 100 10 20; q_2 3 900 20 200; *) (* Ok avec u_0 = 1 *)

	(*q_3 1 10 5 3; q_3 2 100 10 5; q_3 3 200 10 6; q_3 4 1000 25 5;*) (* Ok avec u_0 = 1 *)