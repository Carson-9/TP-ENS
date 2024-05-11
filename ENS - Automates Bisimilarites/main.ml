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


let aut_affiche (a : automate_t) : unit = match a with
	|(q, sigma, delta, i, f) ->
		Printf.printf "Automate : q = %d, Sigma = %d : \n" q sigma;

		Printf.printf "États Initiaux : "; List.iter (fun i -> Printf.printf "%d " i) i; 
		Printf.printf "\nÉtats Terminaux : "; List.iter (fun i -> Printf.printf "%d " i) f; print_newline ();
		
		Array.iteri (fun i li -> List.iter (fun (l, n) -> Printf.printf "%d ->_%d %d" i l n) li; print_newline ()) delta



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



(*
	
	Les questions suivantes peuvent se traiter de plusieurs manières comme le suggère l'énoncé.
	Cependant, une idée semblant ressortir de ces trois questions est l'intersection de langages / automate produit : Nous pouvons
	en effet répondre à ces trois questions en considérant qu'une trace appartient à un langage si et seulement si l'intersection
	entre ce langage et le langage reconnu par l'automate est non vide.

*)

(*

	Le langage \phi est régulier, et reconnu par un automate que nous donnons ci-dessous (m = 10)

*)

let automate_phi_const : automate_t = 
	(6, 10, 
	[| [(0, 0); (1, 1); (1, 4); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7, 0); (8, 0); (9, 0)]; 
	((List.init 10 (fun i -> (i, 2))) @ (List.init 10 (fun i -> (i, 3))) @ (List.init 10 (fun i -> (i, 4)))); 
	(List.init 10 (fun i -> (i, 3))); 
	(List.init 10 (fun i -> (i, 4))); 
	[(2, 5)];  
	[]|], [0], [5])


let list_prod_fun (la : 'a list) (lb : 'b list) (f : 'a -> 'a -> 'b): 'b list =
	let rec recursion a b = match (a, b) with
		|(_ , []) -> []
		|([], h::t) -> recursion la t
		|(ha::ta, hb::tb) -> (f ha hb)::(recursion ta b)
	in recursion la lb

let aut_get_voisins (a: automate_t) (q : etat_t) (l : alphabet_t) : etat_t list = match a with
	|(aq, sigma, delta, i, f) -> if(q >= aq || q < 0) then failwith "aut_get_voisins -> Cet état est inconnu!"
	else let transition_list = delta.(q) in List.filter_map (fun (vl, n) -> if (vl <> l) then None else (Some n)) transition_list

let aut_prod (a : automate_t) (b : automate_t) : automate_t = match (a, b) with
	|((aq, asigma, adelta, ai, af), (bq, bsigma, bdelta, bi, bf)) ->
		if(asigma <> bsigma) then failwith "aut_prod -> Les deux alphabets mis en jeu diffèrent!"
		else begin 
			let new_q = aq * bq in
			let transition_tab = Array.make new_q [] in

			let rec build_transitions a_ind b_ind letter = 
				if(a_ind >= aq) then ()
				else if(b_ind >= bq) then build_transitions (a_ind + 1) 0 0
				else if(letter >= asigma) then build_transitions a_ind (b_ind + 1) 0
				else begin 
					let a_voisins = (aut_get_voisins a a_ind letter) in 
					let b_voisins = (aut_get_voisins b b_ind letter) in

					let rec build_transitions_prod_list la lb build_list = match (la, lb) with
						|(_, []) -> transition_tab.(a_ind * bq + b_ind) <- (build_list @ transition_tab.(a_ind * bq + b_ind))
						|([], h::t) -> build_transitions_prod_list a_voisins t build_list
						|(ha::ta, hb::tb) -> build_transitions_prod_list ta lb ((letter, (ha * bq + hb))::build_list)

					in build_transitions_prod_list a_voisins b_voisins []; build_transitions a_ind b_ind (letter + 1)
				end
			in build_transitions 0 0 0; 
			(new_q, asigma, transition_tab, (list_prod_fun ai bi (fun a b -> a * bq + b)), (list_prod_fun af bf (fun a b -> a * bq + b)))
		end


let aut_is_language_empty (a : automate_t) : bool = match a with
	|(q, sigma, delta, i, f) -> let g = aut_to_graph a in

		let rec find_acceptant l accessibility_tab = match l with
			|[] -> false
			|ha::ta -> (accessibility_tab.(ha)) || (find_acceptant ta accessibility_tab) (* Évaluation parresseuse *)

		in let rec iter_initial l = match l with
			|[] -> false
			|h::t -> let access_tab = graphe_etats_accessibles_from_i g h in (find_acceptant f access_tab) || (iter_initial t) 
				
	in not (iter_initial i) (* Not car afin de profiter de l'évaluation parresseuse, nous nous arrêtons lorsque vrai est trouvé, ce qui correspond à un langage non vide*)

(* Question 4 *)

let q_4 n d t = 

	let rec build_t_list t_val =
		if (t_val >= t+1) then []
		else begin
			let cur_automate = aut_gen_random t_val n 10 d in
				if(not (aut_is_language_empty (aut_prod cur_automate automate_phi_const)))
					then t_val::(build_t_list (t_val + 1)) else (build_t_list (t_val + 1))
			end

	in let rec find_min_card_list l = match l with
		|[] -> (Int.max_int, 0)
		|h::tail -> let m, s = (find_min_card_list tail) in if (h < m) then (h, s+1) else (m, s+1)
		
		
	in let t_list = build_t_list 1 in let min, size = find_min_card_list t_list in
	Printf.printf "   * Question 4 : Pour n = %d, d = %d, T = %d, Nous obtenons min = %d et Card = %d\n" n d t min size


(* Question 5 *)

let q_5 n d = 
	let rec build_t_list t = 
		if (t > 100) then []
		else begin 
			let aut_a = aut_gen_random t n 10 d in let aut_b = aut_gen_random (t+1) n 10 d in
			if(not (aut_is_language_empty (aut_prod aut_a aut_b))) then t::(build_t_list (t+1)) else (build_t_list (t+1)) 
		end

	in let rec find_min_card_list l = match l with
		|[] -> (Int.max_int, 0)
		|h::tail -> let m, s = (find_min_card_list tail) in if (h < m) then (h, s+1) else (m, s+1)
	
	in let t_list = build_t_list 1 in let min, size = find_min_card_list t_list in
	Printf.printf "   * Question 5 : Pour n = %d, d = %d, Nous obtenons min = %d et Card = %d\n" n d min size

(* Question 6 *)
let q_6 k = 

	let rec build_aut t k_param = if (k_param <= 0) then aut_gen_random t 10 2 5
	else (aut_prod (aut_gen_random t 10 2 5) (build_aut (t+1) (k_param-1)))

	in let rec build_t_list t = 
		if (t > 10) then []
		else begin 
			let aut = build_aut (5 * t) k in
			if(not (aut_is_language_empty (aut))) then t::(build_t_list (t+1)) else (build_t_list (t+1)) 
		end

	in let rec find_min_card_list l = match l with
		|[] -> (Int.max_int, 0)
		|h::tail -> let m, s = (find_min_card_list tail) in if (h < m) then (h, s+1) else (m, s+1)
	
	in let t_list = build_t_list 1 in let min, size = find_min_card_list t_list in
	Printf.printf "   * Question 6 : Pour k = %d, Nous obtenons min = %d et Card = %d\n" k min size





(* ------------------ PARTIE 2 ------------------------ *)


(* On remarque une similitude avec l'équivalence de Nérode, servant à trouver un automate minimal *)

type state_partition_t = etat_t list list

let list_is_empty (l : 'a list) : bool = match l with |[] -> true |_ -> false 
let rec list_contains (l : 'a list) (e : 'a) : bool = match l with
	|[] -> false
	|h::t -> (e = h) || (list_contains t e)

let list_filter_out (l : 'a list) (filter : 'a list) : 'a list = 
	if(list_is_empty filter) then l else	(* PLus rapide sur filtre vide *)
	let rec recursion la = match la with
		|[] -> []
		|h::t -> if(list_contains filter h) then (recursion t) else h::(recursion t)
	in recursion l

let list_split_on (l : 'a list) (filter : 'a list) : ('a list * 'a list) = 
	if(list_is_empty filter) then ([], l) else  (* PLus rapide sur filtre vide *)
	let rec recursion la = match la with
		|[] -> ([], [])
		|h::t -> let inside, outside = (recursion t) in if (list_contains filter h) then ((h::inside), outside) else (inside, (h::outside))
	in recursion l

let aut_trivial_partition (a : automate_t) : state_partition_t = [(List.init (aut_get_states a) (fun i -> i))]
let aut_basic_nerode_partition (a : automate_t) : state_partition_t = match a with
	|(q, sigma, delta, i, f) -> [list_filter_out (List.init (aut_get_states a) (fun i -> i)) f; f]

let aut_predecessor (a : automate_t) (state_list: etat_t list) (letter : alphabet_t) : etat_t list = match a with
	|(q, sigma, delta, i, f) -> 
		let rec build_predecessor_list state build_list = 
			if(state >= q) then build_list 
			else begin 
				let voisins = aut_get_voisins a state letter in
				let in_v, out_v = list_split_on voisins state_list in
				if (not (list_is_empty in_v)) then build_predecessor_list (state + 1) (state::build_list)
				else build_predecessor_list (state + 1) build_list
			end
		in build_predecessor_list 0 []


let aut_algorithme_5 (a : automate_t) : state_partition_t = match a with
	|(q, sigma, delta, i, f) ->
		let base_partition = aut_basic_nerode_partition a in

		let rec find_letter_class_to_split_on cur_class partition letter = match partition with
			|[] -> ([], [])
			|h::t -> begin if(letter >= sigma) then (find_letter_class_to_split_on cur_class t 0)
				else begin 
					let in_class, out_class = list_split_on cur_class (aut_predecessor a h letter) in
					if( (not (list_is_empty in_class)) && (not (list_is_empty out_class)) ) then (in_class, out_class)
					else find_letter_class_to_split_on cur_class partition (letter + 1) 
				end
			end


		in let rec find_suitable_classes current_partition =
			let rec iter_through_partitions cur_part previous_list = match cur_part with
				|[] -> current_partition
				|h::t -> let in_class, out_class = find_letter_class_to_split_on h current_partition 0 in
				if(not (list_is_empty in_class)) then (find_suitable_classes (in_class::out_class::(previous_list @ t))) else (iter_through_partitions t (h::previous_list))
			in iter_through_partitions current_partition [] 
		in find_suitable_classes base_partition


(* Question 7 *)

let q_7 t n m d = 
	Printf.printf "   * Question 7 : La partition de A_%d(%d, %d, %d) donne %d classes d'équivalence\n" t n m d (List.length (aut_algorithme_5 (aut_gen_random t n m d)))


let rec list_insert_sans_doublon (l :'a list) (e : 'a) : 'a list = match l with
	|[] -> [e]
	|h::t -> if(h = e) then l else h::(list_insert_sans_doublon t e)

let y (i : etat_t) (j : etat_t) (lettre: alphabet_t) = 41 * i + 31 * j + lettre


let aut_gen_random_errone (t : int) (n : int) (m : int) (d : int) (p : int) =
	let a_base = aut_gen_random t n m d in match a_base with
		|(q, sigma, delta, i, f) -> 
			let new_transitions = Array.make q [] in


			(* Attention, il faut cette fois retirer les doublons éventuels !*)

			let rec build_transitions cur_state delta_list new_list = match delta_list with
				|[] -> new_transitions.(cur_state) <- new_list
				|head::tail -> let letter, next = transition_identity head in 
					if((get_u (5 * t + (y cur_state next letter))) mod p = 0) then 
						build_transitions cur_state tail (list_insert_sans_doublon new_list (((get_u (5*t + (y cur_state next letter) + 100)) mod m), next))
					else build_transitions cur_state tail (list_insert_sans_doublon new_list head)

			in let rec iterate_state s = if (s >= q) then () else begin build_transitions s (delta.(s)) []; iterate_state (s+1) end
			in iterate_state 0; (q, sigma, new_transitions, i, f) 

(* Question 8 *)

let q_8 (t : int) (n : int) (m : int) (d : int) (p : int) : unit = 
	let rand_aut = aut_gen_random_errone t n m d p in
	let trans_arr = aut_get_trans_arr rand_aut in

	let rec count_transitions count_buf cur_state trans_list = match trans_list with
		|[] -> if (cur_state >= n - 1) then count_buf else count_transitions count_buf (cur_state + 1) (trans_arr.(cur_state + 1))
		|h::t -> count_transitions (count_buf + 1) cur_state t

	in
	Printf.printf "   * Question 8 : A^{err(%d)}_%d(%d, %d, %d) contient %d transitions\n" p t n m d (count_transitions 0 (-1) [])


let aut_offset (a : automate_t) (s : int) : automate_t = match a with
	|(q, sigma, delta, i, f) ->
	let na = Array.init q (fun l -> (List.map (fun (letter, next) -> (letter, next + s)) (delta.(l)))) in
		(q + s, sigma, na, (List.map (fun k -> k+s) i), (List.map (fun k -> k+s) f))

let aut_union (a : automate_t) (b : automate_t) : automate_t = match (a,b) with
	|((aq, asigma, adelta, ai, af), (bq, bsigma, bdelta, bi, bf)) ->
		let new_b = aut_offset b aq in
		(aq + bq, 
		(max asigma bsigma), 
		(Array.init (aq + bq) (fun i -> if (i < aq) then adelta.(i) else (aut_get_trans_arr new_b).(i - aq))),
		 ai @ (aut_get_init_state new_b), 
		 af @ (aut_get_fin_state new_b))


(* Question 9 *)

let q_9 (n : int) (m : int) (d : int) (p : int) : unit = 

	let rec find_class_of (l : state_partition_t) (a : etat_t) : etat_t list = match l with
		|[] -> failwith "find_class_of -> La partition ne contient pas l'état souhaité"
		|h::t -> if(list_contains h a) then h else (find_class_of t a)

	in let partition_same_class (l : state_partition_t) (a : etat_t) (b : etat_t) : bool = 
		list_contains (find_class_of l a) b

	in let rec build_t_list cur_t = 
		if (cur_t > 100) then []
		else begin
			let rand_aut = aut_gen_random cur_t n m d in
			let rand_aut_error = aut_gen_random_errone cur_t n m d p in
			let union = aut_union rand_aut rand_aut_error in
			let union_partition = aut_algorithme_5 union in
			if(partition_same_class union_partition 0 (aut_get_states rand_aut)) (* Les automates considérés pour l'union ne possèdent qu'un unique état d'entrée par hypothèse *)
				then cur_t::(build_t_list (cur_t + 1)) else build_t_list (cur_t + 1)
			end
	in let rec find_min_card_list l = match l with
		|[] -> (Int.max_int, 0)
		|h::tail -> let m, s = (find_min_card_list tail) in if (h < m) then (h, s+1) else (m, s+1)

	in let t_list = build_t_list 1 in let min, size = find_min_card_list t_list in
	Printf.printf "   * Question 9 : Pour p = %d, n = %d, m = %d, d = %d, Nous obtenons min = %d et Card = %d\n" p n m d min size




(* ----------------------- PARTIE 3 ----------------------- *)

(*
	Reconnaître des inclusions de langages se fait assez facilement via des intersections. En notant U = L(A) et V = L(B),
	nous avons U \subset V \iff (U \cap V^c) = \emptyset. V^c se reconnait facilement en inversant les états de sortie
		Attention néanmoins, il faut que B soit complet!
*)


let aut_invert (a : automate_t) : automate_t = match a with	
	|(q, sigma, delta, i, f) ->
		let base_list = List.init q (fun i -> i) in
		(q, sigma, delta, i, (list_filter_out base_list f))


let aut_complete (a : automate_t) : automate_t = match a with
	|(q, sigma, delta, i, f) ->
		let new_arr = Array.init (q+1) (fun i -> if (i < q) then delta.(i) else (List.init sigma (fun l -> (l, q)))) in
		
		let base_letter_list = List.init sigma (fun i -> i) in
		
		let rec add_bot_transitions cur_state =
			if (cur_state >= q+1) then () else begin
			let rec find_cur_transitions_letter_list tr_list build_list = match tr_list with
				|[] -> build_list
				|h::t -> let new_letter = (transition_get_letter h) in find_cur_transitions_letter_list t (list_insert_sans_doublon build_list new_letter)
			in let rec add_bot_transitions_state letter_list = match letter_list with
				|[] ->()
				|h::t -> new_arr.(cur_state) <- (h, q)::(new_arr.(cur_state))
			in add_bot_transitions_state (list_filter_out base_letter_list (find_cur_transitions_letter_list (new_arr.(cur_state)) []));
				add_bot_transitions (cur_state + 1)
		end
		in add_bot_transitions 0;
		(q+1, sigma, new_arr, i, f)

let aut_is_language_included (a : automate_t) (b : automate_t) : bool =
	let n_aut = aut_prod a (aut_invert b) in (* Il faut déterminiser et compléter b (en émondant) pour en donner l'inverse!*)
	aut_is_language_empty n_aut


(* Question 10*)
let q_10 n d d_prime = 

	let rec build_t_list t = 
		if (t > 50) then []
		else begin
			if(aut_is_language_included (aut_gen_random (2 * t) n 2 d) (aut_gen_random (2 * t + 1) n 2 d_prime) )
				then (t::(build_t_list (t+1))) 
				else build_t_list (t+1)
		end
	in let rec find_min_card_list l = match l with
		|[] -> (Int.max_int, 0)
		|h::tail -> let m, s = (find_min_card_list tail) in if (h < m) then (h, s+1) else (m, s+1)

	in let t_list = build_t_list 1 in let min, size = find_min_card_list t_list in
	Printf.printf "   * Question 10 : Pour n = %d, d = %d, d' = %d, Nous obtenons min = %d et Card = %d\n" n d d_prime min size

let _ =
	u_tab.(0) <- u_0;

	(* q_1 2; q_1 10; q_1 1000; q_1 11000; *)	(* Ok avec u_0 = 1 *)

	(* q_2 1 10 5 3; q_2 2 100 10 20; q_2 3 900 20 200; *)  (* Ok avec u_0 = 1 *)

	(* q_3 1 10 5 3; q_3 2 100 10 5; q_3 3 200 10 6; q_3 4 1000 25 5;*) (* Ok avec u_0 = 1 *)

	(* q_4 10 5 15; q_4 50 5 100; q_4 75 7 100; q_4 200 10 100;*) (* Ok avec u_0 = 1 *)

	(* q_5 20 10; q_5 50 10; q_5 150 8; q_5 500 12;*) (* Ok avec u_0 = 1, ATTENTION AU TEMPS D'EXECUTION!*)

	(* q_6 3; q_6 4; q_6 5;*) (* Ok avec u_0 = 1*)

	(* q_7 1 20 2 40; q_7 2 40 3 70; q_7 3 75 3 150; q_7 4 100 5 200;*) (* Ok avec u_0 = 1*)

	(* q_8 1 10 2 50 25; q_8 2 20 2 100 50; q_8 3 25 2 100 50; q_8 4 30 2 100 50;*) (* Ok avec u_0 = 1*)
	
	(* q_9 10 2 50 25; q_9 20 2 100 50; q_9 25 2 100 50; q_9 30 2 100 50; *)  (* Ok avec u_0 = 1 Attention au temps d'exécution *)

	(* q_10 10 3 8; q_10 15 5 10; q_10 60 5 10; q_10 90 10 15; *) (* Il faut déterminiser et émonder les automates ... flemme*)
	
	(* Q 11 : Se sert de Q 10, on vérifie A \subset B et B \subset A, puis s'ils sont bisimilaires *)