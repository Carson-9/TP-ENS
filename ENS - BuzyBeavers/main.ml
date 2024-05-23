(*
   
  Castors Affairés

*)


(* Définition de constantes globales données par le sujet *)
let max_u_size = 10000000
let u_0 = 42
let u_tab = Array.make max_u_size (-1)
let u_ind = ref 0
let m_mod = 2147483647


let get_u (ind : int) : int = 
  if (ind < 0) then failwith "get_u -> Index is Negative"
  else if (ind >= max_u_size) then failwith "get_u -> Index Out of Bounds"
  else if(ind <= !u_ind) then u_tab.(ind)
  else begin
    	for i = (!u_ind + 1) to ind do
      		u_tab.(i) <- (16807 * u_tab.(i-1)) mod m_mod
    	done;
    	u_ind := ind;
    	u_tab.(ind)
	end


let q_1 (i : int) : unit = 
	Printf.printf "   * Question 1 : u_%d = %d \n" i ((get_u i) mod 1234)

    

(* En théorie inutile de développer ainsi mais rend le code plus compréhensible*)
type state_t = int	(* Assimilé a [|1, n|]*)
type move_t = int
type bit_t = int
type transition_fun_t = (state_t -> bit_t -> (state_t * bit_t * move_t))
type automata_t = state_t * transition_fun_t
type transition_t = state_t * bit_t

let automata_identity (a : automata_t) : automata_t = a

let automata_get_Q (a : automata_t) : state_t = match a with
    |(q, delta) -> q

let automata_get_delta (a : automata_t) : transition_fun_t = match a with
    |(q, delta) -> delta

let transition_identity (t : transition_t) : transition_t = t

let transition_get_state (t : transition_t) : state_t = match t with
	|(q, b) -> q

let transition_get_bit (t : transition_t) : bit_t = match t with
	|(q, b) -> b


let z (t : int) (q_prime : state_t) (x_prime : bit_t) : int = (t + 8 * q_prime + 4 * x_prime)

let aut_gen_random (n : int) (t : int) : automata_t =
	let q_prime (z_val : int) : state_t = if( (get_u (z_val+1)) mod n = 0) then n else ((get_u z_val) mod n) in
	let f = fun (q : state_t) (x : bit_t) -> let z_val = (z t q x) in 
		((q_prime z_val), ((get_u (z_val + 2)) mod 2), 2 * ((get_u (z_val + 3)) mod 2) - 1) 
	in (n, f) 


let automata_print_verbose (a : automata_t) (s : string) : unit = match a with |(q, f) ->
	
	let rec print_state_transitions (q_cur : state_t) (f : transition_fun_t) = 
		if(q_cur >= q) then Printf.printf "\n\n" else begin
			let q_0, x_0, d_0 = f q_cur 0 in let q_1, x_1, d_1 = f q_cur 1 in
			Printf.printf "δ(q = %d, 0) -> (q' = %d, x' = %d, d = %d) -- δ(q = %d, 1) -> (q' = %d, x' = %d, d = %d)\n" 
			q_cur  q_0 x_0 d_0 q_cur q_1 x_1 d_1 ;
			print_state_transitions (q_cur + 1) f
		end

	in 
		Printf.printf "Automate actuel : %s\n\n" s;
		Printf.printf "* |Q| = %d\n" q;
		(print_state_transitions 0 f)


let automata_print (a : automata_t) : unit = automata_print_verbose a ""

let q_2 (n : int) (t : int) = 
	Printf.printf "   * Question 2 : \n"; automata_print_verbose (aut_gen_random n t) (Printf.sprintf "T(%d,%d)" n t)


let q_3 (k : int) : unit = 

	let bool_to_int (b : bool) = if b then 1 else 0 in

	let rec count_final_transitions (q : state_t) (t : int) =
		if(q >= 4) then 0 else
		bool_to_int ((get_u ((z t q 0) + 1)) mod 4 = 0) + bool_to_int ((get_u ((z t q 1) + 1)) mod 4 = 0)
			+ (count_final_transitions (q+1) t)	in

	let rec count_t_valid t = 
		if(t >= 1000) then 0 else begin
			if ((count_final_transitions 0 t) = k) then 1 + (count_t_valid (t+1)) 
			else (count_t_valid (t+1))
		end	in
	
	Printf.printf "   * Question 3 : Nous avons %d valeurs de t valides\n" (count_t_valid 0)

let max_tape_size = 512

type position_t = int
type tape_t = bit_t array (* Conversion de tape : Z -> int en int array afin d'éviter les appels récursifs infâmes lors des calculs*)
(* Nous utilisons la bijection usuelle de N dans Z*)

type tape_state_t = tape_t * position_t * state_t 
type turing_machine_t = automata_t * tape_state_t
type time_t = int

let relative_to_tape_ind (n : position_t) : int = 
	if(n >= 0) then 2 * n
	else 2 * (-n) - 1
  
  let tape_to_relative_ind (n : int) : position_t =
	if(n mod 2 = 0) then n/2
	else -((n+1) / 2)
  

exception Tape_OOB of position_t

let tape_write (tape : tape_t) (position : position_t) (bit : bit_t) : unit =
	let abs_pos = relative_to_tape_ind position in
	if(abs_pos >= max_tape_size || abs_pos < 0) then raise (Tape_OOB position)
	else tape.(abs_pos) <- bit

let tape_read (tape : tape_t) (position : position_t) : bit_t = 
	let abs_pos = relative_to_tape_ind position in
	if(abs_pos >= max_tape_size || abs_pos < 0) then raise (Tape_OOB position)
	else tape.(abs_pos)

let tape_state_identity (s : tape_state_t) : tape_state_t = s
let tape_state_get_tape (s : tape_state_t) : tape_t = match s with
	|(t, p, state) -> t
let tape_state_get_position (s : tape_state_t) : position_t = match s with
	|(t, p, state) -> p
let tape_state_get_state (s : tape_state_t) : state_t = match s with
	|(t, p, state) -> state

let turing_machine_identity (t : turing_machine_t) : turing_machine_t = t
let turing_machine_get_automata (t : turing_machine_t) : automata_t = match t with
	|(a, s) -> a
let turing_machine_get_tape_state (t : turing_machine_t) : tape_state_t = match t with
	|(a, s) -> s


let turing_machine_gen_random (n : int) (t : int) : turing_machine_t = 
	let initial_tape = (Array.make (max_tape_size) 0, 0, 0) in
	((aut_gen_random n t), initial_tape)


exception Final_State of turing_machine_t * time_t

let turing_machine_emulate (t : turing_machine_t) : turing_machine_t = match t with
	|(aut, tape_state) -> let tape, pos, state = tape_state_identity tape_state in
		let q, delta = automata_identity aut in let bit_val = tape_read tape pos in
		if(state = q) then raise (Final_State (t,0)) else begin
		let new_state, new_bit, offset = delta state bit_val in
		tape_write tape pos new_bit; 
		(aut, (tape, (pos + offset), new_state)) end

 
let turing_machine_run (t : turing_machine_t) (max_time : time_t) : turing_machine_t =

	let rec loop_exec current_machine current_time = 
		if(current_time >= max_time) then current_machine
		else try 
			loop_exec (turing_machine_emulate current_machine) (current_time + 1)
		with |Final_State(machine, time) -> raise (Final_State(machine, (time + 1)))
			|Tape_OOB(pos) -> raise (Tape_OOB(pos))
			|_ -> failwith "turing_machine_run -> Unknown error!"

	in loop_exec t 0


let tur_machine_print t = match t with
  |((q, delta), (state, pos, tape)) -> Printf.printf "Printing Turing machine : |Q| = %d\n" q;
  for i = 0 to (q-1) do 
    let a, b, c = delta i 0 in
    if (a = q) then begin Printf.printf "Delta(%d, 0) -> ⊥ %d %d | " i b c end
      else Printf.printf "Delta(%d, 0) -> %d %d %d | " i a b c;
    let d, e, f = delta i 1 in
    if(d = q) then begin Printf.printf "Delta(%d, 1) -> ⊥ %d %d\n" i e f end
    else Printf.printf "Delta(%d, 1) -> %d %d %d\n" i d e f;
  done;
  Printf.printf "------------------------\n"



let q_4 (k : int) : unit =

	let rec try_emulate (t : turing_machine_t) (p : int) : bool = 
		if(p > k) then false else try 
			try_emulate (turing_machine_emulate t) (p+1)
			with |Final_State(machine, time) -> true |_ -> failwith "try_emulate -> Unknown Error"
			
	in let rec count_t (t : int) : int = 
		if(t >= 1000) then 0 else begin
			if(try_emulate (turing_machine_gen_random 4 t) 1) then (1 + count_t (t+1)) 
			else (count_t (t+1))
		end

	in Printf.printf "   * Question 4 : Pour K = %d, nous obtenons %d t tels que E(T(4, t)) <= %d\n"
	k (count_t 0) k



(* ---------------------- Partie 2 --------------------------- *)

(*
   
	La notion d'ensemble bouclant ressemble beaucoup à la forte connexité.
	En effet, en représentant l'automate de T sous forme de graphe, un ensemble bouclant correspond
	à un ensemble d'états tels que les sommets associés (plusieurs par états possiblement) forment une composante
	fortement connexe.


	On remarque que L : les ensembles bouclants sont exactement les éléments ne pouvant accéder à l'état terminal
*)

type transition_list_t = state_t list
type graphe_t = state_t * (transition_list_t array)


let automata_to_graph (a : automata_t) : graphe_t = match a with
	|(q, delta) ->
		let transition_array = Array.make (q+1) [] in

		let rec populate_transitions current_state =
			if(current_state >= q) then () else begin
				let new_state_0, new_bit_0, offset_0 = delta current_state 0 in
				let new_state_1, new_bit_1, offset_1 = delta current_state 1 in
				if(new_state_0 = new_state_1) then transition_array.(current_state) <- [new_state_0]
				else  transition_array.(current_state) <- new_state_0 :: [new_state_1];
				populate_transitions (current_state + 1)
			end


		in populate_transitions 0; (q + 1, transition_array)


(* 
	Calculer Kosaraju n'est pas utile (mais plus rapide), nous nous contenterons 
	de calculer les états accessibles depuis un sommet donné  
*)


let get_etats_accessibles_list (g : graphe_t) (e : state_t) :  transition_list_t = match g with	
	|(q_with_final, transitions) ->

		let rec pp_recursif new_state accessed_arr =
			if(accessed_arr.(new_state)) then () 
			else begin
				accessed_arr.(new_state) <- true;
				call_pp_recursif transitions.(new_state) accessed_arr
			end

			and call_pp_recursif state_list accessed_arr = match state_list with
				|[] -> ()
				|head::tail -> pp_recursif head accessed_arr; call_pp_recursif tail accessed_arr

		in let rec make_list_from_array arr index cur_list = 
			if(index >= q_with_final) then cur_list
			else if(arr.(index)) then (make_list_from_array arr (index + 1) ((index)::cur_list))
			else (make_list_from_array arr (index + 1) cur_list)


		in if(e < 0 || e >= q_with_final) then [] else begin
		let accessed_array = Array.make q_with_final false in
		pp_recursif e accessed_array;
		make_list_from_array accessed_array 0 []
		end


exception Invalid_State of state_t

let get_etats_accessibles_array (g : graphe_t) (e : state_t) : bool array = match g with
	|(q_with_final, transitions) ->
		let rec pp_recursif new_state accessed_arr =
			if(accessed_arr.(new_state)) then () 
			else begin
				accessed_arr.(new_state) <- true;
				call_pp_recursif transitions.(new_state) accessed_arr
			end

			and call_pp_recursif state_list accessed_arr = match state_list with
				|[] -> ()
				|head::tail -> pp_recursif head accessed_arr; call_pp_recursif tail accessed_arr

		in if(e < 0 || e >= q_with_final) then raise (Invalid_State e) else begin
		let accessed_array = Array.make q_with_final false in
		pp_recursif e accessed_array;
		accessed_array
		end


let q_5_a (n : int) (m : int) : unit =

	let rec count_t t = 
		if(t >= m) then 0 else begin
			let accessed_array = get_etats_accessibles_array (automata_to_graph (aut_gen_random n t)) 0 in
			if(not accessed_array.(n)) then (1 + (count_t (t+1))) else (count_t (t+1))
			end
		in 
		Printf.printf "   * Question 5 (a) : Pour n = %d, M = %d, nous obtenons %d t tels que 0 appartienne à L(T(%d, t))\n"
		n m (count_t 0) n


let q_5_b (n : int) (m : int) : unit = 

	let rec count_state_in_looping graph current_state = 
		if(current_state = n) then 0 else begin
			let accessed_array = get_etats_accessibles_array graph current_state in
			if(not accessed_array.(n)) then (1 + (count_state_in_looping graph (current_state + 1)))
			else (count_state_in_looping graph (current_state + 1))
		end

	in let rec count_t t = 
		if(t >= m) then 0 else begin
			if((count_state_in_looping (automata_to_graph (aut_gen_random n t)) 0) = n / 2) then
				(1 + (count_t (t+1))) else (count_t (t+1))
		end

	in Printf.printf "   * Question 5 (b) : Pour n = %d, M = %d, nous obtenons %d t tels |L(T(%d, t))| = %d / 2\n"
	n m (count_t 0) n n



let q_6 (n : int) (m : int) (k : int) : unit =

	let rec count_t_couple t = 
		if (t >= m) then (0, 0) else begin
			try
				let exec_result = turing_machine_run (turing_machine_gen_random n t) k in
				let f_prev, b_prev = count_t_couple (t+1) in
				let fin_state = tape_state_get_state (turing_machine_get_tape_state exec_result) in
				let accessed_array = get_etats_accessibles_array (automata_to_graph (aut_gen_random n t)) fin_state in
				if (not accessed_array.(n)) then (f_prev, (b_prev + 1))
				else (f_prev, b_prev)
			with | Final_State(machine, time) -> let f_prev, b_prev = count_t_couple (t+1) in ((f_prev + 1), b_prev)
				| Tape_OOB(pos) -> failwith "Tape Out Of Bounds!"
				| Invalid_State(e) -> failwith "Invalid State called!"
				| unknown_except -> raise unknown_except

		end

	in let f, b = count_t_couple 0 in let i = m - f - b in 
	Printf.printf "   * Question 6 : Pour n = %d, M = %d, K = %d, nous obtenons f = %d, b = %d, i = %d\n"
	n m k f b i




type tape_configuration_t = (state_t * bit_t)

let tape_configuration_identity (c : tape_configuration_t) : tape_configuration_t = c
let tape_configuration_get_state (c : tape_configuration_t) : state_t =  match c with
	|(s, b) -> s
let tape_configuration_get_bit (c : tape_configuration_t) : state_t =  match c with
	|(s, b) -> b

type tape_block_previous_config_t = tape_configuration_t list

let turing_machine_detect_cycle (t : turing_machine_t) (k : time_t) : (turing_machine_t * bool) =
	let tape_prev_config_arr = Array.make (max_tape_size) [] in

	let rec insert_config_find_loop new_conf tape_prev_conf = match tape_prev_conf with	
		|[] -> tape_prev_config_arr[new_conf]
		|h::t ->

	let rec loop_exec current_machine current_time = 
		if(current_time >= max_time) then current_machine
		else try 
			loop_exec (turing_machine_emulate current_machine) (current_time + 1)
		with |Final_State(machine, time) -> raise (Final_State(machine, (time + 1)))
			|Tape_OOB(pos) -> raise (Tape_OOB(pos))
			|_ -> failwith "turing_machine_run -> Unknown error!"

	in loop_exec t 0


(* Main *)

let _ =
  u_tab.(0) <- u_0;

	(* q_1 10; q_1 10000; q_1 1000000; *) (* OK *)

	(* q_2 2 2; q_2 2 3; q_2 3 4; *) (* OK *)

	(* q_3 0; q_3 1; q_3 3 *) (* OK *)

	(* q_4 2; q_4 10; q_4 100; *)  (* OK *)

	(* q_5_a 4 1000; q_5_a 8 10000; q_5_a 32 100000; *) (* OK *)
	(* q_5_b 4 1000; q_5_b 8 10000; q_5_b 32 100000; *) (* OK *)

	(* q_6 2 1000 20; q_6 3 5000 50; q_6 4 10000 200; *) (* OK *)