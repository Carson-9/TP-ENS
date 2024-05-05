let const_max_term = 10000000
let const_m = 2147483647
let const_mult = 16807

let const_u_0 = 42
let u_bar = Array.make const_max_term (-1)
let u_mod = 999983


let initialize_u_bar l = 
	for i = 1 to (l+1) do
		u_bar.(i) <- (const_mult * u_bar.(i-1) + 17) mod const_m;
	done


let get_u index = 
	let index_mod = index mod u_mod in
	if(u_bar.(index_mod) >= 0) then (u_bar.(index_mod))
	else begin
		let first_valid = ref index_mod in
		while (u_bar.(!first_valid) < 0 && !first_valid >= 1) do
			first_valid := !first_valid - 1;
		done;

		for i = (!first_valid + 1) to index do
			u_bar.(i) <- (const_mult * u_bar.(i-1) + 17) mod const_m;
		done;
		(u_bar.(index_mod)) end


let question_1 () = 
	print_string "Question 1 -- \n";
	print_int ((get_u 16) mod 997); print_newline ();
	print_int ((get_u 1024) mod 997); print_newline ();
	print_int ((get_u 1000000) mod 997); print_newline ()



type alphabet = Zero | One | Two | Three
type measure = alphabet array
type interpretation = bool array

let get_duration note is_long = match note, is_long with
	|Zero, true -> 16
	|Zero, false -> 1
	|One, true -> 32
	|One, false -> 2
	|Two, true -> 64
	|Two, false -> 4
	|Three, true -> 128
	|Three, false -> 8


let int_to_alphabet i = 
	if(i = 0) then Zero
	else if (i = 1) then One
	else if (i = 2) then Two
	else Three

let int_to_bool i =
	if(i = 0) then false
	else true

let get_length m i = 
	let n = Array.length m in
	if(Array.length i != n) then failwith "get_length -> Interpretation is of different size than the measure!"
	else begin 
		let len = ref 0 in
		for c = 0 to (n-1) do
			len := !len + get_duration m.(c) i.(c)
		done;
		!len end


let get_interpretation_number i = 

	(*Le sujet inverse les bits de poids faible et fort!!! Grrr*)

	let n = Array.length i in
	let num = ref 0 in
	for c = 0 to (n-1) do 
		 num := !num * 2 + (if (i.(c)) then 1 else 0)
	done;
	!num


let m_n_k n k =
	let m_table = Array.make k Zero in
	for i = 0 to (k-1) do
		m_table.(i) <- int_to_alphabet ((get_u (n + k - i - 1)) mod 4)
	done;
	m_table;;
	
let i_n_k n k = 
	let i_table = Array.make k false in
	for i = 0 to (k-1) do
		i_table.(i) <- int_to_bool ((get_u ((2 * n) + k - i - 1)) mod 2)
	done;
	i_table;;


let d_n_k n k = get_length (m_n_k n k) (i_n_k n k)



let array_truncate a l = Array.init l (fun i -> a.(i))


let equalize_len ma ia mb ib =
	let equalize_table = Array.make_matrix (Array.length ma) (Array.length mb) 0 in


let question_2 () =
	print_string "Question 2 -- \n";
	print_int (d_n_k 0 16); print_newline ();
	print_int (get_interpretation_number (i_n_k 0 6)); print_newline ();
	print_int (get_interpretation_number (i_n_k 0 16)); print_newline ();
	print_int (d_n_k 0 10000); print_newline ()



let _ = 

	u_bar.(0) <- const_u_0;
	initialize_u_bar 1000000;
	question_1 (); print_newline ();
	question_2 (); print_newline ();

