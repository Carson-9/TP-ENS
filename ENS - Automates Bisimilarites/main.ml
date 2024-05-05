let max_u = 99999
let u_tab = Array.make max_u (-1)
let u_0 = 42
let a = 1103515245
let c = 12345
let m = 0b1000000000000000



let get_u i =
	if (u_tab.(i) <> -1) then u_tab.(i) else
	if(i >= max_u) then failwith "get_u -> Outside of Range!\n"

	else begin
		let p = ref i in 
		while u_tab.(!p) = (-1) do
			p := (!p) -1
		done;

		for k = !p + 1 to i do
			u_tab.(k) <- (a * u_tab.(k-1) + c) mod m
		done;
		u_tab.(i)
	end 


let x i j s = 
	(271 * (get_u i) + 293 * (get_u j) + 283 * (get_u s)) mod 1000


type aut = {q : int; sigma : int; delta : bool array array array; i : int list; f : int list}

let aut_t t n m d = 
	let new_delta = Array.init n (fun i -> Array.make_matrix m n false) in
	for i = 0 to (n-1) do 
		for s = 0 to (m-1) do
			for j = 0 to (n-1) do
				if ((( get_u (10 * t + (x i j s))) mod (n * n * m / (d* (n - i + 1)) + 1)) = 0) then
					new_delta.(i).(s).(j) <- true 
				done;
			done;
		done;
		{q = n; sigma = m; delta = new_delta; i = [0]; f = [n-1]}


let get_num_trans aut =
	let count = ref 0 in 
	for i = 0 to (aut.q - 1) do
		for s = 0 to (aut.sigma - 1) do
			for j = 0 to (aut.q - 1) do 
				if(aut.delta.(i).(s).(j)) then count := !count + 1
				done;
			done;
		done; !count




let q_1 () = 
	print_string "Question 1 -- \n";
	print_int (get_u 2); print_string " \n";
	print_int (get_u 10); print_string " \n";
	print_int (get_u 1000); print_string " \n";
	print_int (get_u 11000); print_string " \n\n"

let q_2 () = 
	print_string "Question 2 -- \n";
	print_int (get_num_trans (aut_t 1 10 5 3)); print_string "\n";
	print_int (get_num_trans (aut_t 2 100 10 20)); print_string "\n";
	print_int (get_num_trans (aut_t 3 900 20 200)); print_string "\n\n"



let _ =
	u_tab.(0) <- u_0;
	q_1 ();
	q_2 ();