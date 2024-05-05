let u_size = 10000
let u_arr = Array.make u_size (-1)
let u_0 = 42

type galaxy = {alliance_base : int; alien_base : int; road_number : int; planet_values : (float * float) array; road_infos : int array array}



let u_init lim = 
	u_arr.(0) <- u_0;
	for i = 1 to (lim) do
		u_arr.(i) <- (15091 * u_arr.(i-1)) mod 64007
	done; ()

let get_u ind = match ind with
	|ind when ind >= u_size -> failwith "u_arr -> Index Out of Bounds"
	|_ -> if(u_arr.(ind) = (-1)) then (u_init ind); u_arr.(ind)


let generate_galaxy n m p =
	let planet_stats = Array.init (n+m) (fun i -> if(i < n) then ((float_of_int (get_u (2 * i) mod 21)), (float_of_int (get_u (2 * i + 1) mod 21)))
		else ((float_of_int (get_u (2 * i) mod 7)), (float_of_int (get_u (2 * i + 1) mod 7)))) in

	let road_stats = Array.make_matrix n m (-1) in

	for i = 0 to (p-1) do
		let a = (get_u (2 * (n+m) + 3 * i) mod n) in
		let b = ((get_u (2 * (n+m) + 3 * i + 2) mod m)) in
		let d = (get_u (2 * (n+m) + 3 * i + 1) mod 50) in

		if((road_stats.(a).(b) = -1) || (road_stats.(a).(b) > d)) then road_stats.(a).(b) <- d
	done;

	{alliance_base = n; alien_base = m; road_number = p; planet_values = planet_stats; road_infos = road_stats}



type graph = {n : int; g : bool array array; deg : int array; nb_arr : int; ref_gal : galaxy ref}


let does_planet_win gal i j t = 
	if(gal.road_infos.(i).(j) = -1) then false
	else let fin_t = t +. (float_of_int gal.road_infos.(i).(j)) in
	let all_plan = gal.planet_values.(i) in
	let ali_plan = gal.planet_values.(j) in
	if(((fst all_plan) +. fin_t *. (snd all_plan)) > ((fst ali_plan) +. fin_t *. (snd ali_plan))) then true
	else false

let g_t n m p t = 

	let gal = generate_galaxy n m p in
	let gr = Array.make_matrix n m false in
	let d = Array.make (n+m) 0 in
	let nb_a = ref 0 in

	for i = 0 to (n-1) do
		for j = 0 to (m-1) do
			if(does_planet_win gal i j t) then begin 
				gr.(i).(j) <- true;
				d.(i) <- d.(i) + 1 ;
				d.(n + j) <- d.(n+j) + 1;
				nb_a := !nb_a +1
			end
		done;
	done;
	{n = (n + m); g = gr; deg = d; nb_arr = !nb_a; ref_gal = ref gal}




let critical_time gr ennemy_planet = 
	let n = (Array.length gr.g) in
	let time_arr = Array.make n 0.0 in
	let gal = !(gr.ref_gal) in
	let min_time = ref (-1.0) in

	for i = 0 to (n-1) do
		let n0, m0 = gal.planet_values.(i) in
		let ne, me = gal.planet_values.(n + ennemy_planet) in
		if(m0 = me) then begin 
			if n0 = ne then time_arr.(i) <- 0.0 else time_arr.(i) <- (-1.0)
		end
		else begin 
			time_arr.(i) <- (if( (ne -. n0) /. (m0 -. me) <= 0.0) then 0.0 else ((ne -. n0) /. (m0 -. me))); 
			if(!min_time < 0.0 && time_arr.(i) > 0.0) then min_time := time_arr.(i)
			else if (time_arr.(i) > 0.0 && !min_time > time_arr.(i)) then min_time := time_arr.(i)
		end
		done;
	(time_arr, !min_time)




let calculate_critical_times gr = 
	let min_time = ref (-1.0) in
	let time_list = ref [] in

	let purge_list l = match l with
		|[] -> []
		|h::t -> if(h <= 0.0) then (purge_list t) else h::(purge_list t)
	in

	for i = 0 to ((!(gr.ref_gal).ennemy_planet) - 1) do 
		let t_a, m_t = critical_time gr i in
		time_arr := (Array.to_list t_a) @ time_arr;
		if(!min_time < 0.0 || !min_time > m_t) then min_time := m_t
	done;
	((purge_list !time_list), !min_time)


let question_format i f =
	Printf.printf "Question %d : \n" i;
	f ();
	Printf.printf "End of Question %d ---- \n\n" i

let q1 () = 
	Printf.printf "    * %d\n    * %d\n    * %d\n" 
	(get_u 10) 
	(get_u 100)
	(get_u 1000)

let q2 () =
	Printf.printf "    * %d\n    * %d\n    * %d\n" 
	((generate_galaxy 20 10 40).road_infos.(0).(0))
	((generate_galaxy 20 10 200).road_infos.(0).(0))
	((generate_galaxy 100 50 300).road_infos.(0).(0))


let min_of_arr a =
	let n = Array.length a in
	let min = ref 0 in
	for i = 1 to (n-1) do
		if(a.(i) < a.(!min)) then min := i
	done;
	a.(!min)

let max_of_arr a =
	let n = Array.length a in
	let max = ref 0 in
	for i = 1 to (n-1) do
		if(a.(i) > a.(!max)) then max := i
	done;
	a.(!max)

let q3 () = 
	let g1 = g_t 20 10 40 3.0 in
	let g2 = g_t 20 10 200 30.0 in
	let g3 = g_t 100 50 300 50.0 in

	Printf.printf "    * %d, %d, %d\n    * %d, %d, %d\n    * %d, %d, %d\n" 
	g1.nb_arr (min_of_arr g1.deg) (max_of_arr g1.deg)
	g2.nb_arr (min_of_arr g2.deg) (max_of_arr g2.deg)
	g3.nb_arr (min_of_arr g3.deg) (max_of_arr g3.deg) 

 
let _ =
	question_format 1 q1;
	question_format 2 q2;
	question_format 3 q3