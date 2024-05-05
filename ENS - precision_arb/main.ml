let max_u = 50000
let u_tab = Array.make max_u (-1)
let u_0 = 3

type naive_big_num = int list
type out_types = I of int | T of (int * int * int) | N of naive_big_num

let linear_fill_u max_rank =
	for i = 1 to (max_rank - 1) do
		if(u_tab.(i) = -1) then u_tab.(i) <- (15091 * u_tab.(i-1)) mod 64007
	done; ()
	
let get_u ind = match ind with
	|ind when ind >= max_u -> failwith "get_u -> Index is over the size limit!"
	|_ -> if(u_tab.(ind) = (-1)) then linear_fill_u (ind + 1);
	u_tab.(ind)




	let rec print_li l = match l with
	|[] -> ()
	|h::t -> print_int h; print_li t 


let rec print_out_t t = match t with
	|I(i) -> print_int i
	|T(a, b, c) -> print_int a; print_string " "; print_int b; print_string " "; print_int c
	|N(l) -> print_li l


let print_question str param_l =

	let rec aux l = match l with
		|[] -> ()
		|h::t -> print_string "    * "; print_out_t h; print_newline (); (aux t)

	in (print_string str); (aux param_l) 


(*
let v k l = 
	let sum = ref 0 in
	let ten_pow = ref 1 in
	for i = 1 to l do
		sum := !sum + (((get_u (k * l + i)) mod 10) * !ten_pow);
		ten_pow := 10 * !ten_pow;
	done;
	!sum
*)

let v k l = 

	let rec sub_fn i acc = match i with
		|i when i = l -> ((get_u ((k + 1) * l)) mod 10)::acc
		|_ -> (sub_fn (i+1) (((get_u (k * l + i)) mod 10) :: acc))

in sub_fn 1 []


let rec sig_f n = match n with
	|[] -> (0,0,0)
	|[e] -> if(e = 3) then (1, 0, 0) 
			else if (e = 5) then (0, 1, 0) 
			else if(e = 7) then (0, 0, 1) 
			else (0, 0, 0)
	|h::t -> let a, b, c = sig_f t in
		if(h = 3) then (a + 1, b, c) 
		else if (h = 5) then (a, b + 1, c) 
		else if(h = 7) then (a, b, c + 1) 
		else (a, b, c)


(*
Renvoie la comparaison entre a et b, deux entiers en représentation naïve :
		-1 si a > b,
		0 si a = b
		1 si b > a
*)
let rec comp a b = 
	match(a,b) with
	|([], []) -> 0
	|([a], [b]) -> if(a < b) then 1 else -1
	|(l, []) -> (-1)
	|([], l) -> 1
	|(h1::t1, h2::t2) -> if(h1 > h2) then -1 else if(h1 < h2) then 1 else (comp t1 t2)

let cmp a b = if(comp a b) <= 0 then true else false


let s l i = v i l

let s_list n l = 
	let arr = Array.make n [] in
	for i = 0 to (n-1) do 
		arr.(i) <- s l i
	done;
	arr


let question_3 n l =
	let snl = s_list n l in 
	let cur_max = ref [] in
		for i = 0 to (n-1) do 
			if(cmp snl.(i) !cur_max) then cur_max := snl.(i)
		done;
	sig_f (!cur_max)




let mirror l = 
	let rec mir_rec buf li = match li with
		|[] -> buf
		|h::t -> mir_rec (h::buf) t
	in mir_rec [] l  


let sum a b =

	let rec add la lb carry = match (la, lb) with
		|([], []) -> [carry]
		|(l, []) -> if(carry = 0) then l else add l [carry] 0
		|([], l) -> if(carry = 0) then l else add l [carry] 0
		|(ha::ta, hb::tb) -> ((ha + hb + carry) mod 10)::(add ta tb ((ha + hb + carry) / 10))
		
	in
	let na = mirror a in let nb = mirror b in
	mirror (add na nb 0)
	

let question_4 a b c d = sig_f (sum (v a b) (v c d))


let _ =
	u_tab.(0) <- u_0;
	print_question ("Question 1 :\n") ([I(get_u 30); I(get_u 300); I(get_u 3000)]);
	print_question ("Question 2 :\n") ([N(v 1 5); T(sig_f (v 2 50)); T(sig_f (v 10 500))]);
	(*print_question ("Question 3 :\n") ([T(question_3 10 10); T(question_3 100 100); T(question_3 200 200)]);*)
	print_question ("Question 4 :\n") ([T(question_4 1 50 2 50); T(question_4 1 100 2 100); T(question_4 1 500 2 500)]);
