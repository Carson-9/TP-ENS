(*
   
  Compression [0,1[

*)


(* Définition de constantes globales données par le sujet *)
let max_u_size = 10000000
let u_0 = 42
let u_tab = Array.make max_u_size (-1)
let u_ind = ref 0
let m_mod = 336529


let get_u (ind : int) : int = 
  if (ind < 0) then failwith "get_u -> Index is Negative"
  else if (ind >= max_u_size) then failwith "get_u -> Index Out of Bounds"
  else if (ind <= !u_ind) then u_tab.(ind)
  else begin
    	for i = (!u_ind + 1) to ind do
      		u_tab.(i) <- (28 * u_tab.(i-1)) mod m_mod
    	done;
    	u_ind := ind;
    	u_tab.(ind)
	end


let q_1 (i : int) : unit = 
	Printf.printf "   * Question 1 : u_%d mod 1000 = %d \n" i ((get_u i) mod 1000)


type alphabet_t = int (* Assimilé à [|0, M|] *)
type symbol_t = int
type source_arr_t = symbol_t array
type source_list_t = symbol_t list
let main_alphabet : alphabet_t = 30

let bin_log (x : float) : float = (log x) /. (log 2.)

let gen_source_arr_random (n : int) : source_arr_t = 
  let sn = Array.make (n+1) 0 in
  let rec populate_sn index = if (index >= (n)) then begin sn.(n) <- (main_alphabet - 1); sn end
    else begin sn.(index) <- ((get_u index) mod (main_alphabet - 1)); populate_sn (index + 1) end
  
  in populate_sn 0


let gen_source_list_random (n : int) : source_list_t = 
  
  let rec build_sn ind = 
    if(ind = 0) then [main_alphabet - 1]
    else ((get_u (n - ind)) mod (main_alphabet - 1))::(build_sn (ind-1))

  in build_sn n


let q_2 (n : int) : unit =
  let a = (get_u (n-4)) mod (main_alphabet - 1) in
  let b = (get_u (n-3)) mod (main_alphabet - 1) in
  let c = (get_u (n-2)) mod (main_alphabet - 1) in
  let d = (get_u (n-1)) mod (main_alphabet - 1) in
  Printf.printf "   * Q2 : Avec n = %d, [%d, %d, %d, %d, %d]\n" n a b c d (main_alphabet - 1)


type occurence_t = int

let get_occ_table_array (source : source_arr_t) : occurence_t array = 
  let occ_table = Array.make main_alphabet 0 in
  let n = Array.length source in
  let rec fill_occ_table index = if(index >= n) then occ_table
    else begin occ_table.(source.(index)) <- occ_table.(source.(index)) + 1; fill_occ_table (index + 1) end
  in fill_occ_table 0

let get_occ_table_list (source : source_list_t ) : occurence_t array =
  let occ_table = Array.make main_alphabet 0 in
  let rec fill_occ_table subl = match subl with
    |[] -> occ_table
    |h::t -> if(h > main_alphabet) then failwith "get_occ_table_list -> found letter out of range!\n" 
      else occ_table.(h) <- (occ_table.(h) + 1); fill_occ_table t
  in fill_occ_table source


let occurence_probability (occ_nb : occurence_t) (source_size : int) : float = 
  ((Float.of_int occ_nb) /. (Float.of_int source_size))

let entropy_of_source (n : int) : float =
  let sn = gen_source_list_random n in
  let occ_table = get_occ_table_list sn in

  let rec compute_entropy index = 
    if (index >= main_alphabet) then 0.0
    else begin
      let prev_entropy = compute_entropy (index + 1) in
      if(occ_table.(index) = 0) then prev_entropy else 
        begin
          let occ_probability = occurence_probability (occ_table.(index)) (n+1) in
          prev_entropy -. (occ_probability *. (bin_log occ_probability))
        end
      end
  in (compute_entropy 0)


let q_3 n = 
  Printf.printf "   * Q3 : Entropie de S_%d ~ %f\n" n 
  ((Float.round ((entropy_of_source n) *. 100000.0)) /. (100000.0))




(* ---------------------- Partie 1 --------------------------- *)


type 'a bin_tree_t = Feuille of 'a | Noeud of ('a bin_tree_t) * ('a bin_tree_t)
type huffman_tree_t = int bin_tree_t

let insert_in_list_couple (l : ('a * int) list) (c : 'a * int) : (('a * int) list) = 
  let c_tree, c_score = c in
  let rec insert_rec sub_list = match sub_list with
    |[] -> [c]
    |(th,oh)::t -> if(oh < c_score) then (th,oh)::(insert_rec t) else (c::sub_list)
  in insert_rec l 

let huffman_tree_from_occurences (occ_table : occurence_t array) : (huffman_tree_t * int) = 
  let rec build_init_tree_list i =
    if(i >= main_alphabet) then []
    else if(occ_table.(i) > 0) then insert_in_list_couple (build_init_tree_list (i+1)) ((Feuille i), occ_table.(i))
    else (build_init_tree_list (i+1))
  in

  let rec build_huff_tree subt_list = match subt_list with
    |[] -> failwith "make_huffman_tree -> Error, Empty list!"
    |[e] -> e
    |(ta,oa)::(tb, ob)::t -> build_huff_tree (insert_in_list_couple t ((Noeud (ta, tb)), (oa + ob)))
  in build_huff_tree (build_init_tree_list 0)


let encode_source_huffman (n : int) : string =
  let sn = gen_source_list_random n in
  let occ_table = get_occ_table_list sn in
  let huff_tree, occ_score = huffman_tree_from_occurences occ_table in

  let ht = Hashtbl.create main_alphabet in

  let rec make_hashtbl subt code = match subt with
    |Feuille(c) -> Hashtbl.add ht c code
    |Noeud(ta, tb) -> make_hashtbl (ta) (code ^ "0"); make_hashtbl (tb) (code ^ "1")


  in make_hashtbl huff_tree "";

  let rec encode symb_list acc_text = match symb_list with
    |[] -> acc_text
    |h::t -> encode t (acc_text ^ (Hashtbl.find ht h))

  in encode sn ""


let entropy_bin_string s = 
  let n = String.length s in
  let rec get_occ i =
    if(i >= n) then (0,0)
    else let o0, o1 = get_occ (i+1) in if(s.[i] = '1') then (o0, o1+1) else (o0 + 1, o1) 

  in let o0,o1 = get_occ 0 in
  let prob_zero = (Float.of_int o0) /. (Float.of_int n)  in
  let prob_un = (Float.of_int o1) /. (Float.of_int n) in

  0.0 -. (prob_zero *. (bin_log prob_zero)) -. (prob_un *. (bin_log prob_un))


let q_4 (n : int) =
  let compressed_str = (encode_source_huffman n) in
  Printf.printf "   * Q4 : Pour n = %d, Nous obtenons |s| = %d\n" n (String.length compressed_str)



(* ---------------------- Partie 2 --------------------------- *)


let frequency_table_random_source (n : int) : float array = 
  let sn = gen_source_list_random n in
  Array.map (fun e -> occurence_probability e (n+1)) (get_occ_table_list sn)


let arithmetic_code (n : int) : float * float =
  let sn = gen_source_list_random n in
  let occ_table = frequency_table_random_source n

  in let rec shift_interval_begin symbol scale index = 
    if(index >= main_alphabet || index = symbol) then 0.0
    else (shift_interval_begin symbol scale (index + 1)) +. (scale *. occ_table.(index))

  in let rec recursion l d word = match word with
    |[] -> (l, d)
    |h::t -> let scale = (d -. l) in 
      let beginning_interval = shift_interval_begin h scale 0 in
      recursion (l +. beginning_interval) ((l +. beginning_interval) +. (scale *. (occ_table.(h)))) t

    in recursion 0.0 1.0 sn


let float_round_to_n (f : float) (n : int) : float =
  let rec build_nb cur_f cur_n = 
    if(cur_n <= 0) then Float.round cur_f else (build_nb (cur_f *. 10.0) (cur_n - 1)) /. (10.0)
  in build_nb f n

let q_5 (n : int) : unit = 
  let l, d = arithmetic_code n in
  Printf.printf "   * Q5 : [%.15f, %.15f[\n" (float_round_to_n l n) (float_round_to_n d n)


let arithmetic_decode (code : float) (n : int) (precision : int) : symbol_t list =

  let occ_table = frequency_table_random_source n in

  let rec find_current_letter scale letter current_lower_bound = 
    if(letter >= main_alphabet || (current_lower_bound +. (scale *. (occ_table.(letter)))) >= code) then (letter, current_lower_bound)
    else find_current_letter scale (letter + 1) (current_lower_bound +. (scale *. (occ_table.(letter))))

  in let rec rebuild_word l d word_acc prec = 
      let scale = (d -. l) in
      let letter, new_l = find_current_letter scale 0 l in 
      if (prec <= 0) then (letter :: word_acc)
      else rebuild_word new_l (new_l +. (scale *. (occ_table.(letter)))) (letter :: word_acc) (prec - 1)
 

in List.rev (rebuild_word 0.0 1.0 [] precision)


let q_6 (n : int) : unit = match (arithmetic_decode ((Float.of_int u_0) /. (Float.of_int n)) n 5) with
  |a::b::c::d::e::t -> Printf.printf "   * Q6 : [%d, %d, %d, %d, %d]\n" a b c d e; 
  |_ -> failwith "arithmetic_decode -> Bad decode!"


let bin_code_ln (n : int) (number_bits : int) : int list = 
  let l, d = arithmetic_code n in
  
  let rec decompose current_float precision bit_list cur_pow_two = 
    if(precision <= 0) then bit_list
    else begin
      if(current_float >= cur_pow_two) then decompose (current_float -. cur_pow_two) (precision - 1) (1::bit_list) (cur_pow_two /. 2.0)
      else decompose current_float  (precision - 1) (0::bit_list) (cur_pow_two /. 2.0)
    end

  in (decompose l number_bits [] 0.5)


let q_7 (n : int) : unit = match (bin_code_ln n 20) with
  |a::b::c::d::e::f::t -> Printf.printf "   * Q7 : [%d, %d, %d, %d, %d, %d]\n" f e d c b a; 
  |_ -> failwith "bin_code_ln -> n is too little for 5 digit precision!"



(* ---------------------- Partie 3 --------------------------- *)


type decision_tree_t = int bin_tree_t

let make_file_contains_array (filename : string) : bool array array =

  let new_array = Array.make 50000 [||] in

  let update_array (word : string) (a : bool array) : unit = 
    let n = String.length word in 
    
    let rec update_recursive (cur_index : int) : unit =
      if(cur_index >= n) then () else begin
      let index_of_char = (Char.code word.[cur_index]) - (Char.code 'a') in
      a.(index_of_char) <- true; update_recursive (cur_index + 1)

    in update_recursive 0

  let rec read_file (file_handle : in_channel) (cur_line : int) : unit = 
    try
      let new_line = read_line file_handle in
      new_array.((get_u cur_line)) <- Array.make main_alphabet false in
      update_array new_line (new_array.((get_u cur_line)));
      read_file file_handle (cur_line + 1) 
    with |End_Of_File -> ()

  let make_truth_array () : unit = 
    let file_handle = open_in filename in
    read_file file_handle 0;
    close_in file_handle

  in make_truth_array (); new_array



let word_contains_char (a : bool array) (c : char) : bool = 
  a.(((Char.code c) - (Char.code 'a')))


(* FAIRE ID3 *)


	(* Main *)

let _ =
  u_tab.(0) <- u_0;

  (* q_1 1; q_1 42; q_1 100000; *)  (* OK *)
  (* q_2 10; q_2 1000; q_2 100000; *) (* OK *)
  (* q_3 10; q_3 1000; q_3 100000; *) (* OK *)
  (* q_4 10; q_4 1000; q_4 100000; *) (* OK *)
  (* q_5 7; q_5 10; q_5 15; *) (* OK *)
  (* q_6 100; q_6 500; q_6 1000; *) (* OK *)
  (* q_7 7; q_7 10; q_7 15; *) (* OK *)

