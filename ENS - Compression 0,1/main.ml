let u0_tilde = 42
let u0 = 42

let array_size_max = 10000000
let u_table = Array.make array_size_max (-1)
let u_defined_index = ref 0

let alphabet_size = 29


let bin_log x = (log x) /. (log 2.)

let get_u i = 
  if(!u_defined_index >= i) then u_table.(i)
  else if (i >= array_size_max) then failwith "get_u -> Index Out of Bounds (Too big!)"
  else begin
    for index = (!u_defined_index + 1) to i do
      u_table.(index) <- (28 * u_table.(index - 1)) mod 336529
    done;
    u_defined_index := i;
    u_table.(i)
  end


let get_sn n = 
  let sn_arr = Array.make (n+1) 0 in
  for i = 0 to (n-1) do
    sn_arr.(i) <- (get_u i) mod alphabet_size
  done;
  sn_arr.(n) <- alphabet_size;
  sn_arr



let get_sn_list n = 
  let rec recursion ind = 
    if(ind = 0) then [alphabet_size]
    else ((get_u (n - ind)) mod alphabet_size)::(recursion (ind-1))

  in recursion n

let get_occ_table a = 
  let occ_table = Array.make (alphabet_size + 1) 0 in
  let n = Array.length a in
  for i = 0 to (n-1) do
    occ_table.(a.(i)) <- occ_table.(a.(i)) + 1;
  done;
  occ_table

let get_occ_table_list l =
  let occ_table = Array.make (alphabet_size + 1) 0 in
  let rec list_recursion subl = match subl with
    |[] -> occ_table
    |h::t -> if(h > alphabet_size) then failwith "get_occ_table_list -> found letter out of range!\n" 
      else occ_table.(h) <- (occ_table.(h) + 1); list_recursion t
  in list_recursion l

let entropy_of_source n =
  let sn = get_sn_list n in
  let occ_table = get_occ_table_list sn in
  let entropy = ref 0.0 in
  for letter = 0 to (alphabet_size) do
    let occ_prob = ((Float.of_int (occ_table.(letter))) /. (Float.of_int (n + 1))) in
    if(occ_prob > 0.0) then entropy := !entropy -. (occ_prob *. (bin_log occ_prob))
  done;
  (Float.trunc (!entropy *. 100000.0)) /. (100000.0)




type bin_tree = Feuille of int | Noeud of bin_tree * bin_tree


let rec insert_in_list_couple l c = match l with
  |[] -> [c]
  |(th,oh)::t -> if(oh < (snd c)) then (th,oh)::(insert_in_list_couple t c) else (c::l)

let make_huffman_tree occ_table = 
  let rec build_init_tree_list i =
    if(i > alphabet_size) then []
    else if(occ_table.(i) > 0) then insert_in_list_couple (build_init_tree_list (i+1)) ((Feuille i), occ_table.(i))
    else (build_init_tree_list (i+1))
  in
  let rec build_huff_tree subt_list = match subt_list with
    |[] -> failwith "make_huffman_tree -> Error, Empty list!"
    |(ta,oa)::(tb, ob)::t -> build_huff_tree (insert_in_list_couple t ((Noeud (ta, tb)), (oa + ob)))
    |[e] -> e
  in build_huff_tree (build_init_tree_list 0)


let encode_from_tree n =
  let sn = get_sn_list n in
  let occ_table = get_occ_table_list sn in
  let huff_tree, occ_score = make_huffman_tree occ_table in

  let ht = Hashtbl.create alphabet_size in

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



let arithmetic_code n =
  let sn = get_sn_list n in
  let sn_length = List.length sn in
  let occ_table = Array.map (fun e -> (Float.of_int e) /. (Float.of_int (sn_length))) (get_occ_table_list sn) in

  let rec recursion l d word = match word with
    |[] -> (l, d)
    |h::t -> let scale = (d -. l) in 
      let offset = ref 0.0 in
      for i = 0 to (alphabet_size + 1) do
        if(i < h) then offset := !offset +. (scale *. (occ_table.(i)))
      done;
      recursion (l +. !offset) ((l +. !offset) +. (scale *. occ_table.(h))) t

    in recursion 0.0 1.0 sn



let arithmetic_decode f precision =

  let sn = get_sn_list precision in
  let sn_length = List.length sn in
  let occ_table = Array.map (fun e -> (Float.of_int e) /. (Float.of_int (sn_length))) (get_occ_table_list sn) in

  let rec recursion l d word_acc prec = 
      if (prec <= 0) then begin
        let offset = ref 0.0 in
        let letter = ref 0  in
        let scale = (d -. l) in
        for i = 0 to (alphabet_size) do
          if(l +. !offset < f) then begin offset := !offset +. (scale *. (occ_table.(i))); letter := i end
        done;
        (!letter :: word_acc)
      end 
      else begin
        let offset = ref 0.0 in
        let letter = ref 0  in
        let scale = (d -. l) in
        for i = 0 to (alphabet_size) do
          if(l +. !offset < f) then begin offset := !offset +. (scale *. (occ_table.(i))); letter := i end
        done;
        recursion (l +. !offset -. (scale *. (occ_table.(!letter)))) (l +. !offset ) (!letter::word_acc) (prec - 1)
      end

in List.rev (recursion 0.0 1.0 [] precision)



let bin_code_ln n = 
  let l, d = arithmetic_code n in
  let rec write_build cur_two_pow acc prec =
    if (prec = 0) then acc
    else begin
      let new_bit = (int_of_float (l *. cur_two_pow)) mod 2 in
      write_build (cur_two_pow *. 2.0) (new_bit::(acc)) (prec - 1)
    end
  in (write_build 2.0 [] n)



let q1 x = 
  Printf.printf " Q1  * %d\n" ((get_u x) mod 1000)


let q2 n =
  let a = (get_u (n-4)) mod alphabet_size in
  let b = (get_u (n-3)) mod alphabet_size in
  let c = (get_u (n-2)) mod alphabet_size in
  let d = (get_u (n-1)) mod alphabet_size in
  Printf.printf " Q2  * %d %d %d %d %d\n" a b c d alphabet_size


let q3 n = 
  Printf.printf " Q3  * %f\n" (entropy_of_source n)

let q4 n =
  let compressed_str = (encode_from_tree n) in
  Printf.printf " Q4  * %s : %f\n" compressed_str (entropy_bin_string compressed_str)


let q5 n = 
  let l, d = arithmetic_code n in
  Printf.printf " Q5  * [%f, %f[\n" l d

let q6 n = match (arithmetic_decode ((Float.of_int u0) /. (Float.of_int n)) n) with
  |a::b::c::d::e::t -> Printf.printf " Q6  * %d %d %d %d %d\n" a b c d e; 
  |_ -> failwith "arithmetic_decode -> Bad decode!"


let q7 n = match (bin_code_ln n) with
  |a::b::c::d::e::t -> Printf.printf " Q7  * %d %d %d %d %d\n" a b c d e; 
  |_ -> failwith "bin_code_ln -> n is too little for 5 digit precision!"

let _ =
  u_table.(0) <- u0;
  let init_u = get_u 10000 in
  
  (*
  q1 1;
  q1 42;
  q1 100000;

  q2 10;
  q2 1000;
  q2 100000;
  

  q3 10;
  q3 1000;
  q3 100000;
  

  q4 10;
  q4 1000;
  q4 100000;
  

  q5 7;
  q5 10;
  q5 15;
  

  q6 100;
  q6 500;
  q6 1000;
  *)

  q7 7;
  q7 10; 
  q7 15;



