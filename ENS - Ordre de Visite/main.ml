let max_u = 100000
let u_0 = 5

let u_table = Array.make max_u (-1)


let get_u i = 
  if(i = 0) then u_0 else
  if(i > max_u) then (-1) else
  if(u_table.(i) >= 0) then u_table.(i) else begin
  let filled_ind = ref i in 
  while (u_table.(!filled_ind) = (-1)) do
    filled_ind := !filled_ind -1
  done;
  for index = (!filled_ind + 1) to i do
    u_table.(index) <- (19999991 * u_table.(index - 1)) mod 19999999
  done;
  u_table.(i)
  end

let q1 n = 
  Printf.printf "    * Question 1 : %d\n" ((get_u n) mod 1000)



type graph = int list array


let rng_graph n m =
  
  let g = Array.make n [] in

  let rec fill_arete current_node next_node =
    if(next_node < 0) then []
    else begin let prev_list = (fill_arete current_node (next_node - 1)) in
    if(current_node <> next_node && (((get_u (n + m + 7 * current_node + 11 * next_node)) mod 1000) < m))
      then next_node::prev_list else prev_list end
  in
  for k = 0 to (n - 1) do
    g.(k) <- fill_arete k (n-1)
  done;
  g


let rec list_length l = match l with
  |[] -> 0
  |h::t -> 1 + (list_length t)

let get_nb_edge g = 

  let arc_nb = ref 0 in
  let n = Array.length g in
  for k = 0 to (n-1) do
    arc_nb := !arc_nb + (list_length g.(k))
  done;
  !arc_nb

  let q2 n m =
    Printf.printf "    * Question 2 : %d\n" (get_nb_edge (rng_graph n m))



type colored_graph = (graph * (int -> int))

let colored_get_graph cg = match cg with 
  |(g, c) -> g
  (*|_ -> failwith "Unknown graph format!\n"        <- Warning Unused ...*)

let colored_get_color cg = match cg with 
  |(g, c) -> c
  (*|_ -> failwith "Unknown graph format!\n"        <- Warning Unused ...*)

let colored_graph_unfold cg = cg
  


let rng_colored_graph n m p = 
  ((rng_graph n m), (fun i -> ((get_u (5 * i)) mod p))) 


let control_sum cg = 
  let g, c = colored_graph_unfold cg in
  let n = Array.length g in
  let cur_sum = ref (get_nb_edge g) in
  for k = 0 to (n-1) do
    cur_sum := !cur_sum + (c k)
  done;
  !cur_sum

let q3 n m p =
  Printf.printf "    * Question 3 : %d\n" (control_sum (rng_colored_graph n m p))



let rec is_el_in_list l e = match l with
  |[] -> false
  |h::t -> (h = e) || (is_el_in_list t e) (* Lazy evaluation *)


let rec separate cl sep_ens = match cl with
  |[] -> ([], [])
  |h::t -> let la, lb = (separate t sep_ens) in
    if(is_el_in_list sep_ens h) then (h::la, lb) else (la, h::lb)

let rec refine entry separator =  match entry with
  |[] -> []
  |h::t -> begin match (separate h separator) with
    |([], out_list) -> (out_list)::(refine t separator)
    |(in_list, []) ->  (in_list)::(refine t separator)
    |(in_list, out_list) -> (in_list)::(out_list)::(refine t separator)
end

let rec linear_list_init n = 
  if(n = 0) then [0] else n::(linear_list_init (n-1))

let graph_partition_refining n m r =
  let g = rng_graph n m in

  let rec graph_partition_refining_rec i = 
    if(i = 0) then refine [(linear_list_init (n-1))] (g.((get_u (n + m)) mod n))
    else let prev_refine = graph_partition_refining_rec (i-1) in
    refine prev_refine (g.((get_u (n + m + i)) mod n))
  in graph_partition_refining_rec r


let q4 n m r =
  Printf.printf "    * Question 4 : %d\n" (list_length (graph_partition_refining n m r))


let relation_calculation cg = 
  let g, c = colored_graph_unfold cg in
  let n = Array.length g in   (* Compute at most n iterations *)

  let initial_color_partition () = 
    let color_array = Array.make n [] in
    for i = 0 to (n-1) do
      color_array.(c i) <- i::color_array.(c i)
    done;
    Array.to_list color_array
  in
    
  let rec clean_empty_list l = match l with
      |[] -> []
      |h::t -> begin match h with 
        |[] -> (clean_empty_list t) 
        |a::b -> h::(clean_empty_list t) 
    end
  in

  let rec relation_over_out_class i partition = if(i = -1) then partition else begin
    let prev_refine = relation_over_out_class (i-1) partition in
    refine prev_refine (g.(i))
  end

  in let rec relation_over_path_length k partition = if (k = 0) then partition else begin
    let previous_classes = relation_over_path_length (k-1) partition in
    relation_over_out_class (n-1) previous_classes 
  end
    
  in relation_over_path_length n (clean_empty_list (initial_color_partition ()))


let q5 n m p =
  Printf.printf "    * Question 5 : %d\n" (list_length (relation_calculation (rng_colored_graph n m p)))


let _ =
  u_table.(0) <- u_0;

  q1 50;
  q1 100;
  q1 1000;
  q1 5000;
  
  print_string "\n\n";

  q2 100 100;
  q2 100 500;
  q2 1000 100;
  q2 1000 500;

  print_string "\n\n";

  q3 777 222 5;
  q3 777 500 25;
  q3 1234 222 25;
  q3 1234 500 75;

  print_string "\n\n";

  q4 111 222 5;
  q4 111 500 100;
  q4 1234 222 5;
  q4 1234 500 100;

  print_string "\n\n";

  (*q5 3000 1 4;
  q5 3001 2 4;
  q5 3002 900 200;
  q5 3002 900 224;*)

  print_string "\n\n";


