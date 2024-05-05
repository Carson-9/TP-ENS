(** ============== START PRELUDE ================== **)

(** Utilitaires *)

(** `read_text filename` lit la chaine dans le fichier `filename` et renvoi une string *)
let read_text (file : string) : string =
  let c = open_in file in
  input_line c

(** `read_motif(filename)` lit le motif dans le fichier `filename` et renvoi une liste de string *)
let read_motif (file : string) : string list =
  let c = open_in file in
  let rec aux () =
    try let s = input_line c in s :: aux () with
    | End_of_file -> []
  in
  aux ()

(** Tries *)

type node = Empty | Node of (bool * trie)
and trie = { a:node; c:node; g:node; t:node }

let pp fmt =
  let rec pp_node fmt (final, strie) =
    if final then Format.fprintf fmt "@<1>✓";
    if strie = { a=Empty; c=Empty; g=Empty; t=Empty } then ()
    else Format.fprintf fmt "@[<v>@<1>┐%a@]" pp_tree strie
  and pp_branch fmt c node = match node with
    | Empty -> ()
    | Node node -> Format.fprintf fmt "@,@<1>└%c%a" c pp_node node
  and pp_tree fmt t =
    pp_branch fmt 'A' t.a;
    pp_branch fmt 'C' t.c;
    pp_branch fmt 'G' t.g;
    pp_branch fmt 'T' t.t;
  in
  Format.fprintf fmt "@[<v>%a@]" pp_tree

(** `print_trie t` imprime le trie `t` *)
let print_trie (t: trie) : unit = Format.printf "%a@." pp t

let empty = { a=Empty; c=Empty; g=Empty; t=Empty }
let node f t = Node (f, t)

(** Un trie simple, correspondant au motif A, ACT, CGG, CGAC, TC *)
let simple_trie = {
  a = node true {empty with
                 c = node false {empty with t=node true empty}};
  c = node false {empty with
                  g = node false
                      {empty with
                       g = node true empty;
                       a = node false {empty with c = node true empty}}};
  g = Empty;
  t = node false {empty with
                  c = node true empty};
}


(** ============== STOP PRELUDE ================== **)


let mem word trie = 

	let n = String.length word in

    let rec eval_in_tree t i = match t with
		|Empty -> false
		|Node(b, next_trie) -> if(i >= (n-1)) then b
    else begin match word.[i] with
			|'A' -> eval_in_tree (next_trie.a) (i+1)
			|'C' -> eval_in_tree (next_trie.c) (i+1)
			|'G' -> eval_in_tree (next_trie.g) (i+1)
			|'T' -> eval_in_tree (next_trie.t) (i+1)
			|_ -> false
    	end

		
    in match word.[0] with
		|'A' -> eval_in_tree (trie.a) 0
		|'C' -> eval_in_tree (trie.c) 0
		|'G' -> eval_in_tree (trie.g) 0
		|'T' -> eval_in_tree (trie.t) 0
		|_ -> false


let find_first word trie =

	let n = String.length word in

    let rec eval_in_tree t i = match t with
		|Empty -> n
		|Node(b, next_trie) -> if b then i
    	else begin match word.[i] with
			|'A' -> eval_in_tree (next_trie.a) (i+1)
			|'C' -> eval_in_tree (next_trie.c) (i+1)
			|'G' -> eval_in_tree (next_trie.g) (i+1)
			|'T' -> eval_in_tree (next_trie.t) (i+1)
			|_ -> n
    	end in


	let rec string_eval ind = if(ind >= n) then None else match word.[ind] with
	|'A' -> let end_ind = (eval_in_tree (trie.a) ind) in
			if(end_ind) < n then Some(ind, String.sub word ind (end_ind - ind + 1)) else string_eval (ind + 1)
	|'C' -> let end_ind = eval_in_tree (trie.c) ind in
			if(end_ind) < n then Some(ind, String.sub word ind (end_ind - ind + 1)) else string_eval (ind + 1)
	|'G' -> let end_ind = eval_in_tree (trie.g) ind in
			if(end_ind) < n then Some(ind, String.sub word ind (end_ind - ind + 1)) else string_eval (ind + 1)
	|'T' -> let end_ind = eval_in_tree (trie.t) ind in
			if(end_ind) < n then Some(ind, String.sub word ind (end_ind - ind + 1)) else string_eval (ind + 1)
	|_ -> None
		
    in string_eval 0


let rec trie_size t = 
	(eval_node t.a) + (eval_node t.c) + (eval_node t.g) + (eval_node t.t)


and eval_node n = match n with
		|Empty -> 0
		|Node(b, next_trie) -> 1 + (trie_size next_trie)




let make_trie word_list = 

	let n = ref 0 in
	
	let rec add_word_to_trie w i = if(i = !n-1) then Node(true, {a = Empty; c = Empty; g = Empty; t = Empty})
		else let new_subt = add_word_to_trie w (i+1) in match w.[i] with
		|'A' -> Node(false, {a = new_subt; c = Empty; g = Empty; t = Empty})
		|'C' -> Node(false, {a = Empty; c = new_subt; g = Empty; t = Empty})
		|'G' -> Node(false, {a = Empty; c = Empty; g = new_subt; t = Empty})
		|'T' -> Node(false, {a = Empty; c = Empty; g = Empty; t = new_subt}) 
		|_ -> failwith "Word Contains an unknown character"

	in

	let rec parcours_trie w i subt = match w.[i] with
		|'A' -> begin match subt.a with
			|Empty -> {subt with a = (add_word_to_trie w i)}
			|Node(b, sub_subt) -> if(i =!n-1) then {subt with a = Node(true, sub_subt)} else {subt with a = Node(b, (parcours_trie w (i+1) sub_subt))}
		end

		|'C' -> begin match subt.c with
			|Empty -> {subt with c = (add_word_to_trie w i)}
			|Node(b, sub_subt) -> if(i = !n-1) then {subt with c = Node(true, sub_subt)} else {subt with c = Node(b, (parcours_trie w (i+1) sub_subt))}
		end

		|'G' -> begin match subt.g with
			|Empty -> {subt with g = (add_word_to_trie w i)}
			|Node(b, sub_subt) -> if(i = !n-1) then {subt with g = Node(true, sub_subt)} else {subt with g = Node(b, (parcours_trie w (i+1) sub_subt))}

		end

		|'T' -> begin match subt.t with
			|Empty -> {subt with t = (add_word_to_trie w i)}
			|Node(b, sub_subt) -> if(i = !n-1) then {subt with t = Node(true, sub_subt)} else {subt with t = Node(b, (parcours_trie w (i+1) sub_subt))}
		end

		|_ -> failwith "Word Contains an unknown character"

	in let rec parcours_word_list l = match l with
		|[] -> {a = Empty; c = Empty; g = Empty; t = Empty}
		|h::t -> n := String.length h; (parcours_trie h 0 (parcours_word_list t))

	in parcours_word_list word_list



let print_question_string qs_nb fn args = 
	Format.printf "* Question %d :\n    %s\n\n" qs_nb (fn (args))

let _ =
	print_question_string 1 (fun b -> if(b) then "True" else "False") (mem (read_text "1667/chaine_2.txt") simple_trie);
	print_question_string 2 (fun x -> match x with Some(ind, w) -> (Format.sprintf "Ind : %d, Word : %s" ind w) | None -> "No Result found")
							(find_first (read_text "1667/chaine_10000000.txt") simple_trie);

	print_question_string 3 (fun x -> (string_of_int (trie_size x))) simple_trie;

	print_trie (make_trie (read_motif "1667/motif_1000.txt"));