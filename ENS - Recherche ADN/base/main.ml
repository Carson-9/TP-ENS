include trie.cmi


let mem word trie = 
	let n = String.length word in
    let rec eval_in_tree t i = match t with
		|Empty -> false
		|Node(b, next_trie) -> if(i = (n-1)) then b
			else eval_in_tree (t.(word.[i])) (i+1)
		
    in
    eval_in_tree (trie.(word.[0])) 0


let _ () =
	
	mem "CGAC" simple_trie