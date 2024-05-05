let max_u_size = 10000000
let u_0 = 42
let u_tab = Array.make max_u_size (-1)
let u_ind = ref 0
let m_mod = 2147483647
let max_tape_size = 256


let get_u ind = 
  if (ind < 0) then failwith "get_u -> Index is Negative"
  else if (ind >= max_u_size) then failwith "get_u -> Index Out of Bounds"
  else if(ind <= !u_ind) then u_tab.(ind)
  else begin
    for i = (!u_ind + 1) to ind do
      u_tab.(i) <- (16807 * u_tab.(i-1)) mod m_mod
    done;
    u_tab.(ind) end




(* En théorie inutile de développer ainsi mais rend le code plus compréhensible*)
type transition_fun = (int * int -> int * int * int)
type automata = int * transition_fun (* |Q| (Q ~ [|1, n|]) * delta : etat, lettre -> nouvel_etat, nouvelle_lettre, deplacement *)

let automata_get_Q t = match t with
    |(q, delta) -> q

let automata_get_delta t = match t with
    |(q, delta) -> delta

let automata_unfold t = t

type state = int
type position = int
type tape = int array (* Conversion de tape : Z -> int en int array afin d'éviter les appels récursifs infâmes lors des calculs*)
(* Nous utilisons la bijection usuelle de N dans Z*)

let get_tape_ind n = 
  if(n >= 0) then 2 * n
  else 2 * (-n) - 1

let get_relative_ind n =
  if(n mod 2 = 0) then n/2
  else -((n+1) / 2)

type machine_state = state * position * tape
type tur_machine = automata * machine_state 

let emulate t = match t with
  |((q, delta), (state, pos, tape)) ->
    let tape_pos = get_tape_ind pos in
    let delta_res = delta state (tape.(tape_pos)) in begin match delta_res with
      |(new_state, new_letter, offset) -> tape.(tape_pos) <- new_letter;
        ((q, delta), (new_state, (pos + offset), tape))
  end

let is_done t = match t with
|((q, delta), (state, pos, tape)) -> if (state = q) then true else false


let tur_machine_rng n t =
  let delta = (fun state letter -> 
    let z = t + 8 * state + 4 * letter in
    if((get_u (z+1)) mod n = 0) then (n, ((get_u (z+2)) mod 2), 2 * ((get_u (z+3)) mod 2) - 1)
    else (((get_u (z)) mod n), ((get_u (z+2)) mod 2), 2 * ((get_u (z+3)) mod 2) - 1)
  ) in
  ((n,delta), (0, 0, (Array.make max_tape_size 0))) (*Such an array represents an unitialized tape*)

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






  

(* Questions *)

let q1 n = 
  Printf.printf "    * Question 1 : u_%d = %d \n" n ((get_u n) mod 1234)

let q2 n t = Printf.printf "    * Question 2 :\n";
  tur_machine_print (tur_machine_rng n t)

let q3 k = 
  let res_count = ref 0 in
  let fin_transition = ref 0 in
  for t = 0 to 999 do 
    fin_transition := 0;
      for state = 0 to 3 do
        let z_0 = t + 8 * state in 
        if((get_u (z_0+1)) mod 4 = 0) then incr fin_transition;
        let z_1 = t + 8 * state + 4 in
        if((get_u (z_1+1)) mod 4 = 0) then incr fin_transition;
      done;
      if(!fin_transition = k) then incr res_count;
  done;
  Printf.printf "    * Question 3 : %d\n" !res_count

(* Main *)

let _ =
  u_tab.(0) <- u_0;
  q1 10;
  q1 10000;
  q1 1000000;
  print_string "\n";

  q2 2 2;
  q2 2 3;
  q2 3 4;
  print_string "\n";

  q3 0;
  q3 1;
  q3 3;
  print_string "\n";