(* Exercise largely taken from Jeff Erickson's lecture notes. *)

(* In a previous life, you worked as a cashier in the lost Antarctican colony of Nadira, spending
   the better part of your day giving change to your customers. Because paper is a very rare
   and valuable resource in Antarctica, cashiers were required by law to use the fewest bills
   possible whenever they gave change. Thanks to the numerological predilections of one of
   its founders, the currency of Nadira, called Dream Dollars, was available in the following
   denominations: $1, $4, $7, $13, $28, $52, $91, $365.
*)

let denominations = [1; 4; 7; 13; 28; 52; 91; 365]


(* 0.i) Formulate the problem precisely in natural language. *)

(*
   Given an amount n, find the fewest numbers from the list denominations,
   so that their sum equals n.
   or
   Find a list l such that sum(l) = n where each x € l is also € denominations.
*)


(* 0.ii) Describe the problem recursively. *)

(*
   Given an amount n, pick all numbers m_i from the list, which are 
   still smaller than n, and then solve the problem for n-m_i. We also
   have to add candidate to the list. In the end, we return the shortest
   list.
*)


(* 1. The greedy change algorithm repeatedly takes the largest bill that does
   not exceed the target amount. For example, to make $122 using the greedy
   algorithm, we first take a $91 bill, then a $28 bill, and finally three $1
   bills.

   Give an example where this greedy algorithm uses more Dream Dollar bills
   than the minimum possible.

   Hint: this is tricky. If you can't find a solution, you can implement the
   greedy algorithm and test it against your dynamic programming solutions
   later.
*)

let find_largest_element_smaller_than x xs =
  let rec aux acc = function
    | [] -> acc
    | y :: ys ->
      if y <= x then
        let acc' = match acc with
        | None -> Some y
        | Some z -> Some (max y z)
        in
        aux acc' ys
      else
        acc
  in
  aux None xs

let bills_greedy n denominations =
  let rec aux acc n =
    if n = 0 then
      acc
    else
      match find_largest_element_smaller_than n denominations with
      | None -> failwith "Error"
      | Some el -> aux (el :: acc) (n-el)
  in
  aux [] n

(* 2.i) Describe and analyze a recursive algorithm that computes, given an
   integer k, the shortest list of bills needed to make k Dream Dollars. (Don’t
   worry about making your algorithm fast; just make sure it’s correct.)
*)

let rec i_th_element i xs =
  match i, xs with
    | _, [] -> failwith "Error"
    | 0, x :: _ -> x
    | i, x :: xs -> i_th_element (i-1) xs

let rec bills_rec n =
  if n = 0 then [] else
  let sols = List.map (fun cand -> bills_rec (n-cand) @ [cand]) (List.filter (fun x -> x <= n) denominations)
  in let sols' = List.sort (fun s1 s2 -> compare (List.length s1) (List.length s2)) sols
  in match sols' with
      | [] -> failwith "Impossible"
      | s :: _ -> s
    
    (* match n, candidates with
      | 0, _ -> acc
      | _, [] -> failwith "Error"
      | n, x :: candidates ->
        aux (x :: acc) (n-x) (x :: candidates) *)
      
      


(* 2.ii) Draw the call tree of your recursive definition for n = 5 and identify
   which subproblems are repeated. Can you find an evaluation order that will
   allow you to compute your solutions bottom-up? *)

(*
   MAKE A DRAWING
*)


(* 2.iii) Describe a dynamic programming algorithm that computes, given an integer
   k, the shortest list of bills needed to make k Dream Dollars. (This one needs
   to be fast.)
*)

let memoiziraj f =
  let rezultati = Hashtbl.create 512 in
  let mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f

let bills_iter n = memoiziraj (bills_rec n)
