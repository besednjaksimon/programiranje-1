(* 1. naloga *)

(* a) *)

let podvoji_vsoto x y = 2 * (x + y)

(* b) *)

let povsod_vecji (x1, y1, z1) (x2, y2, z2) =
  x1 > x2 && y1 > y2 && z1 > z2

(* c) *)

let uporabi_ce_lahko f = function
  | None -> None
  | Some x -> Some (f x)

(* d) *)

let pojavi_dvakrat x l =
  let rec aux stevec = function
    | [] -> stevec
    | y :: ys ->
      if x = y then
        aux (stevec+1) ys
      else
        aux stevec ys
  in
  (aux 0 l) = 2
    
    

(* e) *)

let izracunaj_v_tocki x l =
  let rec aux acc = function
    | [] -> acc
    | f :: fs -> aux ((f x) :: acc) fs
  in
  aux [] l

(* f) *)

let eksponent x p =
  let rec aux acc = function
    | 0 -> acc
    | p -> aux (x * acc) (p-1)
  in
  aux 1 p
  