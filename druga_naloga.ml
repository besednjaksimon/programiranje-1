(* 2. naloga *)

(* a) *)

type 'a mm_drevo =
  | Empty
  | Node of 'a * int * 'a mm_drevo * 'a mm_drevo

(* b) *)

let testno_drevo = Node (2, 2, Node (1, 3, Empty, Empty), Node (5, 1, Node (4, 1, Empty, Empty), Node (8, 2, Empty, Empty)))

let rec vstavi drevo x =
  match drevo with
    | Empty -> Node (x, 1, Empty, Empty)
    | Node (y, s, l, r) ->
      if x < y then
        Node (y, s, vstavi l x, r)
      else if x > y then
        Node (y, s, l, vstavi r x)
      else
        Node (x, s+1, l, r)

(* c) *)

let rec multimnozica_iz_seznama = function
  | [] -> Empty
  | x :: xs -> vstavi (multimnozica_iz_seznama xs) x

(* d) *)

let rec velikost_multimnozice = function
  | Empty -> 0
  | Node (x, s, l, r) -> s + velikost_multimnozice l + velikost_multimnozice r

(* e) *)

(* Pomozna funkcija, ki vrne seznam k-tih ponovitev x. *)
let rec seznam_ponovitev x = function
  | 0 -> []
  | k -> x :: seznam_ponovitev x (k-1)

let rec seznam_iz_multimnozice = function
  | Empty -> []
  | Node (x, s, l, r) ->
    (seznam_iz_multimnozice l) @ (seznam_ponovitev x s) @ (seznam_iz_multimnozice r)

(* DODATNA NALOGA: *)

(* Repno rekurzivno stikanje seznamov *)
let stikanje_seznamov xs ys =
  let zs = List.rev xs in
  let rec aux xs ys =
    match xs with
      | [] -> ys
      | x :: xs -> aux xs (x :: ys)
  in
  aux zs ys

(* Pomozna funkcija (repno rekurzivna), ki vrne seznam k-tih ponovitev x. *)
let seznam_ponovitev' x k =
  let rec aux acc = function
    | 0 -> List.rev acc
    | k -> aux (x :: acc) (k-1)
  in
  aux [] k

let seznam_iz_multimnozice' drevo =
  match drevo with
    | Empty -> []
    | Node (x, s, l, r) ->
      (* S to pomozno funkcijo zajamemo elemente levega poddrevesa. *)
      let rec aux1 acc11 acc12 = function
        | Empty -> stikanje_seznamov acc11 acc12
        | Node (x1, s1, l1, r1) ->
          aux1 (stikanje_seznamov (seznam_ponovitev' x1 s1) acc11) (stikanje_seznamov acc12 (aux2 [] [] r1)) l1
      (* S to pomozno funkcijo zajamemo elemente desnega poddrevesa. *)
      and aux2 acc21 acc22 = function
        | Empty -> stikanje_seznamov acc21 acc22
        | Node (x2, s2, l2, r2) ->
          aux2 (stikanje_seznamov (aux1 [] [] l2) acc21) (stikanje_seznamov acc22 (seznam_ponovitev' x2 s2)) r2
      in
      stikanje_seznamov (aux1 [] [] l) (stikanje_seznamov (seznam_ponovitev' x s) (aux2 [] [] r))
