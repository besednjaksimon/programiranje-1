(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)
Random.self_init ();;
(* 1.1) Definirajte funkcijo, ki vzame tri cela števila ter vrne njihov produkt.
   Primer: /zmnozi 2 3 4 = 24/ *)

let zmnozi a b c =
  a * b * c

(* 1.2) Definirajte funkcijo, ki vzame celo število x in celo število k, ter
   vrne vrednost izraza x^3 + k.
   Primer: /afin_kub 2 1 = 9/ *)

let afin_kub x k =
  int_of_float((float_of_int x) ** 3. +. float_of_int k)

(* 1.3) Definirajte funkcijo, ki vzame seznam in izračuna seznam vrednosti funkcije
   f(x) = x^3 + 2 za elemente vhodnega seznama.
   Primer: /vse_kubiraj_in_pristej_dva [1; 2; 3] = [3; 10; 29]/ *)

let reverse xs =
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] xs

let vse_kubiraj_in_pristej_dva xs =
  reverse (List.rev_map (fun x -> (afin_kub x 2)) xs)

(* 1.4) Definirajte funkcijo, ki varno vrne zadnji element seznama v primeru,
   da seznam ni prazen. Uporabite tip option.
   Primer: /zadnji_element [1; 2; 3] = Some 3/ *)

let zadnji_element xs =
  let xs = reverse xs in
  match xs with
  | [] -> None
  | x :: xs -> Some x

(* 1.5) Definirajte funkcijo, ki izračuna n-to Fibonaccijevo število.
   Pri tem upoštevamo začetna pogoja /fibonacci 0 = 1/ in /fibonacci 1 = 1/.
   Primer: /fibonacci 20 = 10946/ *)

 let fibonacci n =
  let rec aux a b n =
    if n = 0 then a
    else aux b (a + b) (n - 1)
  in
  aux 0 1 n

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)

type 'a drevo =
  | RoznoDrevo of 'a * 'a drevo list

(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = RoznoDrevo(1, []);;
let t' = RoznoDrevo(2, [t; t]);;
let t'' = RoznoDrevo(3, [RoznoDrevo((-1), []); t'; RoznoDrevo(0, [])]);;

(* 2.3) Definirajte funkcijo, ki preveri ali je dano rožno drevo list drevesa,
   torej ima prazen gozd poddreves. *)

let je_list (RoznoDrevo(k, l)) =
  match (k, l) with
  | (_, []) -> true
  | (_, _) -> false

(* 2.4) Definirajte funkcijo, ki preveri, ali drevo celih števil vsebuje zgolj pozitivna števila. *)

let rec vsa_pozitivna (RoznoDrevo(k, l)) =
  match (k, l) with
  | (k, []) ->
    begin if k < 0 then false else true
    end
  | (k, l) -> List.for_all (fun x -> x) (List.rev_map vsa_pozitivna l)

let drevcek = RoznoDrevo(5,[RoznoDrevo(3, [RoznoDrevo((-9), []); RoznoDrevo(9, [])]); RoznoDrevo(1, [RoznoDrevo(2, [])]);
  RoznoDrevo(2, [])]);;

(* 2.5) Definirajte funkcijo, ki izračuna največjo širino rožnega drevesa, torej največjo dolžino
   gozda, ki se pojavi v kateremkoli vozlišču rožnega drevesa. *)

let rec sirina_drevesa (RoznoDrevo(_, l)) : int =
  List.fold_left (fun n t -> max n (sirina_drevesa t)) (List.length l) l
    
(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)

(* Poljubno dolg seznam (do 10) samih n-jev *)
let poljuben_seznam n =
  let dolzina = 1 + Random.int 10 in
  let rec aux acc dolzina =
    if dolzina > 0 then
      aux (n :: acc) (dolzina - 1)
    else
      acc
  in
  aux [] dolzina

let rec globoko_drevo n =
  if n <= 0 then
    RoznoDrevo(1 + Random.int 10, []) 
  else
    RoznoDrevo(1 + Random.int 10, List.rev_map globoko_drevo (poljuben_seznam (n - 1)))

(* 2.7) Definirajte funkcijo, ki pretvori rožno drevo v seznam. Vrstni red vrednosti v seznamu
   pri tem ni pomemben.
   Primer: /drevo_v_seznam t'' = [3; -1; 2; 1; 1; 0]/ (ali katerakoli permutacija [3; -1; 2; 1; 1; 0])

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)

let drevo_v_seznam drevo =
  let rec aux acc = function
    | RoznoDrevo(x, []) -> x :: acc
    | RoznoDrevo(y, zs) -> List.fold_left aux (y :: acc) zs
  in 
  aux [] drevo 
