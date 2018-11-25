(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame dve celi števili ter vrne njuno vsoto.
   Primer: /sestej 2 3 = 5/ *)

let sestej x y = 
  x + y

(* 1.2) Definirajte funkcijo, ki svojemu argumentu prišteje 3.
   Primer: /pristej_tri 10 = 13/ *)

let pristej_tri x =
  x + 3

(* 1.3) Definirajte funkcijo, ki vsem elementom seznama prišteje 5.
   Primer: /vsem_pristej_pet [1; 2] = [6; 7]/ *)

let reverse l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | x :: l -> aux (x :: acc) l
  in
  aux [] l

let vsem_pristej_pet l =
  let rec aux acc l =
    match l with
    | [] -> reverse acc
    | x :: l -> aux ((x + 5) :: acc) l
  in
  aux [] l

(*
let rec vsem_pristej_pet l =
  match l with
  | [] -> []
  | x :: l -> (x + 5) :: vsem_pristej_pet l
*)

(* 1.4) Definirajte funkcijo, ki vrne zadnjo komponento nabora s tremi elementi.
   Primer: /tretji (1, "horse", [None]) = [None]/ *)

let tretji trojica =
  match trojica with
    | (_, _, z) -> z

(* 1.5) Definirajte funkcijo, ki vzame dve funkciji ter vrne njun kompozitum.
   Primer: /kompozitum succ string_of_int 5 = "6"/ *)

let kompozitum f g x =
  f (g x)

(*
let kompozitum f g x =
  g (f x)

Za primer v navodilih dela ta, a primer najverjetneje ni pravi.
*)

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

(* 2.2) Napišite funkcijo, ki vrne koren danega rožnega drevesa. *)

let koren (RoznoDrevo(k, l)) =
  k

(* 2.3) Napišite funkcijo, ki preveri, ali drevo celih števil vsebuje kakšno negativno število. *)

let rec kaksno_negativno (RoznoDrevo(k, l)) =
  match (k, l) with
  | (k, []) ->
    begin if k < 0 then true else false
    end
  | (k, l) -> List.exists (fun x -> x) (List.rev_map kaksno_negativno l)
  
let drevcek = RoznoDrevo(5,[RoznoDrevo(3, [RoznoDrevo((-9), []); RoznoDrevo(9, [])]); RoznoDrevo(1, [RoznoDrevo(2, [])]);
RoznoDrevo(2, [])]);;

(* 2.4) Sestavite funkcijo, ki sprejme naravno število ter sestavi (poljubno)
   drevo, ki ima toliko otrok.
   Namig: napišite pomožno funkcijo, ki ustvari poljuben seznam dane dolžine. *)

Random.self_init ();;

let poljuben_seznam n =
  let rec aux acc n =
    if n > 0 then
      aux ((1 + Random.int 10) :: acc) (n-1)
    else
      acc
  in
  aux [] n

let drevo_z_veliko_otroci n =
  let seznam = poljuben_seznam n in
  let rec aux1 acc ints =
    match ints with
    | [] -> reverse acc
    | glava :: rep -> aux1 (RoznoDrevo(glava, []) :: acc) rep
  in
  let aux2 = aux1 [] seznam in
  match aux2 with
  | aux2 -> RoznoDrevo((1 + Random.int 10), aux2)


(* 2.5) Sestavite funkcijo, ki izračuna število vseh vozlišč v drevesu.
   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)

let rec velikost (RoznoDrevo(_,l)) =
  List.fold_left (fun n t -> n + velikost t) 1 l

let dre = drevo_z_veliko_otroci 10;;
