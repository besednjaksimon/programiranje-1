(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

Random.self_init ();;

(* 1.1) Definirajte funkcijo, ki vzame par in zamenja komponenti para.
   Primer: /obrni (2, 4) = (4, 2)/ *)

 let obrni (x, y) = (y, x)

(* 1.2) Definirajte funkcijo, ki vzame par p in vrednost x in zamenja drugo
   komponento para p z x.
   Primer: /zamenjaj_drugo (2, 4) 7 = (2, 7)/ *)

let zamenjaj_drugo (x, y) z = (x, z)

(* 1.3) Definirajte funkcijo, ki vzame seznam parov in izračuna nov seznam parov,
   ki imajo drugo komponento zamenjano z 42.
   Primer: /vsem_zamenjaj_drugo_z_42 [(12, 1); (2, 4)] = [(12, 42); (2, 42)]/ *)

let vsem_zamenjaj_drugo_z_42 list =
  let rec aux acc = function
    | [] -> List.rev acc
    | (x, y) :: l -> aux ((x, 42) :: acc) l
  in
  aux [] list

(* 1.4) Definirajte funkcijo, ki varno vrne glavo seznama v primeru, ko seznam ni prazen.
   Uporabite tip option.
   Primer: /glava [1; 2; 3] = Some 1/ *)

let glava = function
  | [] -> None
  | x :: xs -> Some x

(* 1.5) Definirajte funkcijo, vzame funkcijo (f: 'a -> 'b), neko vrednost (x : 'a) in
   celo število n. Funkcija naj vrne vrednost, ki jo dobimo če f n-krat uporabimo na x,
   torej f (f ... (f x)...).
   Primer: /uporabi_veckrat succ 0 420 = 420/ *)

let uporabi_veckrat f x n =
  let rec aux x = function
    | n when n <= 0 -> x
    | n -> aux (f x) (n-1)
  in
  aux x n

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

(* 2.3) Definirajte funkcijo, ki vrne gozd rožnega drevesa. *)

let vrni_gozd = function
  | RoznoDrevo(_, gozd) -> gozd

(* 2.4) Definirajte funkcijo, ki izpiše vse vrednosti v rožnem drevesu celih števil.
   Števila naj bodo v ločenih vrsticah. Uporabite (print_int : int -> unit) in
   (print_newline : unit -> unit). *)

let rec izpisi_vrednosti = function
  | RoznoDrevo(x, []) -> print_string (string_of_int x ^ "\n");
  | RoznoDrevo(x, l) -> 
    print_string (string_of_int x ^ "\n");
    List.iter izpisi_vrednosti l

(* 2.5) Definirajte funkcijo, ki izračuna globino rožnega drevesa, t.j. dolžino
   najdaljše poti od korena do lista. *)

let rec globina (RoznoDrevo(_, l)) : int =
  match l with
  | [] -> 1
  | _ -> List.fold_left (fun n t -> max (n+1) (globina t)) 0 l

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
  if n <= 1 then
    RoznoDrevo(1 + Random.int 10, []) 
  else
    RoznoDrevo(1 + Random.int 10, List.rev_map globoko_drevo (poljuben_seznam (n - 1)))

let rec stevilo_vrednosti (RoznoDrevo (_, l)) =
  match l with
  | [] -> 1
  | l -> List.fold_left (fun n t -> n + stevilo_vrednosti t) 1 l

(* 2.7) Definirajte funkcijo, ki sprejme funkcijo (f : 'b -> 'a -> 'b) in začetno vrednost (acc : 'b)
   in funkcijo f zloži [fold] preko drevesa (t : 'a drevo). Vrstni red pri tem ni pomemben.

   Za primer t' želimo vrednost f (f (f acc 1) 2) 2)  (lahko tudi f (f (f acc 2) 1) 2))
   Primer: /zlozi (fun acc x -> x + acc) 0 t'' = 6/

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)

let zlozi f acc drevo =
  let rec aux acc drevo =
    match drevo with
    | RoznoDrevo (x, []) -> f acc x
    | RoznoDrevo (x, l) -> List.fold_left aux (f acc x) l
  in
  aux acc drevo
