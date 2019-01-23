(* ==========================================================================
   NALOGA 2.1

   Filtracijsko drevo ima dve vrsti osnovnih gradnikov:
   - Vozlišča imajo celoštevilsko vrednost, levo poddrevo in desno poddrevo.
   - Listi oz. škatle imajo seznam celoštevilskih vrednosti.

   Primer:
                       10
                     /    \
                    5      15
                  /  \    /  \
                 [1] []  []  [19;20]

   Napišite tip, ki predstavlja filtracijsko drevo in nato konstruirajte
   zgornji primer.
   ========================================================================== *)

type filter_tree = 
  | Node of  int * filter_tree * filter_tree
  | Box of int list

let example_tree = Node (10,
                        Node (5, Box [1], Box []),
                        Node (15, Box [], Box [19; 20]))

(* ==========================================================================
   NALOGA 2.2

   Filtracijsko drevo razvršča števila v škatle glede na njihovo vrednost.
   Vozlišče z vrednostjo "k" razvrsti število "n" v levo poddrevo če velja
   n <= k oz. v desno poddrevo če velja n > k.
   Ko število doseže škatlo, ga dodamo v seznam števil v škatli.
   Škatle lahko vsebujejo ponovitve in niso nujno urejene.

   Napišite funkcijo, ki sprejme število in filtracijsko drevo in vrne
   filtracijsko drevo z vstavljenim številom.

   Primer:
               10                                        10
             /    \            insert 12 t             /    \
    t =     5      15          ------------>          5      15
          /  \    /  \                              /  \    /  \
         [1] []  []  [19;20]                       [1] [] [12] [19;20]
   ========================================================================== *)

let rec insert n = function
  | Box zs -> Box (n :: zs)
  | Node (k, l, r) ->
    if n <= k then Node (k, insert n l, r)
    else
      Node (k, l, insert n r)

(* ==========================================================================
   NALOGA 2.3

   Napišite funkcijo, ki sprejem seznam celih števil in filtracijsko drevo
   in vrne filtracijsko drevo z vstavljenimi elementi seznama.
   Vrstni red vstavljanja ni pomemben.
   ========================================================================== *)

let rec insert_many l ftree =
  List.fold_left (fun n t -> insert t n) ftree l

(* ==========================================================================
   NALOGA 2.4

   Definirajte funkcijo, ki sprejme filtracijsko drevo in preveri ali
   so vsa števila v pravilnih škatlah glede na način razvrščanja.

   Primer:
       5                                      5
     /   \    ----> true                    /   \    ----> false
   [1;2] [7]                              [1]   [2;7]
   ========================================================================== *)

let boxed_correctly ftree =
  match ftree with
  | Box xs -> true
  | Node (node, l, r) ->
    let rec aux_l node = function
      | Node (x, l, r) -> aux_l x l && aux_r x r
      | Box xs ->
        List.for_all ((>=) node) xs
    and aux_r node = function
      | Node (x, l, r) -> aux_l x l && aux_r x r
      | Box xs ->
      List.for_all ((<) node) xs
    in
    aux_l node l && aux_r node r
