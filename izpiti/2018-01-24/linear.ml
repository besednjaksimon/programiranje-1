(* ==========================================================================
   NALOGA 3 [Razlaga]

   Linearne preslikave na vektorjih lahko predstavimo tako s funkcijami kot z
   matrikami.

   Vektorje in matrike bomo predstavljali s pari oz. četvorci.

   |x|                 |a  b|
   |y| = (x, y)        |c  d| = (a, b, c, d)

   Spodaj je že napisana signatura modula, ki predstavlja linearne funkcije.
   ========================================================================== *)

module type Linear = sig
  (* Osnovni tip modula. *)
  type t
  (* Identiteta. *)
  val id : t
  (* Izračun funkcije na podatkih. *)
  val apply : int*int -> t -> int*int
  (* Funkcija, ki sprejme matriko in jo pretvori v osnovni tip modula.
     Če je osnovni tip modula matrika, pretvori matriko v matriko. *)
  val of_matrix : int*int*int*int -> t
  (* Funkcija, ki sprejme funkcijo in jo pretvori v osnovni tip modula. 
     Če je osnovni tip modula matrika, pretvori funkcijo v matriko. *)
  val of_function : ((int*int) -> (int*int)) -> t
  (* Vrne kompozitum dveh preslikav. *)
  val compose : t -> t -> t
end

(* ==========================================================================
   NALOGA 3.1

   Napišite modul, ki za predstavitev linearnih preslikav uporablja matrike.
   ========================================================================== *)

module Matrix (* : Linear *) = struct
  type t = int * int * int * int
  let id = (1, 0, 0, 1)
  let apply (x, y) (a, b, c, d) = (a*x + b*y, c*x + d*y)
  let of_matrix m = m
  let of_function f =
    let (a, b) = f (1, 0)
    and (c, d) = f (0, 1) in
    (a, b, c, d)
  let compose (a1, b1, c1, d1) (a2, b2, c2, d2) =
    (a1*a2 + b1*c2, a1*b2 + b1*d2, c1*a2 + d1*c2, c1*b2 + d1*d2)
end

(* ==========================================================================
   NALOGA 3.2

   Napišite modul, ki za predstavitev linearnih preslikav uporablja funkcije.
   ========================================================================== *)

module Function (* : Linear *) = struct
  type t = (int*int) -> (int*int)
  let id = fun x -> x
  let apply (x, y) f = f (x, y)
  let of_matrix (a, b, c, d) =
    fun (x, y) -> (a*x + b*y, c*x + d*y)
  let of_function f = f
  let compose f g = fun x -> f (g x)
end
