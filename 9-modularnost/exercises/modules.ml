(* ========== Exercise 8: Modules  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 "Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Define a module type [NAT] that specifies the structure of a "natural numbers 
 structure". It should have a carrier type, a function that tests for equality, 
 a zero and a one element, addition, subtraction, and multiplication operations 
 and conversion functions from and to OCaml's [int] type.

 Note: Conversion functions are usually named [to_int] and [of_int], so that
 when used, the function name [NAT.of_int] tells you that you a natural number
 from an integer.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t

  val eq   : t -> t -> bool
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val to_int : t -> int 
  val of_int : int -> t

end

(*----------------------------------------------------------------------------*]
 Write a module that implements the [NAT] signature, using OCaml's [int] type 
 as the carrier.

 Trick: until you're done implementing [Nat_int], it won't have the required
 signature. You can add stubs with [failwith "later"] to make the compiler 
 happy and leave a note for yourself, however it does not work with constants. 
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct

  type t = int
  let eq x y = x = y
  let zero = 0
  let one = 1
  let add x y = ( + ) x y
  let sub x y = max 0 (x - y)
  let mult = ( * )
  let to_int n = n
  (* [of_int k] if k < 0 then this function
      - fails with an error?
      - uses 0 instead?
      - uses absolute value instead?
       *)
  let of_int k = max 0 k 

end

(*----------------------------------------------------------------------------*]
 Write another implementation of [NAT], taking inspiration from the Peano
 axioms: https://en.wikipedia.org/wiki/Peano_axioms

 First define the carrier type with two constructors, one for zero and one for
 the successor of another natural number.
 Most of the functions are defined using recursion, for instance the equality 
 of [k] and [l] is decided by recursion on both [k] and [l] where the base case
 is that [Zero = Zero].
[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = Zero | S of t
  let rec eq x y =
    match (x, y) with
    | (Zero, Zero) -> true
    | (Zero, S _) -> false
    | (S _, Zero) -> false
    | (S x, S y) -> eq x y
  let zero = Zero
  let one = S (Zero)
  let rec add x y =
    match (x, y) with
    | (x, Zero) -> x
    | (x, S y) -> add (S x) y
  let rec sub x y =
    match (x, y) with
    | (_, Zero) -> x
    | (Zero, _) -> Zero
    | (S x, S y) -> sub x y
  let rec mult x y =
    match (x, y) with
    | (x, Zero) -> Zero
    | (Zero, y) -> Zero
    | (S x, y) -> add y (mult x y)
  let rec to_int = function
    | Zero -> 0
    | S x -> 1 + to_int x
  let rec of_int n =
    if n <= 0 then Zero
    else S (of_int (n-1))

end

(*----------------------------------------------------------------------------*]
 OCaml modules are first class and can be passed to functions as arguments by
 using the keyword [module]. The function definition is then

 # let f (module M : M_sig) = ...

 and passing a module as an argument is done by

 # f (module M_implementation);;

 The function [sum_nat_100] accepts a module of type [NAT] and using the module
 sums the first 100 natural numbers. Because the function cant return something
 of the type [NAT.t] (because we don't know what module it belongs to, it could
 be an [int] or a variant type) it returns an [int] that we get with the method
 [to_int].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_nat_100 (module Nat_int);;
 - : int = 4950
 # sum_nat_100 (module Nat_peano);;
 - : int = 4950
[*----------------------------------------------------------------------------*)

let sum_nat_100 (module Nat : NAT) =
  let hundred = Nat.of_int 100 in
  let rec sum counter acc =
    if Nat.eq counter hundred then
      acc
    else
      sum (Nat.add counter Nat.one) (Nat.add acc counter)
  in
  sum Nat.zero Nat.zero |> Nat.to_int

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Now we follow the fable told by John Reynolds in the introduction.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Define the signature of a module of complex numbers.
 We will need a carrier type, a test for equality, zero, one, i, negation and
 conjugation, addition, and multiplication.
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t
  val eq : t -> t -> bool
  val zero : t
  val one : t
  val i : t
  val neg : t -> t
  val conj : t -> t
  val add : t -> t -> t
  val mult : t -> t -> t
end

(*----------------------------------------------------------------------------*]
 Write an implementation of Professor Descartes's complex numbers. This should 
 be the cartesian representation.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x = y
  let zero = {re = 0.; im = 0.}
  let one = {re = 1.; im = 0.}    
  let i = {re = 0.; im = 1.}
  let neg = function
    | {re = x; im = y} -> {re = -. x; im = -. y}
  let conj = function
    | {re = x; im = y} -> {re = x; im = -. y}
  let add x y =
    match (x, y) with
    | ({re = x1; im = y1}, {re = x2; im = y2}) ->
      {re = x1 +. x2; im = y1 +. y2}
    (* {re = x.re +. y.re; im = x.im +. y.im} *)
  let mult x y =
    match (x, y) with
    | ({re = x1; im = y1}, {re = x2; im = y2}) ->
      {re = (x1 *. x2) -. (y1 *. y2); im = (x1 *. y2) +. (x2 *. y1)}
    (*
    let re = x.re *. y.re -. x.im *. y.im in
    let im = x.im *. y.re +. x.re *. y.im in
    {re; im} 
       *)

end

(*----------------------------------------------------------------------------*]
 Now implement Professor Bessel's complex numbers. The carrier this time
 will be a polar representation, with a magnitude and an argument for each
 complex number.

 Recommendation: Implement addition at the end, as it gets very messy (might
 as well be the end of the century).
[*----------------------------------------------------------------------------*)


module Polar (* : COMPLEX *) = struct

  type t = {magn : float; arg : float}

  (* Auxiliary functions to make life easier. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  let eq x y =
    match (x, y) with
    | ({magn = r1; arg = b1}, {magn = r2; arg = b2}) ->
      let diff = (b2 -. b1) /. pi in
      r1 = r2 && diff = snd (modf diff)
  let zero = {magn = 0.; arg = 0.}
  let one = {magn = 1.; arg = 0.}
  let i = {magn = 1.; arg = pi /. 2.}
  let neg = function
    | {magn = r; arg = b} -> {magn = r; arg = b +. pi}
  let conj = function
    | {magn = r; arg = b} -> {magn = r; arg = -.b}
  let add x y =
    match (x, y) with
    | ({magn = r1; arg = b1}, {magn = r2; arg = b2}) ->
      let (x1, y1, x2, y2) = (r1 *. cos b1, r1*. sin b1, r2 *. cos b2, r2 *. sin b2) in
      let (c, d) = (x1 +. x2, y1 +. y2) in
      {magn = sqrt (c ** 2. +. d ** 2.); arg = atan (d /. c)}
  let mult x y =
    match (x, y) with
    | ({magn = r1; arg = b1}, {magn = r2; arg = b2}) ->
      {magn = r1 *. r2; arg = b1 +. b2}

end


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DICTIONARIES
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 In the tree exercises we defined a type of dictionaries [('key, 'value) dict],
 that also had functions [dict_get], [dict_insert] and [print_dict]. Write a
 fitting signature for dictionaries [DICT] and construct it's implementation
 in the same way as in the tree exercises.

 The module should include an [empty] dictionary and functions [get], [insert]
 and [print] (where print should again work only on [(string, int) t)].
[*----------------------------------------------------------------------------*)

module type DICT = sig

  type ('key, 'value) t
  val empty : ('key, 'value) t
  val get : 'key -> ('key, 'value) t -> 'value option
  val insert : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
  val print : (string, int) t -> unit

end

module Tree_dict : DICT = struct

  type ('key, 'value) t =
    | D_Empty
    | D_Node of ('key, 'value) t * 'key * 'value * ('key, 'value) t

  let empty = D_Empty
  let rec get key dict =
    match dict with
    | D_Empty -> None
    | D_Node (l, k, v, r) ->
      if key = k then
        Some v
      else if key < k then
        get key l
      else
        get key r
  let rec insert key value dict =
    match dict with
    | D_Empty -> D_Node (D_Empty, key, value, D_Empty)
    | D_Node (l, k, v,  r) ->
      if key = k then
        D_Node (l, k, value, r)
      else if key < k then
        D_Node (insert key value l, k, v, r)
      else
        D_Node (l, k, v, insert key value r)
  let rec print = function
  | D_Empty -> ()
  | D_Node (d_l, k, v, d_r) -> (
    print d_l;
    print_string (k ^ " : "); print_int v; print_newline ();
    print d_r)
  
end
  
(*----------------------------------------------------------------------------*]
 The function [count (module Dict) list] counts how often certain elements
 appear in [list] using the chosen dictionary module and prints it.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let count (module Dict : DICT) list =
  let rec counter dict = function
  | [] -> Dict.print dict
  | x :: xs -> 
    let new_count = 
      match Dict.get x dict with
      | Some x -> x + 1 
      | None -> 1
    in
    let new_dict = Dict.insert x new_count dict in
    counter new_dict xs
in
counter Dict.empty list
