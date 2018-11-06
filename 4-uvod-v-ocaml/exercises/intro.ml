
(* ========== Exercise 1: Introduction to OCaml  ========== *)


(*----------------------------------------------------------------------------*]
 The function [penultimate_element] returns the second-to-last element of a
 list. If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)

let rec penultimate_element xs = 
 match xs with
 | [] -> failwith "List too short"
 | x :: _last :: [] -> x
 | x :: xs -> penultimate_element xs

(*----------------------------------------------------------------------------*]
 The function [get k list] returns the [k]-th element in the list [list].
 Numbering (as usual) starts with 0. If [k] is negative, the function returns
 the first element.
 If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k xs =
  if k < 0 then
    get 0 xs
  else
    match (k, xs) with
    | (_, []) -> failwith "List too short"
    | (0, x :: _) -> x
    | (k, x :: xs) -> get (k - 1) xs  

(*----------------------------------------------------------------------------*]
 The function [double list] doubles the occurences of elements in the list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double xs =
  match xs with
  | [] ->  []
  | x :: xs -> x :: x :: double xs
  (* x :: xs -> 
    let xs_doubled = double xs in
    x :: x :: xs_doubled *)

(*----------------------------------------------------------------------------*]
 The function [divide k list] divides the list into a pair of lists. The first
 list contains the first [k] elements of the list and the second contains the
 rest.
 When [k] is outside the bounds of [list], the appropriate list should be empty.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k xs =
    match (k, xs) with
    | (_, []) -> ([], [])
    | (k, xs) when k <= 0 -> ([], xs)
    | (k, x :: xs) ->
      let (l, r) = divide (k-1) xs in
      (x :: l, r)

(*----------------------------------------------------------------------------*]
 The function [delete k list] removes the [k]-th element of the list.
 If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec delete k xs =
  match (k, xs) with
  | (k, xs) ->
    let (l, r) = divide k xs in
    match (l, r) with
    | (_, []) -> failwith "List too short"
    | (l, s :: r) -> l @ r
    
(*----------------------------------------------------------------------------*]
 The function [slice i k list] returns the sub-list of [list] from the [i]-th
 up to (excluding) the [k]-th element. Suppose that [i] and [k] are fitting.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)

let rec slice i k xs =
  let (_, r1) = divide i xs in
  let (l2, r2) = divide (k-i) r1 in
  match (l2, r2) with
  | (l2, _) -> l2

(*----------------------------------------------------------------------------*]
 The function [insert x k list] inserts (not replaces) [x] into the list at the
 index [k].
 If [k] is outside of bounds of [list], insert the element at the beggining or
 the end instead.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k xs =
  if k < 0 then
    x :: xs
  else
    let (l, r) = divide k xs in
    match (l, r) with
    | (l, []) -> l @ [x]
    | (l, r) -> l @ x :: r

(*----------------------------------------------------------------------------*]
 The function [rotate n list] rotates the list to the left by [n] places.
 Suppose that [n] is within the bounds of [list].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n xs =
  let (l, r) = divide n xs in 
  r @ l 

(*----------------------------------------------------------------------------*]
 The function [remove x list] removes all occurrences of [x] in the list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x xs =
  match (x, xs) with
  | (x, []) -> []
  | (x, y :: xs) ->
    if x = y then
      remove x xs
    else
      y :: remove x xs

(*----------------------------------------------------------------------------*]
 The function [is_palindrome] checks if a list is a palindrome.
 Hint: Use an auxiliary function that reverses a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let is_palindrome xs =
  let rec reverse xs =
    match xs with
    | [] -> []
    | x :: xs -> reverse xs @ x :: []
  in 
  if reverse xs = xs then
    true
  else
    false
    
(*----------------------------------------------------------------------------*]
 The function [max_on_components] returns a list with the maximum element
 of the two given lists at the each index.
 The lenght of the returned list should be equal to the shorter of the lists.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components xs ys =
  match (xs, ys) with
  | ([], _) -> []
  | (_, []) -> []
  | (x :: xs, y :: ys) ->
    if x < y then
      y :: max_on_components xs ys
    else
      x :: max_on_components xs ys  

(*----------------------------------------------------------------------------*]
 The function [second_largest] returns the second largest value in the list.
 Multiple occurrences of the same element count as one value.
 Suppose the list contains at least two distinct values.
 Hint: Use an auxiliary function that finds the maximum element of a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec second_largest xs =
  let rec largest xs =
    match xs with
    | [] -> failwith "List is too short"
    | [x] -> x
    | x :: y :: xs ->
    if x < y then
      largest (y :: xs)
    else
      largest (x :: xs)
  in
  largest (remove (largest xs) xs)
