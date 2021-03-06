(* ========== Exercises 6: Dynamic programing  ========== *)


(*----------------------------------------------------------------------------*]
 The gluttonous mouse is located in the left upper corner of a matrix. It can
 only move one field down or one field right and at the end it must arrive at
 the lower right corner. On every square of the field there is a given
 (non-negative) amount of cheese. The mouse wants to eat as much as possible
 and its trying to figure out the optimal way.

 Write the function [max_cheese cheese_matrix] that given a matrix of cheese
 amounts calculates the overall amount of cheese that the mouse will eat if it
 follows the optimal way.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

let max_cheese cheese_matrix =
  let dimx = Array.length cheese_matrix in
  if dimx = 0 then 0 else
  let dimy = Array.length cheese_matrix.(0) in
  let rec best_path x y =
    print_endline(string_of_int x ^ ", " ^ (string_of_int y));
    let current_cheese = cheese_matrix.(x).(y) in
    let best_down = if (y+1) = dimy then 0 else best_path x (y+1)
    and best_right = if (x+1) = dimx then 0 else best_path (x+1) y in
    current_cheese + max best_right best_down
  in
  best_path 0 0


(*----------------------------------------------------------------------------*]
 We are solving the problem of alternatingly colored towers. There are four
 different types of building blocks, two of them blue and two red. The blue
 blocks have heights 2 and 3 and the red ones 1 and 2.

 Write the function [alternating_towers] for a given height calculates the
 number of different towers of given height that we can build using alternatingly
 colored blocks (red on blue, blue on red etc.). We may start with any color.

 Hint: Use two mutually recursive auxilary functions using the keyword "and".
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)

let rec red = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> blue (n - 1) + blue (n - 2)
and blue = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> red (n - 2) + red (n - 3)

let alternating_towers = function
  | 0 -> 1
  | n -> red n + blue n

(*----------------------------------------------------------------------------*]
 You have won a coupon for Mercator, allowing you to purchase any articles in
 the shop, whose total weight does not exceed [max_w] kilograms. Write a
 function [best_value articles max_w], which computes the largest price sum,
 that we can get for the coupon, where we can take as many articles of the
 same kind as we want. Then write a function [best_value_uniques articles max_w]
 where only one article of the same kind may be pruchased.

 Hint: The module [Array] offers many functions such as [map], [fold_left],
 [copy] and similar, which can be used as an alternative to loops.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # best_value articles 1.;;
 - : float = 10.95
 # best_value_unique articles 1.;;
- : float = 7.66
[*----------------------------------------------------------------------------*)

(* Articles are of form (name, price, weight) *)
let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]

let best_value articles w_max =
  let weight = function
    | (_, _, w) -> w
  and price = function
    | (_, p, _) -> p
  in
  let rec aux articles (p_sum, w_sum) =
    if w_sum <= 0. then p_sum
    else
      let buy article =
        if weight article > w_sum then (p_sum, 0.)
        else
          (p_sum +. price article, w_sum -. weight article)
      in
      Array.map buy articles
      |> Array.map (aux articles)
      |> Array.fold_left max 0.
  in aux articles (0., w_max)

let best_value_unique articles w_max =
  let options = Array.init (Array.length articles) (fun _ -> 1)
  in
  let weight = function
    | (_, _, w) -> w
  and price = function
    | (_, p, _) -> p
  in
  let rec aux articles (p_sum, w_sum, options) =
    if w_sum <= 0. then p_sum
    else
      let buy i article =
        if weight article > w_sum || options.(i) = 0
          then (p_sum, 0., options)
        else
          let options = Array.copy options
          in
          options.(i) <- 0;
          (p_sum +. price article, w_sum -. weight article, options)
      in
      Array.mapi buy articles
      |> Array.map (aux articles)
      |> Array.fold_left max 0.
  in aux articles (0., w_max, options)
  