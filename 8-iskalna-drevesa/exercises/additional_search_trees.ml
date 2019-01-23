(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 ADDITIONAL EXERCISES 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 The function [bst_of_list] constructs a bst out of the elements of a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let leaf x = Node (Empty, x, Empty)

let test_tree =
  let left = Node(leaf 0, 2, Empty)
  and right = Node(leaf 6, 7, leaf 11)
  in
  Node(left, 5, right)

let rec insert x tree =
  match tree with
  | Empty -> leaf x
  | Node (l, y, r) ->
    if x = y then
      tree
    else if x < y then
      Node (insert x l, y, r)
    else
      Node (l, y, insert x r)
    
let rec list_of_tree = function
  | Empty -> []
  | Node (l, x, r) -> list_of_tree l @ [x] @ list_of_tree r

let is_bst tree =
  let xs = list_of_tree tree in
  let rec aux xs =
    match xs with
    | [] -> true
    | [x] -> true
    | x :: y :: tail ->
      if x < y then
        aux (y :: tail)
      else
        false
  in
  aux xs

let bst_of_list xs = List.fold_right insert xs Empty

(*----------------------------------------------------------------------------*]
 The function [tree_sort] sorts a list by transforming it to a tree and back.

 Note: Please do not actually use this in your code.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)

let tree_sort list =
  let t = bst_of_list list in
  list_of_tree t

(* 
let tree_sort list = list |> bst_of_list |> list_of_tree
*)

(*----------------------------------------------------------------------------*]
 The function [follow directions tree] of type [direction list -> 'a tree -> 
 'a option] accepts a list of directions for traversing the tree and returns the
 data in the node at the end of the traversal. Because the directions might not
 lead to an actual node in the tree, the result is returned as an [option] type.
 Don't forget to define the type [directions].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)

type direction =
  | Right
  | Left

let rec follow directions = function
  | Empty -> None
  | Node (l, x, r) ->
    (match directions with
     | [] -> Some x
     | Left :: t1 -> follow t1 l
     | Right :: t2 -> follow t2 r)

(*----------------------------------------------------------------------------*]
 The function [prune directions tree] finds the node given by [directions] and
 removes the subtree that starts in the node.

 Warning: When using [Some Node(l, x, r)] Ocaml complains because it reads it 
 as [(Some Node)(l, x, r)] so use paranthesis when necessary.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)

let rec prune directions tree =
  match directions, tree with
  | _, Empty -> None
  | [], _ -> Some Empty
  | Left :: t1, Node (l, x, r) ->
    (match prune t1 l with
     | None -> None
     | Some new_l -> Some (Node (new_l, x, r)))
  | Right :: t2, Node (l, x, r) ->
    (match prune t2 r with
     | None -> None
     | Some new_r -> Some (Node (l, x, new_r)))

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 PHANTOM TREES

 An additional approach to deletion is to modify the type of the tree. Define a
 new type of tree where nodes additionaly contain information about its state,
 which can either [Exist] or be a [Ghost] if the node is only used for searching
 but is not considered present. We assume that all trees are BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type state = Exists | Ghost

type 'a phantom_tree =
  | P_Empty
  | P_Node of 'a phantom_tree * 'a * 'a phantom_tree * state

(*----------------------------------------------------------------------------*]
 The function [phantomize] of type ['a tree -> 'a phantom_tree] maps a regular
 tree into a phantom tree.
 The function [kill x ptree] removes the element [x] from the tree by setting
 it's state to [Ghost].
 Assume that there are no repeated elements in input trees.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)

let rec phantomize = function
  | Empty -> P_Empty
  | Node (l, x, r) ->
    let p_l = phantomize l in
    let p_r = phantomize r in
    P_Node (p_l, x, p_r, Exists)

let rec kill x = function
  | P_Empty -> P_Empty
  | P_Node (p_l, y, p_r, s) ->
    if x = y then P_Node (p_l, y, p_r, Ghost)
    else if x < y then P_Node (kill x p_l, y, p_r, s)
    else P_Node (p_l, y, kill x p_r, s)

(*----------------------------------------------------------------------------*]
 The function [unphantomize] of type ['a phantom_tree -> 'a tree] maps a
 phantom tree into a regular one, keeping only nodes that exist (no ghosts
 allowed). The order of nodes in the output tree is not important.

 Hint: You may use a transformation to another data structure.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)

let unphantomize ptree =
  let rec list_of_ptree = function
    | P_Empty -> []
    | P_Node (p_l, _, p_r, Ghost) -> (list_of_ptree p_l) @ (list_of_ptree p_r)
    | P_Node (p_l, x, p_r, Exists) -> (list_of_ptree p_l) @ [x] @ (list_of_ptree p_r)
  in
  ptree |> list_of_ptree |> bst_of_list
