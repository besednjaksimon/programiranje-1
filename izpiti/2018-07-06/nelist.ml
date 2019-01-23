type 'a nelist =
  | Base of 'a
  | Cons of 'a * 'a nelist

let head = function
  | Base x -> x
  | Cons (x, _) -> x

let rec last = function
  | Base x -> x
  | Cons (x, y) -> last y

let tail = function
  | Base _ -> None
  | Cons (_, t) -> Some t

let length xs =
  let rec aux acc = function
    | Base _ -> acc + 1
    | Cons (_ ,t) -> aux (acc+1) t
  in
  aux 0 xs

let list_of_nelist xs =
  let rec aux acc = function
    | Base x -> List.rev (x :: acc)
    | Cons (x, t) -> aux (x :: acc) t
  in
  aux [] xs

let rec fold f s = function
  | Base x -> f s x
  | Cons (x, t) -> fold f (f s x) t
