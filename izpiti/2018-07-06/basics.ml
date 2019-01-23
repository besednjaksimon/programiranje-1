let apply f x = f x

let revapply x f = f x

let drop n xs =
  if List.length xs < n then None
  else
    let rec aux acc n xs =
      match n, xs with
      | 0, _ -> Some (List.rev acc)
      | n, x :: xs -> aux (x :: acc) (n-1) xs
      | _, _ -> failwith "There is apparently another unmatched case."
    in
    aux [] n xs

let rec take n xs = ()

let take_tailrec n xs = ()
