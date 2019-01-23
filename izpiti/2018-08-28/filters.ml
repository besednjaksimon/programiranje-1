type 'a veriga =
  | Filter of ('a -> bool) * 'a list * 'a veriga
  | Ostalo of 'a list

let test = Filter ((fun x -> x < 0), [], Filter ((fun x -> x < 10), [], Ostalo []))
let test2 = Filter ((fun x -> x < 0), [-5; -7], Filter ((fun x -> x < 10), [7; 2], Ostalo [100]))

let rec vstavi x = function
  | Ostalo ys -> Ostalo (x :: ys)
  | Filter (f, xs, c) ->
    if f x then
      Filter (f, x :: xs, c)
    else
      Filter (f, xs, vstavi x c)

let rec poisci x = function
  | Ostalo ys -> List.mem x ys
  | Filter (f, xs, c) ->
    if f x then
      List.mem x xs
    else
      poisci x c

let rec izprazni_filtre = function
  | Ostalo ys -> (Ostalo [], ys)
  | Filter (f, xs, c) ->
    let prazna, seznami = izprazni_filtre c in
      (Filter (f, [], prazna), xs @ seznami)


let rec dodaj_filter f veriga =
  let prazna, seznam = izprazni_filtre veriga in
  let nova_veriga = Filter (f, [], prazna) in
  List.fold_right vstavi seznam nova_veriga
