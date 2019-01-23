let razlika_kvadratov x y = (x + y) * (x + y) - x * x - y * y

let rec uporabi_na_paru f (x, y) = (f x, f y)

let rec ponovi_seznam n sez = 
  if n <= 0 then 
    [] 
  else
    sez @ ponovi_seznam (n-1) sez

let razdeli sez = 
  let rec aux acc1 acc2 = function
    | [] -> (acc1, acc2)
    | x :: xs ->
      if x < 0 then
        aux (x :: acc1) acc2 xs
      else
        aux acc1 (x :: acc2) xs
  in
  aux [] [] sez

