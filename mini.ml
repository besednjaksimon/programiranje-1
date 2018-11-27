(* -------- 1 -------- *)

let vsota_seznama xs =
  let rec aux1 acc xs =
    match xs with
    | [] -> acc
    | glava :: rep -> aux1 (acc + glava) rep
  in
  aux1 0 xs

(* -------- 2 -------- *)

let rec ali_narasca seznam =
  match seznam with
  | []
  | [_] -> true
  | x :: y :: rep -> 
    if x <= y then
      ali_narasca (y :: rep)
    else
      false

(* -------- 3 -------- *)

let rec vstavi_celo n seznam =
  match seznam with
  | [] -> [n]
  | glava :: rep ->
    if glava <= n then
      glava :: vstavi_celo n rep
    else
      n :: seznam

let rec uredi seznam =
  match seznam with
  | [] -> []
  | glava :: rep -> vstavi_celo glava (uredi rep)

(* -------- 4 -------- *)

let rec vstavi_celo' cmp n seznam =
  match seznam with
  | [] -> [n]
  | glava :: rep ->
    if cmp glava n then
      glava :: vstavi_celo' cmp n rep
    else
      n :: seznam

let rec uredi' (cmp : 'a -> 'a -> bool) seznam =
  match seznam with
  | [] -> []
  | glava :: rep -> vstavi_celo' cmp glava (uredi' cmp rep)

(* -------- 5 -------- *)

type priority =
  | Top
  | Group of int

type status =
  | Staff
  | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]



(* -------- 6 -------- *)

let obrni xs =
  let rec aux1 acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> aux1 (x :: acc) xs
  in
  aux1 [] xs

let uredi_potnike seznam =
  let rec aux1 acc1 acc2 acc3 seznam =
    match seznam with
    | [] -> (acc1, acc2, acc3)
    | {status; name} :: seznam ->
      begin match status with
      | Staff -> aux1 ({status; name} :: acc1) acc2 acc3 seznam
      | Passenger Top -> aux1 acc1 ({status; name} :: acc2) acc3 seznam
      | Passenger (Group n) -> aux1 acc1 acc2 ({status; name} :: acc3) seznam
      end
  in
  let (osebje, potniki_top, potniki_ostali) = aux1 [] [] [] seznam in
  let cmp (flyer1 : flyer) (flyer2 : flyer) =
    match (flyer1, flyer2) with
      | ({status = Passenger Top; name = _}, _) -> failwith "Error"
      | (_, {status = Passenger Top; name = _}) -> failwith "Error"
      | ({status = Staff; name = _}, _) -> failwith "Error"
      | (_, {status = Staff; name = _}) -> failwith "Error"
      | ({status = Passenger (Group n); name = _}, {status = Passenger (Group m); name = _}) -> not (n < m) in
  let potniki_ostali = uredi' cmp potniki_ostali
  in
  osebje @ potniki_top @ potniki_ostali

(* -------- 7 -------- *)

let razdeli_potnike seznam =
  let potniki = uredi_potnike seznam in
  let rec aux acc1 acc2 potniki =
    match (potniki, acc2) with
    | ([], _) -> acc2 :: acc1
    | (glava :: rep, []) -> aux acc1 (glava :: acc2) rep
    | (glava1 :: rep1, glava2 :: rep2) ->
      if glava1.status = glava2.status then
        aux acc1 (glava1 :: glava2 :: rep2) rep1
      else
        aux ((glava2 :: rep2) :: acc1) [glava1] rep1
  in
  obrni (aux [] [] potniki)
