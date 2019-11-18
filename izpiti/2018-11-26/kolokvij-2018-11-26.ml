(* -------- 1 -------- *)
let rec sum_tlrec ll =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (acc + x) xs
  in aux 0 ll
(* -------- 2 -------- *)
let rec je_narascujoc = function
  | [] | [_] -> true
  | x :: (x' :: _ as xs) -> x < x' && (je_narascujoc xs)
(* -------- 3 -------- *)
let rec vstavi n = function
  | [] -> [n]
  | x :: xs when x >= n -> n :: x :: xs
  | x :: xs -> x :: vstavi n xs

let rec uredi = function
  | [] -> []
  | x :: xs -> vstavi x (uredi xs)
(* -------- 4 -------- *)
let rec vstavi' n ll cmp =
  match ll with
  | [] -> [n]
  | x :: xs when (cmp n x) -> n :: x :: xs
  | x :: xs -> x :: vstavi' n xs cmp

let rec uredi' ll cmp =
  match ll with
  | [] -> []
  | x :: xs -> vstavi' x (uredi' xs cmp) cmp

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
let rec sort_flyers ll =
  uredi' ll (fun f f' ->
      match f.status, f'.status with
      | Staff, _ -> true
      | Passenger Top, Staff -> false
      | Passenger Top, _ -> true
      | (Passenger Group n), Staff -> false
      | (Passenger Group n), Passenger Top -> false
      | (Passenger Group n), Passenger Group m -> n >= m
    )
(* -------- 7 -------- *)
let rec group_by_status ll =
  let rec aux acc st = function
    | [] -> acc, []
    | x :: xs -> if x.status = st then aux (x :: acc) st xs else acc, (x :: xs)
  in let rec aux' acc = function
      | [] -> acc
      | x :: xs -> 
        let grp, xs' = aux [] x.status (x :: xs)
        in aux' (grp :: acc) xs'
  in List.rev (aux' [] (sort_flyers ll))