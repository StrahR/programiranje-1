(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root n m = n >= 0 && n * n = m

let pack3 a b c = a, b, c

let sum_if_not p =
  let rec aux acc = function
    | [] -> acc
    | x :: xs when not (p x) -> aux (x + acc) xs
    | _ :: xs -> aux acc xs
  in aux 0


let tlrec_rev ll =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in aux [] ll

let apply funcs =
  let funcs = funcs in
  let rec aux (acc1 : 'a list list) (acc2 : 'a list) fl xl =
    match xl with
    | [] -> tlrec_rev acc1
    | x :: xs ->
      match fl with
      | [] -> aux (acc2 :: acc1) [] funcs xs
      | f :: fs -> aux acc1 (f x :: acc2) fs (x :: xs)
  in aux [] [] funcs

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = Predavanja | Vaje

type srecanje = {predmet : string; vrsta : vrsta_srecanja; trajanje: int}

type urnik = srecanje list list

let vaje : srecanje = {predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 3}
let predavanje : srecanje = {predmet = "Programiranje 1"; vrsta = Predavanja; trajanje = 2}

let urnik_profesor : urnik = [
  [{predmet = "LMN"; vrsta = Vaje; trajanje = 2}]; (* pon *)
  []; (* tor *)
  [{predmet = "LMN"; vrsta = Predavanja; trajanje = 1}]; (* sre *)
  []; (* cet *)
  []; (* pet *)
  [{predmet = "LMN"; vrsta = Vaje; trajanje = 1}]; (* sob *)
]

let preobremenjen_profesor : urnik = [[{predmet = ""; vrsta = Vaje; trajanje = 7}]]
let preobremenjen_profesor' : urnik = [[{predmet = ""; vrsta = Predavanja; trajanje = 7}]]
let preobremenjen_profesor'' : urnik = [[
    {predmet = ""; vrsta = Predavanja; trajanje = 4};
    {predmet = ""; vrsta = Vaje; trajanje = 4}
  ]]

let je_preobremenjen (ll : urnik) =
  let rec aux (pr, vaje) = function
    | [] -> pr, vaje
    | x :: xs -> 
      match x.vrsta with
      | Predavanja -> aux (pr+x.trajanje, vaje) xs
      | Vaje -> aux (pr, vaje+x.trajanje) xs
  in let st_ur_v_dnevu = aux (0, 0)
  in let rec vse_pod_mejami = function
      | [] -> false
      | dan :: xs -> 
        let pr, v = st_ur_v_dnevu dan
        in if pr > 4 || v > 4 then true
        else vse_pod_mejami xs
  in vse_pod_mejami ll
let bogastvo (ll : urnik) =
  let koliko x =
    match x.vrsta with
    | Vaje -> x.trajanje
    | Predavanja -> 2 * x.trajanje
  in let rec prestej = function
      | [] -> 0
      | x :: xs -> koliko x + prestej xs
  in let rec aux acc = function
      | [] -> prestej acc
      | x :: xs -> aux (x @ acc) xs
  in aux [] ll

(* manjka še funkcija koliko ^ *)