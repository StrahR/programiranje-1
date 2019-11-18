(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
  Namig: Definirajte pomožno funkcijo za obračanje seznamov.
  [*----------------------------------------------------------------------------*)

let rec reverse list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in aux [] list

(*----------------------------------------------------------------------------*]
  Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
  vrednosti [n] funkcija vrne prazen seznam.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # repeat "A" 5;;
    - : string list = ["A"; "A"; "A"; "A"; "A"]
    # repeat "A" (-2);;
    - : string list = []
    [*----------------------------------------------------------------------------*)

let rec repeat x n = 
  let rec aux acc x = function
    | n when n > 0 -> aux (x :: acc) x (n-1)
    | _ -> acc
  in aux [] x n

(*----------------------------------------------------------------------------*]
  Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
  vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
  Funkcija je repno rekurzivna.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # range 10;;
    - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
    [*----------------------------------------------------------------------------*)

let rec range n =
  let rec aux acc = function
    | n when n >= 0 -> aux (n :: acc) (n-1)
    | _ -> acc
  in aux [] n

(*----------------------------------------------------------------------------*]
  Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
  funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
  [f x0; f x1; f x2; ...].
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # let plus_two = (+) 2 in
    map plus_two [0; 1; 2; 3; 4];;
    - : int list = [2; 3; 4; 5; 6]
    [*----------------------------------------------------------------------------*)

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: xs
(*----------------------------------------------------------------------------*]
  Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # let plus_two = (fun x -> x + 2) in
    map_tlrec plus_two [0; 1; 2; 3; 4];;
    - : int list = [2; 3; 4; 5; 6]
    [*----------------------------------------------------------------------------*)

let rec map_tlrec f list = 
  let rec aux acc f = function
    | [] -> reverse acc
    | x :: xs -> aux (f x :: acc) (f) xs
  in aux [] f list


(*----------------------------------------------------------------------------*]
  Funkcija [mapi] je ekvivalentna python kodi:

  def mapi(f, list):
      mapi_list = []
      index = 0
      for x in list:
          mapi_list += [f(x, index)]
          index += 1
      return mapi_list

  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # mapi (+) [0; 0; 0; 2; 2; 2];;
    - : int list = [0; 1; 2; 5; 6; 7]
    [*----------------------------------------------------------------------------*)

let rec mapi f list = 
  let rec aux acc n f = function
    | [] -> reverse acc
    | x :: xs -> aux (f x n :: acc) (n+1) (f) xs
  in aux [] 0 f list

(*----------------------------------------------------------------------------*]
  Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
  elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # zip [1; 1; 1; 1] [0; 1; 2; 3];;
    - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
    # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
    Exception: Failure "Different lengths of input lists.".
    [*----------------------------------------------------------------------------*)

let rec zip xs' ys' =
  let rec aux acc xs' ys' =
    match (xs', ys') with
    | [], [] -> reverse acc
    | x :: xs, y :: ys -> aux ((x, y) :: acc) xs ys
    | _ -> failwith "Different lengths of input lists."
  in aux [] xs' ys'

(*----------------------------------------------------------------------------*]
  Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
  [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # unzip [(0,"a"); (1,"b"); (2,"c")];;
    - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
    [*----------------------------------------------------------------------------*)

let rec unzip = function
  | [] -> [], []
  | (x, y) :: tl ->
    let xs, ys = unzip tl in
    (x :: xs, y :: ys)

(*----------------------------------------------------------------------------*]
  Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
    - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
    [*----------------------------------------------------------------------------*)

let rec unzip_tlrec list =
  let rec aux acc' acc'' = function
    | [] -> (reverse acc', reverse acc'')
    | (a, b) :: xs -> aux (a :: acc') (b :: acc'') xs
  in aux [] [] list

(*----------------------------------------------------------------------------*]
  Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
      while condition(x):
          x = f(x)
      return x

  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # loop (fun x -> x < 10) ((+) 4) 4;;
    - : int = 12
    [*----------------------------------------------------------------------------*)

let rec loop condition f x =
  if condition x
  then loop condition f (f x)
  else x

(*----------------------------------------------------------------------------*]
  Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
  funkcijo dveh argumentov [f] in vrne vrednost izračuna
  f(... (f (f x0 x1) x2) ... xn).
  V primeru seznama z manj kot dvema elementoma vrne napako.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
    - : string = "FICUS"
    [*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f = function
  | [] | _ :: [] -> failwith "Seznam naj vsebuje vsaj 2 elementa."
  | [a; b] -> f a b
  | a :: b :: xs -> (fold_left_no_acc f ((f a b) :: xs))

(*----------------------------------------------------------------------------*]
  Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
  vrednosti [x] do vključno [n]-te uporabe, torej
  [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
  Funkcija je repno rekurzivna.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # apply_sequence (fun x -> x * x) 2 5;;
    - : int list = [2; 4; 16; 256; 65536; 4294967296]
    # apply_sequence (fun x -> x * x) 2 (-5);;
    - : int list = []
    [*----------------------------------------------------------------------------*)

let rec apply_sequence f x n = 
  let rec aux acc f x = function
    | n when n > 0 -> 
      let fx = f x in
      aux (fx :: acc) f fx (n-1)
    | _ -> reverse acc
  in aux [] f x n
(*----------------------------------------------------------------------------*]
  Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
  vrne vrednost [true].
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # filter ((<)3) [0; 1; 2; 3; 4; 5];;
    - : int list = [4; 5]
    [*----------------------------------------------------------------------------*)

let rec filter f list =
  let rec aux acc f = function
    | [] -> reverse acc
    | x :: xs ->
      if f x
      then aux (x :: acc) f xs
      else aux acc f xs
  in aux [] f list

(*----------------------------------------------------------------------------*]
  Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
  obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
  Funkcija je repno rekurzivna.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
    - : bool = true
    # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
    - : bool = false
    [*----------------------------------------------------------------------------*)

let rec exists f = function
  | [] -> false
  | x :: xs ->
    if f x
    then true
    else exists f xs

(*----------------------------------------------------------------------------*]
  Funkcija [first f default list] vrne prvi element seznama, za katerega
  funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
  Funkcija je repno rekurzivna.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
    - : int = 5
    # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
    - : int = 0
    [*----------------------------------------------------------------------------*)

let rec first f default = function
  | [] -> default
  | x :: xs ->
    if f x
    then x
    else first f default xs
