(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
  Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
  samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
  v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
  različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
  zanima, katero pot naj ubere.

  Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
  sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
  optimalni poti.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # max_cheese test_matrix;;
    - : int = 13
    [*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

let max_cheese cheese_matrix =
  let h = Array.length cheese_matrix
  and w = Array.length cheese_matrix.(0) in
  let tape = Array.make w 0 in
  (* tape.(w-1) <- cheese_matrix.(h-1).(w-1); *)
  for i = h - 1 downto 0 do
    for j = w - 1 downto 0 do
      let v = cheese_matrix.(i).(j) in
      let dv = max (tape.(j)) (if j = 2 then 0 else tape.(j+1)) in
      tape.(j) <- v + dv
    done
  done;
  tape.(0)

let max_cheese' chesse_matrix =
  let mem = Hashtbl.create 1000 in
  let rec aux acc x y =
    if x >= Array.length chesse_matrix || y >= Array.length chesse_matrix.(x) then acc
    else if Hashtbl.mem mem (x, y) then Hashtbl.find mem (x, y)
    else
      let s1 = aux (chesse_matrix.(x).(y) + acc) (x+1) y
      and s2 = aux (chesse_matrix.(x).(y) + acc) x (y+1) in
      let m = max (s1) (s2) in
      Hashtbl.add mem (x, y) m;
      m
  in aux 0 0 0

(*----------------------------------------------------------------------------*]
  Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
  različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
  3, rdeči pa višin 1 in 2.

  Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
  dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
  v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
  poljubne barve.

  Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # alternating_towers 10;;
    - : int = 35
    [*----------------------------------------------------------------------------*)
type colour = Blue | Red

let alternating_towers h =
  let mem = Hashtbl.create 1000 in
  Hashtbl.add mem (Red, 0) 0;
  Hashtbl.add mem (Red, 1) 1;
  Hashtbl.add mem (Red, 2) 1;
  Hashtbl.add mem (Blue, 0) 0;
  Hashtbl.add mem (Blue, 1) 0;
  Hashtbl.add mem (Blue, 2) 1;
  Hashtbl.add mem (Blue, 3) 2;
  let rec aux height col =
    if Hashtbl.mem mem (col, height)
    then Hashtbl.find mem (col, height)
    else
      let v = (function
          | Blue -> aux (height - 2) Red + aux (height - 3) Red
          | Red -> aux (height - 2) Blue + aux (height - 1) Blue) col
      in Hashtbl.add mem (col, height) v; v
  in aux h Blue + aux h Red

let alternating_towers' h =
  let mem = Hashtbl.create 1000 in
  Hashtbl.add mem (Red, 0) 0;
  Hashtbl.add mem (Red, 1) 1;
  Hashtbl.add mem (Red, 2) 1;
  Hashtbl.add mem (Blue, 0) 0;
  Hashtbl.add mem (Blue, 1) 0;
  Hashtbl.add mem (Blue, 2) 1;
  Hashtbl.add mem (Blue, 3) 2;
  let rec blue = function
    | h when Hashtbl.mem mem (Blue, h) -> Hashtbl.find mem (Blue, h)
    | h -> 
      Hashtbl.add mem (Blue, h) (red (h - 2) + red (h - 3));
      Hashtbl.find mem (Blue, h)
  and red = function
    | h when Hashtbl.mem mem (Red, h) -> Hashtbl.find mem (Red, h)
    | h ->
      Hashtbl.add mem (Red, h) (blue (h - 2) + blue (h - 1));
      Hashtbl.find mem (Red, h)
  in red h + blue h

let (<^>) x y = (x ^ " " ^ string_of_int y)

let alternating_towers'' h =
  let rec aux r1 r2 b1 b2 b3 = function
    | height when height = h -> b1 + r1
    | h -> aux (b2 + b3) r1 (r1 + r2) b1 b2 (h+1)
  in aux 1 0 1 0 0 0

(*----------------------------------------------------------------------------*]
  Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
  poljubne izdelke, katerih skupna masa ne presega [max_w] kilogramov. Napišite
  funkcijo [best_value articles max_w], ki poišče največjo skupno ceno, ki jo
  lahko odnesemo iz trgovine, kjer lahko vsak izdelek vzamemo večkrat, nato pa
  še funkcijo [best_value_uniques articles max_w], kjer lahko vsak izdelek
  vzamemo kvečjemu enkrat.

  Namig: Modul [Array] ponuja funkcije kot so [map], [fold_left], [copy] in
  podobno, kot alternativa uporabi zank.
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


(*----------------------------------------------------------------------------*]
  Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
  Poiščite vrednost najdražjega sprehoda od korena do listov drevesa.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # max_path Empty ;;
    - : 'a option = None
    # max_path test_tree;;
    - : int option = Some 21
    [*----------------------------------------------------------------------------*)

type 'a tree
  = Empty
  | Node of ('a tree) * 'a * ('a tree)

let leaf x = Node (Empty, x, Empty)

let test_tree = Node( Node(leaf 0, 2, leaf 13), 5, Node(leaf 9, 7, leaf 4))

(*----------------------------------------------------------------------------*]
  Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
  Poiščite najdražji sprehod od korena do listov drevesa: Funkcija pot vrne v 
  obliki seznama smeri, katere je potrebno izbrati za najdražji sprehod.

  Napišite tudi funkcijo, ki sprehod pretvori v elemente sprehoda
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # max_path_trace Empty ;;
    - : 'a list = []
    # max_path_trace test_tree;;
    - : direction list = [Right, Left]
    # reconstruct (max_path_trace test_tree);;
    - : int list = [5; 7; 9]
    [*----------------------------------------------------------------------------*)

type direcion 
  = Left
  | Right