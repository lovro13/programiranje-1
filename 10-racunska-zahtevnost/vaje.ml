(* 
Natančno definirajte pogoje, da funkcija `f` uredi seznam. 
*)
(*f uredi seznam če 1. f: A List -> A List; ys = f xs; 2. za vsak i > 0 je y[i-1] <= f[i]; 3. ys je permutacija len(ys) = len(xs)*)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Vstavljanjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert y xs] vstavi [y] v že urejen seznam [xs] in vrne urejen
 seznam. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 9 [0; 2];;
 - : int list = [0; 2; 9]
 # insert 1 [4; 5];;
 - : int list = [1; 4; 5]
 # insert 7 [];;
 - : int list = [7]
[*----------------------------------------------------------------------------*)


let rec insert y xs =
  match xs with
  | [] -> [y]
  | x :: xs' ->
    if x <= y then x :: insert y xs'
    else y :: x :: xs'

let test1 = insert 5 []  (* Rezultat: [5] *)
let test2 = insert 3 [1; 4; 7]  (* Rezultat: [1; 3; 4; 7] *)
let test3 = insert 6 [1; 3; 4; 7]  (* Rezultat: [1; 3; 4; 6; 7] *)
let test4 = insert 0 [1; 2; 3]  (* Rezultat: [0; 1; 2; 3] *)
let test5 = insert 8 [1; 3; 5]  (* Rezultat: [1; 3; 5; 8] *)

(*----------------------------------------------------------------------------*]
 Prazen seznam je že urejen. Funkcija [insert_sort] uredi seznam tako da
 zaporedoma vstavlja vse elemente seznama v prazen seznam.
[*----------------------------------------------------------------------------*)

let rec insert_sort list = 
  let rec aux acc ost = 
    match ost with
    | [] -> acc
    | x :: xs -> aux (insert x acc) xs
  in
  aux [] list

let test6 = insert_sort [2; 3; 5; 1231; 2; 51351; 156]

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri urejanju z izbiranjem na vsakem koraku ločimo dva podseznama, kjer je prvi
 že urejen, drugi pa vsebuje vse elemente, ki jih je še potrebno urediti. Nato
 zaporedoma prenašamo najmanjši element neurejenega podseznama v urejen
 podseznam, dokler ne uredimo vseh. 

 Če pričnemo z praznim urejenim podseznamom, vemo, da so na vsakem koraku vsi
 elementi neurejenega podseznama večji ali enaki elementom urejenega podseznama,
 saj vedno prenesemo najmanjšega. Tako vemo, da moramo naslednji najmanjši člen
 dodati na konec urejenega podseznama.
 (Hitreje je obrniti vrstni red seznama kot na vsakem koraku uporabiti [@].)
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem na Tabelah
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri delu z tabelami (array) namesto seznami, lahko urejanje z izbiranjem 
 naredimo "na mestu", t.j. brez uporabe vmesnih kopij (delov) vhoda. Kot prej
 tabelo ločujemo na že urejen del in še neurejen del, le da tokrat vse elemente
 hranimo v vhodni tabeli, mejo med deloma pa hranimo v spremenljivki
 [boundary_sorted]. Na vsakem koraku tako ne izvlečemo najmanjšega elementa
 neurejenga dela tabele temveč poiščemo njegov indeks in ga zamenjamo z
 elementom na meji med deloma (in s tem dodamo na konec urejenega dela).
 Postopek končamo, ko meja doseže konec tabele.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [swap a i j] zamenja elementa [a.(i)] and [a.(j)]. Zamenjavo naredi
 na mestu in vrne unit.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let test = [|0; 1; 2; 3; 4|];;
 val test : int array = [|0; 1; 2; 3; 4|]
 # swap test 1 4;;
 - : unit = ()
 # test;;
 - : int array = [|0; 4; 2; 3; 1|]
[*----------------------------------------------------------------------------*)

let swap list i j = 
  let temp = list.(j) in
  list.(j) <- list.(i);
  list.(i) <- temp;
  ()

let test7 = [|0; 1; 2; 3; 4|]
let test8 = swap test7 1 4

(*----------------------------------------------------------------------------*]
 Funkcija [index_min a lower upper] poišče indeks najmanjšega elementa tabele
 [a] med indeksoma [lower] and [upper] (oba indeksa sta vključena).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 index_min [|0; 2; 9; 3; 6|] 2 4 = 3
[*----------------------------------------------------------------------------*)

let rec index_min arr lower upper = 
  if lower = upper then lower
  else let nov_min = index_min arr (lower + 1) upper in
  if arr.(lower) <= arr.(nov_min) then lower else nov_min

let test9 = index_min [|0; 2; 9; 3; 6|] 2 4

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort_array] implementira urejanje z izbiranjem na mestu. 
[*----------------------------------------------------------------------------*)

let selection_sort_array array = 
  let n = Array.length array - 1 in
  for i = 0 to n do
    swap array i (index_min array i n)
  done

let test10 = [|5; 3; 8; 4; 2; 7|]
let test11 = selection_sort_array test10

(*----------------------------------------------------------------------------*]
 Funkcija [min_and_rest list] vrne par [Some (z, list')] tako da je [z]
 najmanjši element v [list] in seznam [list'] enak [list] z odstranjeno prvo
 pojavitvijo elementa [z]. V primeru praznega seznama vrne [None]. 
[*----------------------------------------------------------------------------*)

let min_and_rest list =
  match list with
  | [] -> None
  | hd :: tl -> 
    let z = List.fold_left min hd list in
    let list' = 
      let rec odstrani_z lst elem = 
        match lst with
        | [] -> []
        | hd :: tl -> if hd == z then tl 
        else hd :: (odstrani_z tl elem) in
      odstrani_z list z
      in
    Some (z, list')

let test12 = [5; 3; 8; 4; 2; 7]
let test13 = min_and_rest test12

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort] je implementacija zgoraj opisanega algoritma.
 Namig: Uporabi [min_and_rest] iz prejšnje naloge.
[*----------------------------------------------------------------------------*)


let rec selection_sort list = 
  match list with
  | [] -> []
  | hd :: tl -> let par = min_and_rest list in
  match par with
  | None -> []
  | Some (z, list') -> z :: (selection_sort list')

let test14 = selection_sort test12
(*----------------------------------------------------------------------------*]
 Funkcija [randlist len max] generira seznam dolžine [len] z naključnimi
 celimi števili med 0 in [max].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let l = randlist 10 10 ;;
 val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
[*----------------------------------------------------------------------------*)

let rec randlist len max = 
  if len > 0 then (Random.int max) :: (randlist (len - 1) max)
  else []

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Sedaj lahko s pomočjo [randlist] primerjamo našo urejevalno funkcijo (imenovana
 [our_sort] v spodnjem primeru) z urejevalno funkcijo modula [List]. Prav tako
 lahko na manjšem seznamu preverimo v čem je problem.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 let test = (randlist 100 100) in (our_sort test = List.sort compare test);;
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

let test16 = 
  let lst = randlist 1000 100 in
  (selection_sort lst = List.sort compare lst)