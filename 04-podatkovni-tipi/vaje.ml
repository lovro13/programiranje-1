(*----------------------------------------------------------------------------*
 # Podatkovni tipi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Valute

 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute. Oglejmo si dva pristopa k izboljšavi
 varnosti pri uporabi valut.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tipa `euro` in `dollar`, kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število. Nato napišite funkciji
 `euro_to_dollar` in `dollar_to_euro`, ki primerno pretvarjata valuti (točne
 vrednosti pridobite na internetu ali pa si jih izmislite).

 Namig: Občudujte informativnost tipov funkcij.
[*----------------------------------------------------------------------------*)

type euro = Euro of float

type dollar = Dollar of float

let dollar_to_euro (Dollar vsota : dollar) : euro = Euro (vsota *. 0.9)

let euro_to_dollar (Euro vsota : euro) : dollar = Dollar (vsota  /. 0.9)


let primer_valute_1 = dollar_to_euro (Dollar 0.5)
(* val primer_valute_1 : euro = Euro 0.4305 *)

(*----------------------------------------------------------------------------*
 Definirajte tip `currency` kot en vsotni tip z konstruktorji za jen, funt in
 švedsko krono. Nato napišite funkcijo `to_pound`, ki primerno pretvori valuto
 tipa `currency` v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
 Ocaml sam opozori, da je potrebno popraviti funkcijo `to_pound`.
[*----------------------------------------------------------------------------*)

type currency =
  | Yen of float
  | Pound of float
  | Krona of float


let to_pound = function
| Yen x -> Pound (x *. 0.0079)
| Pound x -> Pound (x)
| Krona x -> Pound (x *. 0.085)


let primer_valute_2 = to_pound (Yen 100.)
(* val primer_valute_2 : currency = Pound 0.700000000000000067 *)

(*----------------------------------------------------------------------------*
 ## Mešani seznami

 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip `list` predstavimo s konstruktorjem za prazen seznam
 `Nil`(oz. `[]` v Ocamlu) in pa konstruktorjem za člen `Cons(x, xs)` (oz. `x ::
 xs` v Ocamlu).
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tip `intbool_list` z konstruktorji za:

 - prazen seznam,
 - člen s celoštevilsko vrednostjo,
 - člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal `[5; true; false; 7]`.
[*----------------------------------------------------------------------------*)

type intbool_list = 
| Nil
| IntCons of int * intbool_list
| BoolCons of bool * intbool_list

let test = IntCons(5, BoolCons(true, BoolCons (false, IntCons(7, Nil))))

(*----------------------------------------------------------------------------*
 Funkcija `intbool_map f_int f_bool ib_list` preslika vrednosti `ib_list` v nov
 `intbool_list` seznam, kjer na elementih uporabi primerno od funkcij `f_int`
 oz. `f_bool`.
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool (ib_list : intbool_list) = 
  match ib_list with
  | Nil -> Nil
  | IntCons(hd, tl) -> IntCons(f_int hd, intbool_map f_int f_bool tl)
  | BoolCons(hd, tl) -> BoolCons(f_bool hd, intbool_map f_int f_bool tl)

  let testni_primer1 =
    IntCons (5, BoolCons (true, BoolCons (false, IntCons (7, Nil))))
  
  let testni_primer2 =
    BoolCons (true, IntCons (3, IntCons (5, BoolCons (false, Nil))))
  
  let rezultat1 = intbool_map (fun x -> x * 2) (fun x -> not x) testni_primer1
  let rezultat2 = intbool_map (fun x -> x + 1) (fun x -> x) testni_primer2

(*----------------------------------------------------------------------------*
 Funkcija `intbool_reverse` obrne vrstni red elementov `intbool_list` seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let intbool_reverse ib_list = 
  let rec aux acc ostanek = 
    match ostanek with
    | Nil -> acc
    | IntCons(hd, tl) -> aux (IntCons (hd, acc)) tl
    | BoolCons (hd, tl) -> aux (BoolCons (hd, acc)) tl
  in
  aux Nil ib_list

let rezultat3 = intbool_reverse testni_primer1
let rezultat4 = intbool_reverse testni_primer2

(*----------------------------------------------------------------------------*
 Funkcija `intbool_separate ib_list` loči vrednosti `ib_list` v par `list`
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let rec intbool_separate ib_list = 
  let rec aux acc_int acc_bool ostanek = 
    match ostanek with
    | Nil -> (List.rev acc_int, List.rev acc_bool)
    | IntCons (hd, tl) -> aux (hd :: acc_int) acc_bool tl
    | BoolCons (hd, tl) -> aux acc_int (hd :: acc_bool) tl
  in
  aux [] [] ib_list
(*----------------------------------------------------------------------------*
 ## Čarodeji

 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 `magic`, ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire, frost
 in arcane. Ko se čarodej zaposli na akademiji, se usmeri v zgodovino,
 poučevanje ali raziskovanje oz. historian, teacher in researcher. Definirajte
 tip `specialisation`, ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic = 
| Fire
| Frost
| Arcane

type specialisation =
| Historian
| Teacher
| Researcher

(*----------------------------------------------------------------------------*
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent, na
 koncu pa SE lahko tudi zaposli. Definirajte tip `status`, ki določa ali je
 čarodej:

 - začetnik `Newbie`,
 - študent `Student` (in kateri vrsti magije pripada in koliko časa študira),
 - zaposlen `Employed` (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip `wizard` z poljem za ime in poljem za trenuten
 status ter dodajte primer `professor`, ki je zaposlen učitelj magije ognja, in
 `jaina`, ki je četrto leto študentka magije ledu.
[*----------------------------------------------------------------------------*)

type status = 
| Newbie
| Student of magic * int
| Employed of magic * specialisation

type wizard = {
  ime : string; 
  status : status
  }

let professor = {
  ime = "Profesor Ognjek";
  status = Employed (Fire, Teacher)
  }

let jaina = {
  ime = "Jaina";
  status = Student (Frost, 4)
}

(*----------------------------------------------------------------------------*
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip `magic_counter`, ki v posameznem polju hrani število
 uporabnikov magije. Nato definirajte funkcijo `update counter magic`, ki vrne
 nov števec s posodobljenim poljem glede na vrednost `magic`.
[*----------------------------------------------------------------------------*)

type magic_counter = {
  fire : int;
  frost : int;
  arcane : int  
}

let update counter magic = 
  match magic with 
  | Fire -> {counter with fire = counter.fire + 1}
  | Arcane -> { counter with arcane = counter.arcane + 1}
  | Frost -> { counter with frost = counter.frost + 1}

let primer_carovniki_1 = update {fire = 1; frost = 1; arcane = 1} Arcane
(* val primer_carovniki_1 : magic_counter = {fire = 1; frost = 1; arcane = 2} *)

(*----------------------------------------------------------------------------*
 Funkcija `count_magic` sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
[*----------------------------------------------------------------------------*)

let count_magic list_wizards = 
  let counter = {fire = 0; frost = 0; arcane = 0} in
  let rec aux acc = function
  | [] -> acc
  | hd :: tl -> 
    (let curr_magic_some = 
      match hd.status with 
      | Newbie -> None
      | Student (x, y) -> Some x
      | Employed (x, y) -> Some x
      in
    match curr_magic_some with
    | None -> aux acc tl
    | Some x -> aux (update acc x) tl
    )  
  in
  aux counter list_wizards

let primer_carovniki_2 = count_magic [professor; professor; professor; jaina]

(* val primer_carovniki_2 : magic_counter = {fire = 3; frost = 0; arcane = 0} *)

(*----------------------------------------------------------------------------*
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija. Funkcija `find_candidate magic
 specialisation wizard_list` poišče prvega primernega kandidata na seznamu
 čarodejev in vrne njegovo ime, čim ustreza zahtevam za `specialisation` in
 študira vrsto `magic`. V primeru, da ni primernega kandidata, funkcija vrne
 `None`.
[*----------------------------------------------------------------------------*)

let find_candidate magic specialisation wizards_list = 
  let st_let_sole = 
    match specialisation with
    | Historian -> 3
    | Researcher -> 4
    | Teacher -> 5
  in

  let is_wizard_good magic specialisation wizard1 = 
    let some_curr_magic = 
      match wizard1.status with 
      | Newbie -> None
      | Student (x, y) -> Some x
      | Employed (x, y) -> Some x
      in
    match some_curr_magic with
    | None -> false
    | Some x -> if x != magic then false
    else
      let let_specializije wizard =
        match wizard.status with
        | Student (x, y) -> Some y
        | _ -> None 
      in
      match (let_specializije wizard1) with
      | None -> false
      | Some y -> y >= st_let_sole
      
    in
  
  let rec aux = function
  | [] -> None
  | hd :: tl -> if is_wizard_good magic specialisation hd then Some hd.ime
  else aux tl
in
aux wizards_list

let primer_carovniki_3 =
  find_candidate Frost Researcher [professor; jaina]
(* val primer_carovniki_3 : string option = Some "Jaina" *)
