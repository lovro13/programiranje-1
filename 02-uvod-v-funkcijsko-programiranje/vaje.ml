(* ========== Vaja 2: Uvod v funkcijsko programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Vektorje predstavimo kot seznam števil s plavajočo vejico.
[*----------------------------------------------------------------------------*)

type vector = float list

(*----------------------------------------------------------------------------*]
Definirajte enotske vektorje `i`, `j` in `k` v treh dimenzijah.
[*----------------------------------------------------------------------------*)

let i = [1.; 0.; 0.;]
let j = [0.; 1.; 0.;]
let k = [0.; 0.; 1.;]

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razteg : float -> vector -> vector`, ki vektor, 
predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let rec razteg alpha vector1 = 
    let pomozna_funkcija element = alpha *. element in
    List.map pomozna_funkcija vector1

(*----------------------------------------------------------------------------*]
Napišite funkcijo `sestej : vector -> vector -> vector`, ki vrne vsoto dveh 
vektorjev.
[*----------------------------------------------------------------------------*)

let rec sestej vector1 vector2 = 
    List.map2 (fun x y -> x +. y) vector1 vector2

(*----------------------------------------------------------------------------*]
Napišite funkcijo `skalarni_produkt : vector -> vector -> float`, ki izračuna 
skalarni produkt dveh vektorjev
[*----------------------------------------------------------------------------*)

let rec skalarni_produkt vector1 vector2 = 
    let produkt_po_tockah = List.map2 (fun x y -> x *. y) vector1 vector2 in
    List.fold_left (+.) 0.0 produkt_po_tockah

(*----------------------------------------------------------------------------*]
Napišite funkcijo `norma : vector -> float`, ki vrne evklidsko normo vektorja.
[*----------------------------------------------------------------------------*)

let rec norma vector1 =
    let norma_na_kvadrat = skalarni_produkt vector1 vector1 in
    sqrt norma_na_kvadrat


(*----------------------------------------------------------------------------*]
Napišite funkcijo `projeciraj : vector -> vector -> vector`, ki izračuna 
projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let rec projeciraj vektor1 vektor2= 
    let skalarni_produkt12 = skalarni_produkt vektor1 vektor2 in
    razteg (skalarni_produkt12 /. (norma vektor1)) (razteg (norma vektor1) vektor1)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML 
oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.

Primer:
`ovij "h1" "Hello, world!"`

[*----------------------------------------------------------------------------*)

let rec ovij oznaka tekst = 
    "<" ^ oznaka ^ ">" ^ tekst ^ "<\\" ^ oznaka ^ ">"
    

(*----------------------------------------------------------------------------*]
Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število 
presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za ustrezno število presledkov.

Primer:
`zamakni 4 "Hello, \n world!";;`

[*----------------------------------------------------------------------------*)

let rec zamakni length1 string1= 
        let presledki = String.make length1 ' ' in
        let seznam_vrstic = String.split_on_char '\n' string1 in
        let nov_seznam = List.map (fun vrstica -> presledki ^ vrstica) seznam_vrstic in
        String.concat "\n" nov_seznam

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne 
niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:

Primer:
`ul ["ananas"; "banana"; "čokolada"]`

[*----------------------------------------------------------------------------*)

let rec ul  seznam= 
    let seznam1 = List.map (ovij "li") seznam in
    let string_vrstice = String.concat "\n" seznam1 in
    let string_vrstice2 = zamakni 4 string_vrstice in
    ovij "ul" ("\n" ^ string_vrstice2 ^ "\n")

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme niz, 
ki vsebuje vejico, loči na del pred in del za njo.

Primer:
`razdeli_vrstico "mleko, 2"`

[*----------------------------------------------------------------------------*)

let rec razdeli_vrstico string1 = 
    let seznam  = String.split_on_char ',' string1 in
    let prvi_del = List.nth seznam 0 in
    let drugi_del = List.nth seznam 1 in
    (String.trim prvi_del, String.trim drugi_del)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`, 
ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike 
"izdelek, vrednost", in vrne seznam ustreznih parov.

Primer:
`pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"`

[*----------------------------------------------------------------------------*)

let rec pretvori_v_seznam_parov string1 = 
    let seznam_vrstic = String.split_on_char '\n' string1 in
    let seznam_vrstic_dvojic = List.map (razdeli_vrstico) seznam_vrstic in
    seznam_vrstic_dvojic

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list -> (string * 'b) list`,
ki dano funkcijo uporabi na vseh drugih komponentah elementov seznama.

Primer:
```ml
let seznam = [("ata", "mama"); ("teta", "stric")] in 
pretvori_druge_komponente String.length seznam
```

[*----------------------------------------------------------------------------*)

let rec pretvori_druge_komponente funkcija seznam_parov = 
    let seznam = List.map (fun (prvi, drugi) -> (prvi, funkcija drugi)) seznam_parov in
    seznam  
    
(*----------------------------------------------------------------------------*]
Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki 
sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni 
znesek nakupa.

Primer:
```ml
let nakupovalni_seznam = "mleko, 2\njabolka, 5"
and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
izracunaj_skupni_znesek cenik nakupovalni_seznam
```

[*----------------------------------------------------------------------------*)

let rec izracunaj_skupni_znesek = ()
