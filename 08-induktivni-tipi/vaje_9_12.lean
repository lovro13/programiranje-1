def concat {A : Type} : List A → List A → List A :=
  fun xs ys =>
    match xs with
    | [] => ys
    | x :: xs' => x :: concat xs' ys

def reverse {A: Type} : List A -> List A :=
  fun lst =>
    match lst with
    | [] => []
    | x :: xs => (reverse xs) ++ [x]

def reverseAux {A: Type} : List A -> List A -> List A:=
  fun ost acc =>
    match ost with
    | [] => acc
    | x :: xs => reverseAux xs (x :: acc)

def reverse' {A : Type} : List A -> List A :=
  fun lst =>
    reverseAux lst []

theorem pomozna_lema {A : Type} : ∀ {lst : List A}, ∀ {acc : List A},
 reverseAux lst acc = (reverse lst) ++ acc :=
 by
  intro lst
  induction lst with
  | nil =>
    intro acc
    simp [reverseAux, reverse, concat]
  | cons x xs ih =>
    intro acc
    simp [reverseAux]
    rw[ih]
    simp [reverse]


theorem reverse_eq_reverse' {A: Type} {xs :
 List A} : reverse xs = reverse' xs :=
  by
    rw [reverse']
    rw [pomozna_lema]
    simp[reverse]
