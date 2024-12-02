-- s predavanj ki sem mankal, ampak je podobno kot v ocamlu
-- rekurzvno na taprvega inpotem to zdruzujemo
def concat {A : Type} : List A → List A → List A :=
  fun xs ys =>
    match xs with
    | [] => ys
    | x :: xs' => x :: concat xs' ys

#check (concat ["a", "b"] ["c", "d"])

--reverse napisimi s concatom
def reverse {A : Type} : List A → List A :=
  fun list_x =>
    match list_x with
    | [] => []
    | x :: xs => concat (reverse xs) [x]

#eval (reverse ["1", "2"])


#eval (reverse ["a", "b", "c", "d"])

def length {A : Type} : List A → Nat :=
  fun List =>
    match List with
    | [] => 0
    | _ :: xs => 1 + (length xs)


#eval (length ["a", "b", "c", "d"])

--reverse od singletone je isti singletone, ni treba indukcije oz niti ne moremo iti
--z indukcijo lahko dokazujemo samo z tridtvami za vsak nekaj ...
-- o x vemo skoraj nič, razen tega da je tipa A, zato ne moremo indukcije
theorem trd1  {A : Type} {x : A} : reverse [x] = [x] :=
  by
    simp [reverse]
    simp [concat]

#check trd1

-- smo delali ravno na tablo
theorem trd2 {A : Type} {xs ys : List A} : length (concat xs ys) = length xs + length ys :=
  by
    induction xs with
    | nil =>
      simp [concat]
      simp [length]
    | cons x xs' ih =>
      simp [concat]
      simp [length]
      rw [ih]
      rw [Nat.add_assoc]

#check trd2
-- Tega poznamo že iz predavanj
theorem trd3 {A : Type} {xs : List A} : concat xs [] = xs :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih]


--tudi nekaj na predavnajah
--concat je asociativen
theorem trd4 {A : Type} {xs ys zs : List A} : concat (concat xs ys) zs = concat xs (concat ys zs) :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih]

#check (trd4)

theorem trd5 {A : Type} {xs ys : List A} : reverse (concat xs ys) = concat (reverse ys) (reverse xs) :=
  by
    induction xs with
    | nil =>
      simp [reverse]
      simp [concat]
      rw [trd3]
    | cons x xs' ih =>
      simp [concat]
      simp [reverse]
      rw [ih]
      rw [trd4]

#check (trd5)

theorem trd6 {A : Type} {xs : List A} : length (reverse xs) = length xs :=
  by
    induction xs with
    | nil =>
      simp[reverse]
    | cons x xs' ih =>
      simp [reverse]
      simp [length]
      rw [trd2]
      rw [ih]
      simp [length]
      rw [Nat.add_comm]

#check trd6

theorem trd7 {A : Type} {xs : List A} : reverse (reverse xs) = xs :=
  by
    induction xs with
    | nil =>
      simp [reverse]
    | cons x xs' ih =>
      simp [reverse]
      rw [trd5]
      simp [reverse]
      simp [concat]
      rw [ih]

#check trd7


def map {A B : Type} : (A → B) → List A → List B :=
  fun f xs =>
    match xs with
    | [] => []
    | x :: xs' => (f x) :: (map f xs')

#eval (map (fun a => a + 1) [1, 2])


--domaaaa
theorem map_assoc {A B C : Type} {f : A → B} {g : B → C} {xs : List A} : map g (map f xs) = map (g ∘ f) xs :=
  sorry

theorem map_id {A : Type} {xs : List A} : map id xs = xs :=
  sorry

theorem map_concat {A B : Type} {f : A → B} {xs ys : List A} : map f (concat xs ys) = concat (map f xs) (map f ys) :=
  sorry


theorem map_reverse {A B : Type} {f : A → B} {xs : List A} : map f (reverse xs) = reverse (map f xs) :=
  sorry
--domaaa ali pa nkol :)

inductive tree (A : Type) : Type where
  | empty : tree A
  | node : A → tree A → tree A → tree A

#check tree.rec

def tree_map {A B : Type} : (A → B) → tree A → tree B :=
  fun f tre =>
    match tre with
    | tree.empty => tree.empty
    | tree.node x l r => tree.node (f x) (tree_map f l) (tree_map f r)


theorem tree_map_empty {A B : Type} {f : A → B} : tree_map f tree.empty = tree.empty :=
  sorry

theorem tree_map_comp {A B C : Type} {f : A → B} {g : B → C} {t : tree A} : tree_map g (tree_map f t) = tree_map (g ∘ f) t :=
  sorry

def depth {A : Type} : tree A → Nat :=
  fun t =>
    match t with
    | tree.empty => 0
    | tree.node _ l r => 1 + Nat.max (depth l) (depth r)

-- S tem se ne bomo ukvarjali
theorem max_comm {a b : Nat} : Nat.max a b = Nat.max b a :=
  sorry

def mirror {A : Type} : tree A → tree A :=
  sorry

theorem mirror_depth {A : Type} {t : tree A} : depth (mirror t) = depth t :=
  sorry

theorem mirror_mirror {A : Type} {t : tree A} : mirror (mirror t) = t :=
  sorry

def collect {A : Type} : tree A → List A :=
  fun t =>
    match t with
    | tree.empty => []
    | tree.node x l r => concat (collect l) (concat [x]  (collect r))

theorem trd8 {A : Type} {x : A} {xs ys : List A} : concat xs (x::ys) = concat (concat xs [x]) ys :=
  sorry


theorem collect_mirror {A : Type} {t : tree A} : collect (mirror t) = reverse (collect t) :=
  sorry


def size {A : Type} : tree A → Nat :=
  sorry

theorem size_mirror {A : Type} {t : tree A} : size (mirror t) = size t :=
  sorry
