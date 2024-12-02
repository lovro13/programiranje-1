-- Izomorfizmi

theorem eq1 {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  by
    apply Iff.intro -- constructor sam pogleda če je dokaz ki ga rabimo
    -- konstruktor, kot npr. ekvivalenca
    intro ab
    apply And.intro -- spet lahko uporabimo constructor
    exact ab.right
    exact ab.left
    intro ab
    apply And.intro
    exact ab.right
    exact ab.left


theorem eq2 {A B : Prop} : (A ∨ B) ↔ (B ∨ A) :=
  sorry

theorem eq3 {A B C : Prop} : (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C)) :=
  sorry

theorem eq4 {A B C : Prop} : (A ∨ (B ∨ C)) ↔ (B ∨ (A ∨ C)) :=
 sorry

theorem eq5 {A B C : Prop} : A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) :=
  sorry

theorem eq6 {A B C : Prop} : (B ∨ C) → A ↔ (B → A) ∧ (C → A) :=
  by
    apply Iff.intro
    intro h
    apply And.intro
    intro b
    apply h
    left
    exact b

    intro c
    have xx : B ∨ C := Or.inr c
    have yy := h (Or.inr c)
    exact yy

    intro h BvC
    cases BvC
    case inl b =>
      have l := h.left
      apply h.left
      exact b
    case inr c =>
      exact h.right c

theorem eq7 {A B C : Prop} : C → (A ∧ B) ↔ (C → A) ∧ (C → B) :=
  sorry
