From MyAutomata.Automata Require Import DFA.
Import Stdlib.Lists.List.
Import List.ListNotations.

Inductive ModThreeState := Q0 | Q1 | Q2.

Inductive BitDigit := Zero | One.

Definition modThreeTransition (state : ModThreeState) (bit : BitDigit) : ModThreeState :=
  match state, bit with
  | Q0, Zero => Q0
  | Q0, One  => Q1
  | Q1, Zero => Q2
  | Q1, One  => Q0
  | Q2, Zero => Q1
  | Q2, One  => Q2
  end.

Definition modThreeDFA : DFA ModThreeState BitDigit := {|
  transition := modThreeTransition;
  initialState := Q0;
  isFinalState := fun state =>
    match state with
    | Q0 => true
    | _  => false
    end
|}.

Example modThree_101 : isAccepted modThreeDFA [One; Zero; One] = false.
Proof. reflexivity. Qed.

Example modThree_110 : isAccepted modThreeDFA [One; One; Zero] = true.
Proof. reflexivity. Qed.

Example modThree_0 : isAccepted modThreeDFA [Zero] = true.
Proof. reflexivity. Qed.

Example modThree_empty : isAccepted modThreeDFA [] = true.
Proof. reflexivity. Qed.
