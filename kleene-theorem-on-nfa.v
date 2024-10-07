Require Import List.
Import ListNotations.
Require Import List ssreflect.

Set Implicit Arguments.

Inductive regex (A: Type) : Type :=
| Epsilon : regex A
| Char : A -> regex A
| Union : regex A -> regex A -> regex A
| Concat : regex A -> regex A -> regex A
| Star : regex A -> regex A.

Arguments Epsilon {_}.

Fixpoint regex_accepts (A: Type) (r: regex A) (w: list A) : Prop :=
  match r, w with
  | Epsilon, [] => True
  | Char c, [c'] => c = c'
  | Union r1 r2, _ => regex_accepts r1 w \/ regex_accepts r2 w
  | Concat r1 r2, _ => exists pref suf, pref ++ suf = w /\ regex_accepts r1 pref /\ regex_accepts r2 suf
  | Star r, _ => exists ws, w = concat ws /\ Forall (regex_accepts r) ws
  | _, _ => False
  end.

Definition finite (T: Type) :=
  exists (xs: list T), forall (x: T), In x xs.

Inductive arrow (A State: Type) :=
| sym : State -> A -> State -> arrow A State
| eps : State -> State -> arrow A State.

Arguments eps {_ _}.

Record NFA (A State: Type) : Type :=
  {
    state_fin: finite State          ;
    delta:     arrow A State -> Prop ;
    q0:        State                 ;
    F:         State -> Prop
  }.

Fixpoint run_accepting (A State: Type) (nfa: NFA A State) (w: list A) (cur: State) (run: list State) : Prop :=
  match run with
  | [] => match w with
          | [] => F nfa cur
          | _ => False
          end
  | next :: rest => (delta nfa (eps cur next) /\ run_accepting nfa w next rest) \/
                      match w with
                      | [] => False
                      | x :: xs => delta nfa (sym cur x next) /\ run_accepting nfa xs next rest
                      end
  end.

Definition nfa_accepts (A State: Type) (nfa: NFA A State) (w: list A) : Prop :=
  exists run, run_accepting nfa w (q0 nfa) run.

Unset Implicit Arguments.

Module MNFA.

Lemma sum_fin :
  forall {S1 S2: Type} (F1: finite S1) (F2: finite S2), finite (sum S1 S2).
  intros S1 S2 [xs Ixs] [ys Iys].
  exists (map inl xs ++ map inr ys).
  destruct x.
  - apply in_or_app; left.
    apply in_map. apply Ixs.
  - apply in_or_app; right.
    apply in_map. apply Iys.
Defined.

Ltac inv H := inversion H; clear H; subst.

Inductive multi_delta {A State} (d : arrow A State -> Prop) : State -> list A -> State -> Prop :=
| multi_delta_refl : forall n, multi_delta d n [] n
| multi_delta_step_eps : forall n1 n2 n3 xs, d (eps n1 n2) -> multi_delta d n2 xs n3 ->
    multi_delta d n1 xs n3
| multi_delta_step_sym : forall n1 n2 n3 x xs, d (sym n1 x n2) -> multi_delta d n2 xs n3 ->
    multi_delta d n1 (x :: xs) n3.

Lemma multi_delta_trans : forall {A State} (d : arrow A State -> Prop) n1 n2 n3 xs1 xs2,
  multi_delta d n1 xs1 n2 -> multi_delta d n2 xs2 n3 -> multi_delta d n1 (xs1 ++ xs2) n3.
Proof.
  intros A State d n1 n2 n3 xs1 xs2 Hd1 Hd2; induction Hd1.
  - auto.
  - eapply multi_delta_step_eps; eauto.
  - eapply multi_delta_step_sym; eauto.
Qed.

Inductive nfa_accepts_alt {A State} (nfa : NFA A State) (w : list A) : Prop :=
| accepts_alt : forall n, multi_delta (delta nfa) (q0 nfa) w n -> F nfa n -> nfa_accepts_alt nfa w.

Lemma nfa_accepts_alt_correct : forall {A State} (nfa : NFA A State) w, nfa_accepts nfa w <-> nfa_accepts_alt nfa w.
Proof.
  split.
  - intros [run Hrun].
    cut (exists n, multi_delta (delta nfa) (q0 nfa) w n /\ F nfa n).
    { intros [n []]; eapply accepts_alt; eauto. }
    revert w Hrun; generalize (q0 nfa); induction run; simpl; intros q w Hrun.
    + destruct w; [|easy]; exists q; split; auto; apply multi_delta_refl.
    + destruct Hrun as [[? Hrun]|Hrun].
      * apply IHrun in Hrun; destruct Hrun as [n [Hd Hn]].
        exists n; split; auto; eapply multi_delta_step_eps; eauto.
      * destruct w; [easy|]; destruct Hrun as [? Hrun].
        apply IHrun in Hrun; destruct Hrun as [n [Hd Hn]].
        exists n; split; auto; eapply multi_delta_step_sym; eauto.
  - intros H; inv H; unfold nfa_accepts.
    revert H0; generalize (q0 nfa); intros q Hd; induction Hd.
    + now exists [].
    + edestruct IHHd as [run Hrun]; eauto.
      exists (n2 :: run); simpl; left; auto.
    + edestruct IHHd as [run Hrun]; eauto.
      exists (n2 :: run); simpl; right; auto.
Qed.

Record MNFA (A State1 State2 : Type) : Type :=
{
  m_state_fin1: finite State1;
  m_state_fin2: finite State2;
  m_sm_delta: arrow A (sum State1 State2) -> Prop;
  m_bg_delta: arrow (list A) State1 -> Prop;
  m_q0: State1;
  m_F: State1 -> Prop;
  m_sm_delta_rest: forall ar, m_sm_delta ar -> match ar with
    | sym (inl _) _ _ => False
    | sym _ _ (inl _) => False
    | eps (inl _) (inl _) => False
    | _ => True
    end;
  m_bg_delta_rest: forall ar, m_bg_delta ar -> match ar with
    | eps _ _ => False
    | _ => True
    end;
  m_sm_bg: forall n1 n2 w, multi_delta m_sm_delta (inl n1) w (inl n2) -> exists ws, w = concat ws /\ multi_delta m_bg_delta n1 ws n2;
  m_bg_sm: forall n1 n2 ws, multi_delta m_bg_delta n1 ws n2 -> multi_delta m_sm_delta (inl n1) (concat ws) (inl n2);
}.
Arguments m_state_fin1 {_ _ _}.
Arguments m_state_fin2 {_ _ _}.
Arguments m_sm_delta {_ _ _}.
Arguments m_bg_delta {_ _ _}.
Arguments m_q0 {_ _ _}.
Arguments m_F {_ _ _}.
Arguments Build_MNFA {_ _ _}.

Definition MNFA_NFA {A State1 State2} (mnfa : MNFA A State1 State2) : NFA A (sum State1 State2) :=
  Build_NFA (sum_fin (m_state_fin1 mnfa) (m_state_fin2 mnfa)) (m_sm_delta mnfa) (inl (m_q0 mnfa))
    (fun n => match n with
      | inl n => m_F mnfa n
      | _ => False
      end).
Definition MNFA_LNFA {A State1 State2} (mnfa : MNFA A State1 State2) : NFA (list A) State1 :=
  Build_NFA (m_state_fin1 mnfa) (m_bg_delta mnfa) (m_q0 mnfa) (m_F mnfa).

Lemma use_MNFA {A State1 State2} {P : list A -> Prop} (mnfa : MNFA A State1 State2) :
  (forall w, nfa_accepts (MNFA_LNFA mnfa) w -> P (concat w)) ->
  (forall w, P w -> exists ws, w = concat ws /\ nfa_accepts (MNFA_LNFA mnfa) ws) ->
  {State & {nfa : NFA A State | forall w, nfa_accepts nfa w <-> P w} }.
Proof.
  intros lP Pl; exists _, (MNFA_NFA mnfa); split.
  - intros H; apply nfa_accepts_alt_correct in H; inv H.
    revert H0 H1; change (delta (MNFA_NFA mnfa)) with (m_sm_delta mnfa).
    change (q0 (MNFA_NFA mnfa)) with (inl (B:=State2) (m_q0 mnfa)).
    simpl; destruct n as [n|]; [|easy].
    intros H Hn; apply m_sm_bg in H; destruct H as [ws [? Hws]]; subst.
    eapply lP, nfa_accepts_alt_correct, accepts_alt; eauto.
  - intros Pw; apply Pl in Pw; destruct Pw as [ws [? Hws]]; subst.
    apply nfa_accepts_alt_correct in Hws; apply nfa_accepts_alt_correct; inv Hws.
    apply accepts_alt with (inl n); auto.
    change (delta (MNFA_NFA mnfa)) with (m_sm_delta mnfa).
    change (q0 (MNFA_NFA mnfa)) with (inl (B:=State2) (m_q0 mnfa)).
    apply m_bg_sm; auto.
Qed.

Lemma empty_fin : finite Empty_set.
Proof. exists []; intros []. Qed.

Definition empty_MNFA {A State} (fin : finite State) (q : State) (f : State -> Prop) : MNFA A State Empty_set.
Proof.
  eapply (Build_MNFA fin empty_fin (fun _ => False) (fun _ => False) q f); try easy.
  - intros n1 n2 w H; inv H; [|easy|easy].
    exists []; split; auto; apply multi_delta_refl.
  - intros n1 n2 ws H; inv H; [|easy|easy].
    apply multi_delta_refl.
Defined.

Definition add_arrow {A State1 State2 State3} (mnfa : MNFA A State1 State2) (N1 : State1) (nfa : NFA A State3) (N2 : State1) :
  MNFA A State1 (sum State2 State3).
Proof.
  set (sm_delta := fun ar => match ar with
      | eps (inl n1) (inr (inl n2)) => m_sm_delta mnfa (eps (inl n1) (inr n2))
      | eps (inl n1) (inr (inr n2)) => n1 = N1 /\ n2 = q0 nfa
      | eps (inr (inl n1)) (inl n2) => m_sm_delta mnfa (eps (inr n1) (inl n2))
      | eps (inr (inl n1)) (inr (inl n2)) => m_sm_delta mnfa (eps (inr n1) (inr n2))
      | sym (inr (inl n1)) x (inr (inl n2)) => m_sm_delta mnfa (sym (inr n1) x (inr n2))
      | eps (inr (inr n1)) (inl n2) => F nfa n1 /\ n2 = N2
      | eps (inr (inr n1)) (inr (inr n2)) => delta nfa (eps n1 n2)
      | sym (inr (inr n1)) x (inr (inr n2)) => delta nfa (sym n1 x n2)
      | _ => False
      end).
  set (bg_delta := fun ar => m_bg_delta mnfa ar \/ match ar with
      | sym n1 x n2 => n1 = N1 /\ nfa_accepts nfa x /\ n2 = N2
      | _ => False
      end).
  assert (forall n1 x n2, multi_delta (m_sm_delta mnfa) n1 x n2 ->
    multi_delta sm_delta (match n1 with | inl n1 => inl n1 | inr n1 => inr (inl n1) end) x
                         (match n2 with | inl n2 => inl n2 | inr n2 => inr (inl n2) end)) as Hsm.
  { intros n1 x n2 H; induction H.
    - apply multi_delta_refl.
    - eapply multi_delta_step_eps; eauto.
      destruct n1, n2; auto; now apply m_sm_delta_rest in H.
    - eapply multi_delta_step_sym; eauto.
      destruct n1, n2; auto; now apply m_sm_delta_rest in H. }
  assert (forall n1 x n2, multi_delta (m_bg_delta mnfa) n1 x n2 -> multi_delta bg_delta n1 x n2) as Hbg.
  { intros n1 x n2 H; induction H.
    - apply multi_delta_refl.
    - eapply multi_delta_step_eps; eauto; now left.
    - eapply multi_delta_step_sym; eauto; now left. }
  eapply (Build_MNFA (m_state_fin1 mnfa) (sum_fin (m_state_fin2 mnfa) (state_fin nfa)) sm_delta bg_delta (m_q0 mnfa) (m_F mnfa)).
  - intros [[] ? []|[] []]; simpl; auto; now destruct s.
  - intros []; simpl; auto; intros []; auto; now apply m_bg_delta_rest in H.
  - intros n1 n2 w H.
    assert (exists n' w2, multi_delta sm_delta n' w2 (inl n2) /\ match n' with
      | inl n' => w = w2 /\ n' = n1
      | inr (inl n') => exists w1, w = w1 ++ w2 /\ multi_delta (m_sm_delta mnfa) (inl n1) w1 (inr n')
      | inr (inr n') => exists w1, w = w1 ++ w2 /\ n1 = N1 /\ multi_delta (delta nfa) (q0 nfa) w1 n'
      end) as H'.
    { exists (inl n1), w; repeat split; auto. }
    clear H; destruct H' as [n' [w2 [H Hn']]]; remember (inl n2) as ln2.
    revert n1 n2 w Heqln2 Hn'; induction H; intros; subst.
    + inv Hn'; exists []; split; auto; apply multi_delta_refl.
    + destruct n1 as [n1|[n1|n1]].
      * inv Hn'; destruct n2 as [|[n2|n2]]; [easy| |].
        -- edestruct IHmulti_delta as [ws [? Hws]]; eauto.
           exists []; split; auto; eapply multi_delta_step_eps, multi_delta_refl; auto.
        -- inv H; edestruct IHmulti_delta as [ws [? Hws]]; eauto.
           exists []; repeat split; apply multi_delta_refl.
      * destruct Hn' as [w1 [? Hw1]]; subst.
        destruct n2 as [n2|[n2|]]; [| |easy].
        -- edestruct IHmulti_delta as [ws [? Hws]]; eauto; subst.
           assert (multi_delta (m_sm_delta mnfa) (inl n0) w1 (inl n2)) as Hws1.
           { replace w1 with (w1 ++ []) by now rewrite app_nil_r.
             eapply multi_delta_trans; eauto; eapply multi_delta_step_eps, multi_delta_refl; auto. }
           apply m_sm_bg in Hws1; destruct Hws1 as [ws1 [? Hws1]]; subst.
           exists (ws1 ++ ws); rewrite concat_app; split; auto.
           eapply multi_delta_trans; eauto.
        -- edestruct IHmulti_delta as [ws [? Hws]]; eauto.
           exists w1; split; auto; replace w1 with (w1 ++ []) by now rewrite app_nil_r.
           eapply multi_delta_trans; eauto; eapply multi_delta_step_eps, multi_delta_refl; eauto.
      * destruct Hn' as [w1 [? [? Hw1]]]; subst.
        destruct n2 as [n2|[|n2]]; [|easy|].
        -- inv H; edestruct IHmulti_delta as [ws [? Hws]]; eauto; subst.
           exists (w1 :: ws); split; auto.
           eapply multi_delta_step_sym; eauto.
           right; repeat split; eapply nfa_accepts_alt_correct, accepts_alt; eauto.
        -- edestruct IHmulti_delta as [ws [? Hws]]; eauto.
           exists w1; repeat split.
           replace w1 with (w1 ++ []) by now rewrite app_nil_r.
           eapply multi_delta_trans, multi_delta_step_eps, multi_delta_refl; eauto.
    + destruct n1 as [|[n1|n1]]; [easy| |].
      * destruct n2 as [|[n2|]]; [easy| |easy].
        destruct Hn' as [w1 [? Hw1]]; subst.
        edestruct IHmulti_delta as [ws [? Hws]]; eauto.
        exists (w1 ++ [x]); rewrite <- app_assoc; split; auto.
        eapply multi_delta_trans, multi_delta_step_sym, multi_delta_refl; eauto.
      * destruct n2 as [|[|n2]]; [easy|easy|].
        destruct Hn' as [w1 [? [? Hw1]]]; subst.
        edestruct IHmulti_delta as [ws [? Hws]]; eauto.
        exists (w1 ++ [x]); rewrite <- app_assoc; repeat split.
        eapply multi_delta_trans, multi_delta_step_sym, multi_delta_refl; eauto.
  - intros n1 n2 ws H; induction H.
    + apply multi_delta_refl.
    + destruct H; [|easy]; now apply m_bg_delta_rest in H.
    + simpl; eapply multi_delta_trans; eauto.
      destruct H as [|[? []]]; [|subst].
      * assert (multi_delta (m_sm_delta mnfa) (inl n1) (concat [x]) (inl n2)) as Hmsm.
        { now eapply m_bg_sm, multi_delta_step_sym, multi_delta_refl. }
        apply Hsm in Hmsm; simpl in Hmsm; now rewrite app_nil_r in Hmsm.
      * eapply multi_delta_step_eps with (inr (inr _)); [split; eauto|].
        replace x with (x ++ []) by now rewrite app_nil_r.
        apply nfa_accepts_alt_correct in H1; inv H1.
        eapply multi_delta_trans with (inr (inr _)), multi_delta_step_eps, multi_delta_refl; [|split; eauto].
        revert H; generalize (q0 nfa); clear; intros q H; induction H.
        -- apply multi_delta_refl.
        -- eapply multi_delta_step_eps; now eauto.
        -- eapply multi_delta_step_sym; now eauto.
Defined.

Lemma fin_unit : finite unit.
Proof. exists [tt]; intros []; now left. Qed.

Lemma fin_bool : finite bool.
Proof. exists [true; false]; intros []; repeat ((try now left); right). Qed.

Inductive tri := t0 | t1 | t2.
Lemma fin_tri : finite tri.
Proof. exists [t0; t1; t2]; intros []; repeat ((try now left); right). Qed.

Definition regex_NFA {A} (r : regex A) : {State & {nfa : NFA A State | forall w, nfa_accepts nfa w <-> regex_accepts r w} }.
Proof.
  induction r.
  - exists _, (Build_NFA fin_unit (fun _ => False) tt (fun _ => True)); split.
    + intros H; apply nfa_accepts_alt_correct in H; inv H; inv H0; try easy.
    + intros H; destruct w; [|easy]; apply nfa_accepts_alt_correct.
      eapply accepts_alt; eauto; apply multi_delta_refl.
  - exists _, (Build_NFA fin_bool (fun ar => ar = sym false a true) false (fun b => b = true)); split.
    + intros H; apply nfa_accepts_alt_correct in H; inv H; inv H0; try easy.
      inv H; now inv H2.
    + intros H; destruct w as [|ch []]; [easy| |easy]; inv H.
      eapply nfa_accepts_alt_correct, accepts_alt; simpl; eauto.
      now eapply multi_delta_step_sym, multi_delta_refl.
  - destruct IHr1 as [State1 [nfa1 IH1]], IHr2 as [State2 [nfa2 IH2]].
    eapply (use_MNFA (add_arrow (add_arrow (empty_MNFA fin_tri t0 (fun t => t <> t0)) t0 nfa1 t1) t0 nfa2 t2)); intros w Hw.
    + apply nfa_accepts_alt_correct in Hw; inv Hw; simpl in *.
      inv H; [easy|now destruct H1 as [[]|]|].
      destruct H1 as [[|[? []]]|[? []]]; [easy| |]; subst.
      * inv H2; simpl in *; [rewrite app_nil_r; left; now apply IH1|now destruct H3 as [[]|]|now destruct H3 as [[]|]].
      * inv H2; simpl in *; [rewrite app_nil_r; right; now apply IH2|now destruct H3 as [[]|]|now destruct H3 as [[]|]].
    + exists [w]; simpl; rewrite app_nil_r; split; auto; apply nfa_accepts_alt_correct.
      destruct Hw as [Hw|Hw]; [apply IH1 in Hw|apply IH2 in Hw].
      * apply accepts_alt with t1; [|easy].
        eapply multi_delta_step_sym, multi_delta_refl; left; right; repeat split; auto.
      * apply accepts_alt with t2; [|easy].
        eapply multi_delta_step_sym, multi_delta_refl; right; repeat split; auto.
  - destruct IHr1 as [State1 [nfa1 IH1]], IHr2 as [State2 [nfa2 IH2]].
    eapply (use_MNFA (add_arrow (add_arrow (empty_MNFA fin_tri t0 (fun t => t = t2)) t0 nfa1 t1) t1 nfa2 t2)); intros w Hw.
    + apply nfa_accepts_alt_correct in Hw; inv Hw; simpl in *; subst.
      inv H; destruct H0 as [[]|]; try easy; destruct H as [_ []]; subst.
      inv H1; destruct H0 as  [[]|]; try easy; destruct H0 as [_ []]; subst.
      inv H2; try now destruct H1 as [[]|].
      exists x, x0; simpl; rewrite app_nil_r; repeat split; [apply IH1|apply IH2]; easy.
    + destruct Hw as [pre [suf [? [Hpre Hsuf]]]]; subst.
      exists [pre; suf]; simpl; rewrite app_nil_r; split; auto.
      apply nfa_accepts_alt_correct, accepts_alt with t2; simpl; auto.
      eapply multi_delta_step_sym, multi_delta_step_sym, multi_delta_refl; [left|]; right; repeat split; [apply IH1|apply IH2]; easy.
  - destruct IHr as [State [nfa IH]].
    eapply (use_MNFA (add_arrow (empty_MNFA fin_unit tt (fun _ => True)) tt nfa tt)); intros w Hw.
    + apply nfa_accepts_alt_correct in Hw; inv Hw; simpl in *; destruct n; clear H0.
      induction H.
      * exists []; split; auto.
      * auto.
      * destruct H as [|[? [Hx]]]; [easy|subst].
        destruct IHmulti_delta as [ws []].
        exists (x :: ws); simpl; rewrite H; split; auto.
        apply Forall_cons; auto; now apply IH.
    + destruct Hw as [ws [? Hws]]; subst; exists ws; split; auto.
      apply nfa_accepts_alt_correct, accepts_alt with tt; [|easy].
      simpl; induction ws; [apply multi_delta_refl|inv Hws].
      eapply multi_delta_step_sym; eauto; right; repeat split; now apply IH.
Defined.

Theorem kleene :
  forall (A: Type) (r: regex A),
    exists (State: Type) (nfa: NFA A State),
      (forall (w: list A), regex_accepts r w <-> nfa_accepts nfa w).
Proof.
  intros A r; destruct (regex_NFA r) as [State [nfa]]; exists _, nfa; intros; apply iff_sym; auto.
Qed.

End MNFA.

Module SSRefl_NFA.

Set Implicit Arguments.

Lemma option_fin : forall (T: Type), finite T -> finite (option T).
Proof.
  intros T [xs Ixs].
  exists (None :: map Some xs).
  intro x; destruct x.
  - simpl. right. apply in_map. apply Ixs.
  - simpl. left. reflexivity.
Qed.

Lemma sum_fin :
  forall (S1 S2: Type) (F1: finite S1) (F2: finite S2), finite (S1 + S2).
Proof.
  move => S1 S2 [xs1 H1] [xs2 H2].
  exists (map inl xs1 ++ map inr xs2).
  case => x; rewrite in_app_iff !in_map_iff; eauto.
Qed.

Lemma unit_fin : finite unit.
Proof. by exists [tt]; case => /=; left. Qed.

Lemma bool_fin : finite bool.
Proof. by exists [false; true]; case => /=; auto. Qed.

Lemma nfa_epsilon (A: Type):
  exists S (nfa: NFA A S),
  forall w, w = [] <-> nfa_accepts nfa w.
Proof.
  exists unit.
  exists {|state_fin := unit_fin;
           delta := fun _ => False;
           q0 := tt; F := fun _ => True|}.
  case => [| w ws] /=.
  - by split => // _; exists [].
  - by split => //; move => [] [] //= r rs; tauto.
Qed.

Lemma nfa_char (A: Type) (a: A):
  exists S (nfa: NFA A S),
  forall w, w = [a] <-> nfa_accepts nfa w.
Proof.
  exists bool.
  exists {|state_fin := bool_fin;
           delta := fun arr => match arr with
                               | sym false x true => x = a
                               | _ => False
                               end;
           q0 := false; F := fun s => s = true|}.
  case => [| x [| y w]].
  - by split => //; move => [] [] //=; tauto.
  - split => [-> | [] [] //= r rs].
    + by exists [true] => /=; auto.
    + by firstorder; destruct r => //; congruence.
  - split => //; move => [] [] //=; firstorder.
    destruct a0 => //.
    by case: l H0 => //=; tauto.
Qed.

Lemma nfa_union (A S1 S2: Type) (nfa1: NFA A S1) (nfa2: NFA A S2):
  exists S (nfa: NFA A S),
  forall w, nfa_accepts nfa1 w \/ nfa_accepts nfa2 w <-> nfa_accepts nfa w.
Proof.
  exists (option (S1 + S2)).
  exists {|state_fin := option_fin (sum_fin (state_fin nfa1) (state_fin nfa2));
           q0 := None;
           F := fun s => match s with
                         | Some (inl x) => F nfa1 x
                         | Some (inr y) => F nfa2 y
                         | None => False
                         end;
           delta := fun arr => match arr with
                               | eps None (Some (inl x)) => x = q0 nfa1
                               | eps None (Some (inr y)) => y = q0 nfa2
                               | eps (Some (inl x)) (Some (inl y)) => delta nfa1 (eps x y)
                               | eps (Some (inr x)) (Some (inr y)) => delta nfa2 (eps x y)
                               | sym (Some (inl x)) a (Some (inl y)) => delta nfa1 (sym x a y)
                               | sym (Some (inr x)) a (Some (inr y)) => delta nfa2 (sym x a y)
                               | _ => False
                               end;|}.
  set nfa := {| state_fin := _ |}.
  move => w; split => /=.
  - case; move => [run] H.
    + exists (Some (inl (q0 nfa1)) :: map (fun s => Some (inl s)) run) => /=; left.
      split => //.
      elim: run w (q0 nfa1) H => //= r rs IH w s.
      case; first by move => [H1 /IH H2]; left.
      by case: w => // a w H; right; firstorder.
    + exists (Some (inr (q0 nfa2)) :: map (fun s => Some (inr s)) run) => /=; left.
      split => //.
      elim: run w (q0 nfa2) H => //= r rs IH w s.
      case; first by move => [H1 /IH H2]; left.
      by case: w => // a w H; right; firstorder.
  - move => [run] /=.
    case: run => /= [| s run]; first by case: w.
    case; last by case: w; tauto.
    case: s => [s|]; last by tauto.
    case: s => s [->] H {s}; [left | right].
    + suff: exists run', run = map (fun s => Some (inl s)) run' /\
                       run_accepting nfa1 w (q0 nfa1) run'
        by move => [rs] [_] H1; exists rs.
      elim: run w (q0 nfa1) H => [| r run IH] w s /=;
        first by case: w => // H; exists []; split.
      case; last case: w => // a w;
        case; case: r => // r; case: r => // r Hsr /IH [rs] [->] H;
        by exists (r :: rs); split => //=; tauto.
    + suff: exists run', run = map (fun s => Some (inr s)) run' /\
                       run_accepting nfa2 w (q0 nfa2) run'
        by move => [rs] [_] H1; exists rs.
      elim: run w (q0 nfa2) H => [| r run IH] w s /=;
        first by case: w => // H; exists []; split.
      case; last case: w => // a w;
        case; case: r => // r; case: r => // r Hsr /IH [rs] [->] H;
        by exists (r :: rs); split => //=; tauto.
Qed.

Lemma nfa_concat (A S1 S2: Type) (nfa1: NFA A S1) (nfa2: NFA A S2):
  exists S (nfa: NFA A S),
  forall w, (exists pref suf, w = pref ++ suf /\
                                nfa_accepts nfa1 pref /\
                                nfa_accepts nfa2 suf)
            <-> nfa_accepts nfa w.
Proof.
  exists (S1 + S2)%type.
  exists {|state_fin := sum_fin (state_fin nfa1) (state_fin nfa2);
           q0 := inl (q0 nfa1);
           F := fun s => match s with
                         | inr y => F nfa2 y
                         | _ => False
                         end;
           delta := fun arr => match arr with
                               | eps (inl x) (inl y) => delta nfa1 (eps x y)
                               | eps (inr x) (inr y) => delta nfa2 (eps x y)
                               | eps (inl x) (inr y) => F nfa1 x /\ y = q0 nfa2
                               | sym (inl x) a (inl y) => delta nfa1 (sym x a y)
                               | sym (inr x) a (inr y) => delta nfa2 (sym x a y)
                               | _ => False
                               end;|}.
  set nfa := {| state_fin := _ |}.
  move => w; split.
  - move => [pref] [suf] [->] [] [run1 H1] [run2 H2].
    exists (map inl run1 ++ inr (q0 nfa2) :: map inr run2).
    have Hsuf: run_accepting nfa suf (inr (q0 nfa2)) (map inr run2).
    + move: H2; elim: run2 suf (q0 nfa2) => [| r rs IH] suf s //=; case;
        first by move => [H /IH]; tauto.
      by case: suf => // a suf [H /IH]; tauto.
    + move: H1 => /=.
      elim: run1 pref (q0 nfa1) => [| r rs IH] pref s /=;
        first by case: pref => //=; auto.
      case; first by move => [H /IH]; tauto.
      by case: pref => //= s1 pref [H /IH]; tauto.
  - move => [run] /= Hrun.
    suff: exists pref suf run1 run2,
        pref ++ suf = w /\
        run_accepting nfa1 pref (q0 nfa1) run1 /\
        run_accepting nfa2 suf (q0 nfa2) run2 /\
        run = map inl run1 ++ inr (q0 nfa2) :: map inr run2.
    + move => [pref] [suf] [rs1] [rs2] [<-] [H1] [H2] _.
      exists pref, suf; split => //.
      by split; [exists rs1 | exists rs2].
    + elim: run w (q0 nfa1) Hrun => [| r rs IH] w s /=; first by case: w.
      case.
      * case: r => r.
        ** move => [Hsr /IH] [pref] [suf] [rs1] [rs2] [<-] [H1] [H2] ->.
           by exists pref, suf, (r :: rs1), rs2; repeat split => //=; tauto.
        ** move => [] [Hs Hr] Hrs.
           have: exists rs', rs = map inr rs' /\ run_accepting nfa2 w (q0 nfa2) rs'.
           -- move: Hrs; rewrite Hr {Hr IH Hs s r}.
              elim: rs w (q0 nfa2) => /= [| r rs IH] w s; first by case: w => // H; exists [] => /=.
              case.
              ++ case; case: r => // r H /IH [run] [-> H2]; exists (r :: run).
                 by split => //=; tauto.
              ++ case: w => // a w; case; case: r => // r H /IH [run] [-> H2].
                 by exists (r :: run) => /=; tauto.
           -- move => [run2] [rs_eq] Hacc2.
              by exists [], w, [], run2; repeat split => //=; rewrite Hr rs_eq.
      * case: w => // a w; case; case: r => // r Hsr /IH [pref] [suf] [run1] [run2] [<-] [H1] [H2] ->.
        by exists (a :: pref), suf, (r :: run1), run2; repeat split => //=; tauto.
Qed.

Lemma nfa_plus (A S: Type) (nfa1: NFA A S):
  exists (nfa: NFA A S),
  forall w, (exists ws, ws <> [] /\ w = concat ws /\ Forall (nfa_accepts nfa1) ws) <-> nfa_accepts nfa w.
Proof.
  exists {|state_fin := state_fin nfa1;
           q0 := q0 nfa1;
           F := F nfa1;
           delta := fun arr => match arr with
                               | eps x y => delta nfa1 (eps x y) \/ F nfa1 x /\ y = q0 nfa1
                               | arr => delta nfa1 arr
                               end;|}.
  set nfa := {| state_fin := _ |}.
  move => w; split.
  - move => [ws] [Hws] [->] {w}.
    elim: ws Hws => // w ws IH _.
    rewrite Forall_cons_iff; move => [H1] Hall.
    have Hacc: nfa_accepts nfa w.
    + move: H1 => [run] H1 {IH Hall}; exists run; simpl.
      elim: run w (q0 nfa1) H1 => [| r rs IH] w s //=.
      case; first by firstorder.
      by case: w => // a w; firstorder.
    + have: ws = [] \/ ws <> [] by destruct ws; auto using nil_cons.
      case; first by move => -> /=; rewrite app_nil_r.
      move/IH/(_ Hall) => [run2] H.
      move: Hacc => [run1] {IH} H1.
      exists (run1 ++ q0 nfa :: run2).
      elim: run1 w {1 2}(q0 nfa) H1 => [| r rs IH] w s /=;
        first by case: w => //; tauto.
      by case; last case: w => // a w; move => [Hrs] /IH /=; tauto.
  - move => [run] /= H.
    suff: exists wr1 wrs,
        w = concat (map fst (wr1 :: wrs)) /\
        run = snd wr1 ++ concat (map (fun wr => q0 nfa1 :: snd wr) wrs) /\
        run_accepting nfa1 (fst wr1) (q0 nfa1) (snd wr1) /\
        Forall (fun wr => run_accepting nfa1 (fst wr) (q0 nfa1) (snd wr)) wrs.
    + move => [wr1] [wrs] [->] [_] [Hacc1] Hall {H}.
      exists (map fst (wr1 :: wrs)); split => //=; move: Hall.
      rewrite !Forall_forall /nfa_accepts => H {w}; split => // w Hw.
      suff: exists rs, In (w, rs) (wr1 :: wrs).
      * move => [rs] /=; case => [H1 | /H] /=; last by eauto.
        exists rs; case: wr1 H1 Hacc1 {Hw} => a b.
        by rewrite pair_equal_spec; move => [-> ->] /=.
      * elim: wrs Hw {H} => /= [| a l IH].
        ** by case => // <-; exists (snd wr1); case: wr1 {Hacc1}; tauto.
        ** case; [|case].
          -- move => eq.
             have [rs H] := IH (or_introl eq).
             by exists rs; tauto.
          -- move => <-.
             by exists (snd a); case: a; eauto.
          -- move => H.
             have [rs H1] := IH (or_intror H).
             by exists rs; tauto.
    + elim: run w {1 3}(q0 nfa1) H => [|r rs IH] w s /=;
        first by case: w=> // Hs; exists ([], []), [] => /=; repeat split.
      case.
      * move => [H /IH] [wr1] [wrs] [->] [->] [Hacc1] Hall.
        case: H => [H | [H eq]].
        ** exists (fst wr1, r :: snd wr1), wrs => /=.
           by repeat split => //; tauto.
        ** exists ([], []), (wr1 :: wrs) => /=.
           repeat split => //; first by rewrite eq.
           apply: Forall_cons => //.
           by rewrite -eq.
      * case: w => // a w [Hsr /IH] [wr1] [wrs] [->] [->] [Hacc1] Hall {IH}.
        exists (a :: fst wr1, r :: snd wr1), wrs => /=.
        by repeat split => //; tauto.
Qed.

Theorem kleene :
  forall (A: Type) (r: regex A),
    exists (State: Type) (nfa: NFA A State),
      (forall (w: list A), regex_accepts r w <-> nfa_accepts nfa w).
Proof.
  move => A; induction r.
  - have [S [nfa] H] := nfa_epsilon A.
    by exists S, nfa => w /=; rewrite -H; case: w.
  - have [S [nfa] H] := nfa_char a.
    exists S, nfa => w /=; rewrite -H; case: w => // b [] //.
    by split; congruence.
  - move: IHr1 IHr2 => /= [S1] [nfa1] H1 [S2] [nfa2] H2.
    have [S [nfa] H] := nfa_union nfa1 nfa2.
    by exists S, nfa => w; rewrite H1 H2.
  - move: IHr1 IHr2 => [S1] [nfa1] IH1 [S2] [nfa2] IH2.
    have [S [nfa] H] := nfa_concat nfa1 nfa2.
    exists S, nfa => w /=; rewrite -H.
    split => [] [pref] [suf] [eq] H1; exists pref, suf; rewrite eq.
    + by rewrite -IH1 -IH2.
    + by rewrite IH1 IH2.
  - move: IHr => [S0] [nfa0] H0.
    have Hall: forall ws, Forall (regex_accepts r) ws <-> Forall (nfa_accepts nfa0) ws
      by move => ws; rewrite !Forall_forall; split => Hw w /Hw; rewrite H0.
    have [S1 [nfa1] H1] := nfa_epsilon A.
    have [nfa2 H2] := nfa_plus nfa0.
    have [S [nfa] H] := nfa_union nfa1 nfa2.
    exists S, nfa => w /=; rewrite -H -H1 -H2; split.
    + move => [ws]; case: ws => /= [|w1 ws]; first by tauto.
      by rewrite Hall; move => [->] Hall2; right; exists (w1 :: ws).
    + case; first by move => ->; exists [].
      by move => [ws]; rewrite -Hall; move => Hws; exists ws; tauto.
Qed.

Unset Implicit Arguments.

End SSRefl_NFA.
