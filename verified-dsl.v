Require Import List.
Require Import ZArith.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Program.
Import ListNotations.
Local Open Scope Z_scope.
Local Open Scope string_scope.

Definition matrix := list (list Z).
Definition tuple := (nat, nat).
Definition ident := string.

Definition mat:= (ident, matrix).
Definition m := ("A", [[1;2]]).
Check m.
Check mat.
Check List.length.
Check map.
Example A := (-1:: 1::-3::nil)::( 3::-4::-6::nil)::nil.
Example O:= (-1:: 1::-3::nil)::( 3::4::6::nil)::nil.
Compute map (fun X => List.length X) A.

Definition nRows (m: matrix) := List.length m.
Definition nCols (m: matrix) := 
  match m with
  | [] =>  0%nat
  | hd::tl => List.length hd
  end.

Compute nRows(A).
Compute nCols (A).

Definition size (m: matrix)  :=
(nRows m, nCols m).

Check size.
Check tuple. (*Doubt*)


Inductive stmts :=
| Assign : ident * matrix -> stmts
| Assign2 : ident * ident -> stmts.


Variable f : Z -> Z -> Z.

  Fixpoint map2 (l:list Z) (l2: list Z) : list Z :=
    match l, l2 with
      | [], [] => []
      | [], _ => []
      | _, [] => []
      | hd :: tl, hd2 :: tl2 => (f hd hd2) :: (map2 tl tl2)
    end.

Fixpoint addLists (l1: list Z) (l2: list Z) : list Z :=
  match l1, l2 with
  | [], [] | [], _ | _, [] => []
  | hd1::tl1, hd2::tl2 => (hd1 + hd2) :: addLists tl1 tl2
  end.


Fixpoint addMatrices (m1: matrix) (m2: matrix) : matrix :=
  match m1, m2 with
  | [], [] | [], _ | _, [] => []
  | hd1::tl1, hd2::tl2 => addLists hd1 hd2 :: (addMatrices tl1 tl2)
  end.

Compute (addMatrices A A).

Fixpoint subtractLists (l1: list Z) (l2: list Z) : list Z :=
  match l1, l2 with
  | [], [] | [], _ | _, [] => []
  | hd1::tl1, hd2::tl2 => (hd1 - hd2) :: subtractLists tl1 tl2
  end.


Fixpoint subtractMatrices (m1: matrix) (m2: matrix) : matrix :=
  match m1, m2 with
  | [], [] | [], _ | _, [] => []
  | hd1::tl1, hd2::tl2 => subtractLists hd1 hd2 :: (subtractMatrices tl1 tl2)
  end.

Compute (subtractMatrices A A).

Fixpoint multLists (l1: list Z) (l2: list Z) : list Z :=
  match l1, l2 with
  | [], [] | [], _ | _, [] => []
  | hd1::tl1, hd2::tl2 => (hd1 * hd2) :: multLists tl1 tl2
  end.


Fixpoint multMatrices (m1: matrix) (m2: matrix) : matrix :=
  match m1, m2 with
  | [], [] | [], _ | _, [] => []
  | hd1::tl1, hd2::tl2 => multLists hd1 hd2 :: (multMatrices tl1 tl2)
  end.

Compute (multMatrices A A).


Fixpoint scalarMult (k: Z) (m: matrix) :=
  match m with
  | [] => []
  | hd :: tl=> (map (fun x => k * x) hd) :: scalarMult k tl
  end.

Compute (scalarMult 2 A).

Fixpoint dotProduct (l1: list Z) (l2: list Z) : Z :=
	match l1, l2 with
	| [], [] | [], _ | _, [] => 0
	| hd::tl, hd2::tl2 => hd * hd2 + dotProduct tl tl2
end.

Definition listl := [1;2;3].
Compute (tl A).

Fixpoint firstCol (m: matrix) : list Z :=
  match m with
  | [] => []
  | x::y => (hd (0) x)::(firstCol y)
  end.

Fixpoint restCols (m: matrix) : matrix := 
  match m with
  | [] => []
  | x::y => (tl x) :: restCols y
  end.

Definition m1 := [[1;2];[3;4];[5;6]].
Definition l1 := [1;2;3].
Compute (restCols m1).

Lemma obligation_ncols : forall m0 : matrix, 
0%nat <> nCols m0 ->
(nCols (restCols m0) < nCols m0)%nat.
Proof.
destruct m0 as [ | hd tl].
+ contradiction.
+ simpl in *. destruct hd as [ | l'].
    - contradiction.
    - simpl. auto.
Qed.

 Program Fixpoint productRowAndMatrix (l: list Z) (m: matrix) { measure (nCols m) } : list Z :=
 match (nCols m) with
 |  0%nat => []
 | _ => 
	 let h := dotProduct l (firstCol m) in
	  let t := productRowAndMatrix l (restCols m) in
	  (h::t)
	  end.
Show Obligation Tactic.
Next Obligation.
apply obligation_ncols.
apply H.
Qed.
Compute (productRowAndMatrix l1 m1).

Program Fixpoint transpose (m: matrix) {measure (nCols m)} : matrix :=
match (nCols m) with
 |  0%nat => []
 | _ => 
  (firstCol m) :: (transpose (restCols m))
  end.

Next Obligation.
apply obligation_ncols.
apply H.
Qed.

Compute (transpose m1).

(*
Notation "x +++ y" := (addMatrices x y)
                       (at level 50, left associativity).
                       Z.add_comm
*)

Theorem list_add_comm: forall x y : list Z, addLists x y = addLists y x.
Proof.
intros x y.
revert y.
induction x as [|hd tl].
+ destruct y as [| hd tl].
  - simpl. reflexivity.
  - simpl. reflexivity.
+ destruct y as [| hd' tl'].
  - simpl. reflexivity.
  - simpl. rewrite Z.add_comm. f_equal. rewrite IHtl. reflexivity.
Qed.

Theorem mat_add_comm : forall x y : matrix, addMatrices x y = addMatrices  y x.
Proof.
intros m n.
revert n.
induction m as [|hd tl]. simpl.
+ destruct n as [|hd' tl']. 
    - simpl. reflexivity.
    - simpl. reflexivity.
+ simpl. destruct n as [|hd' tl'].
    - simpl. reflexivity.
    - simpl. rewrite list_add_comm.  f_equal.  rewrite IHtl. reflexivity. Qed.


Compute (addMatrices A O).
Compute (addMatrices O A).


(*let rec productMatrices (m1: matrix) (m2: matrix) : matrix=
	match m1 with
	| x::y -> 
	(*let () = List.iter (printf "%d ") x in 
	let () = printf "\n" in*)
	if y = [] then [productRowAndMatrix x m2] else 
	(productRowAndMatrix x m2) :: (productMatrices y m2)
	*)

Fixpoint productMatrices (m1: matrix) (m2: matrix) : matrix :=
  match m1 with 
  | [] => []
  | x :: y => (productRowAndMatrix x m2) :: (productMatrices y m2)
end.

Theorem productRowAndMatrix_assoc : forall x: list Z, forall y z: matrix, productRowAndMatrix x (productMatrices y z) =
productRowAndMatrix (productRowAndMatrix x y) z.
Proof.
intros x y z.
induction x as [|hd tl].
+ simpl.

Theorem mat_product_assoc : forall x y z : matrix, productMatrices x (productMatrices y z) = productMatrices (productMatrices x y) z.
Proof.
intros x y z.
induction x as [|hd tl].
+ simpl. reflexivity.
+ simpl. f_equal.