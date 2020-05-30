open List
open Printf

type matrix = int list list

type tuple = int * int

type ident = string

type state = ident -> matrix option
let empty_state = fun x -> None
let update s x v = fun y -> if y = x then Some v else s y

type printArgs = Arg1 of tuple | Arg2 of string | Arg3 of matrix | ArgM of ident

type stmts =  Assign of ident * matrix | Assign2 of ident * ident
			| Add of ident * ident * ident | Add2 of ident * matrix * matrix
			| Sub of ident * ident * ident | Sub2 of ident * matrix * matrix
			| Mul of ident * ident * ident | Mul2 of ident * matrix * matrix
			| Prod of ident * ident * ident | Prod2 of ident * matrix * matrix
			| ScalarMult of ident * int * ident | ScalarMult2 of ident * int * matrix
			| Tranpose of ident * ident| Transpose2 of ident * matrix
			| Size of ident | Size2 of matrix
			| Put of printArgs
			| Seq of stmts * stmts
			| Skip
			| Execute
			| End

type program = Program of stmts

let rec nRows (m: matrix) : int = List.length m;;

let rec nCols (m: matrix) : int = 
	match m with 
	| [] -> 0
	| hd::tl -> List.length hd

let size (m: matrix) : tuple = 
	(nRows m, nCols m)

let rec addMatrices (m1: matrix) (m2: matrix) : matrix = 
	match m1, m2 with
	| [], [] -> []
	| hd1::tl1, hd2::tl2 -> (List.map2 (fun x y -> x + y) hd1 hd2) :: addMatrices tl1 tl2

let rec subtractMatrices (m1: matrix) (m2: matrix) : matrix = 
	match m1, m2 with
	| [], [] -> []
	| hd1::tl1, hd2::tl2 -> (List.map2 (fun x y -> x - y) hd1 hd2) :: subtractMatrices tl1 tl2

let rec multMatrices (m1: matrix) (m2: matrix) : matrix = 
	match m1, m2 with
	| [], [] -> []
	| hd1::tl1, hd2::tl2 -> (List.map2 (fun x y -> x * y) hd1 hd2) :: multMatrices tl1 tl2

let rec scalarMult (k: int) (m: matrix) = 
	match m with
	| [] -> []
	| hd :: tl -> (List.map (fun x -> k * x) hd ):: scalarMult k tl

let rec listMult (l1: int list) (l2: int list) : int = 
	match l1, l2 with
	| [], [] -> 0
	| hd::tl, hd2::tl2 -> hd * hd2 + listMult tl tl2

let rec productRowAndMatrix (l: int list) (m: matrix) : int list = 
	match m with
	| x::y -> if x = [] then [] else 
	let hd = List.map List.hd m in
	let tl = List.map List.tl m in
		(listMult l hd) :: (productRowAndMatrix l tl)

let rec productMatrices (m1: matrix) (m2: matrix) : matrix=
	match m1 with
	| x::y -> 
	(*let () = List.iter (printf "%d ") x in 
	let () = printf "\n" in*)
	if y = [] then [productRowAndMatrix x m2] else 
	(productRowAndMatrix x m2) :: (productMatrices y m2)

let rec transpose (m:matrix) : matrix =
  assert (m <> []);
  if List.mem [] m then
    []
    else
      (List.map List.hd m) :: transpose (List.map List.tl m);;

let rec printMatrix (m: matrix) =
	match m with
	| x :: y -> 
		let () = List.iter (printf "%d ") x in
		let() = printf "\n" in 
		if y <> [] then
		printMatrix y

let printIdent i s =
	match s i with
	| Some v -> printf "Printing matrix: %s\n" i; printMatrix v
	| None -> printf "Error in printIdent\n"

let rec step_cmd (c: stmts)(s: state) : (stmts * state) option = 
	match c with 
	| Assign (i, m) -> Some(Skip, update s i m)
	| Assign2 (m1, m2) -> (match s m2 with Some v -> (Some (Assign(m1, v), s)))
	| Add (id3,id1,id2) -> 
		(match s id1, s id2 with
		| Some v1, Some v2 -> Some (Assign(id3, addMatrices v1 v2), s))
	| Add2 (id, m1, m2) -> Some (Assign(id, addMatrices m1 m2), s)
	| Sub (id3, id1, id2) -> 
		(match s id1, s id2 with
		| Some v1, Some v2 -> Some (Assign(id3, subtractMatrices v1 v2), s))
	| Sub2 (id, m1, m2) -> Some (Assign(id, subtractMatrices m1 m2), s)
	| Mul (id3, id1, id2) -> 
		(match s id1, s id2 with
		| Some v1, Some v2 -> Some (Assign(id3, multMatrices v1 v2), s))
	| Mul2 (id, m1, m2) -> Some (Assign(id, multMatrices m1 m2), s)
	| Prod (id3, id1, id2) -> 
		(match s id1, s id2 with
		| Some v1, Some v2 -> Some (Assign(id3, productMatrices v1 v2), s))
	| Prod2 (id, m1, m2) -> Some (Assign(id, productMatrices m1 m2), s)
	| ScalarMult (id2, k, id) -> 
		(match s id with
			| Some v -> Some (Assign (id2, scalarMult k v), s))
	| ScalarMult2 (id, k, m) -> Some (Assign (id, scalarMult k m), s)
	| Tranpose (id2, id1) -> 
		(match s id1 with
			| Some v -> Some (Assign (id2, transpose v), s))
	| Transpose2 (id, m) -> Some (Assign (id, transpose m), s)
	| Size (id) -> 
		(match s id with 
			| Some v -> printf("Size of matrix %s:") id; Some(Put (Arg1 (size v)), s))
	| Size2 (m) -> printf("Size of new matrix: "); Some (Put (Arg1 (size m)), s)
	| Put(subcmd) -> (match subcmd with
					| ArgM (i) -> printIdent i s; Some(Skip, s)
					| Arg1(i, j) -> printf "%d, %d\n" i j; Some (Skip, s)
					| Arg2 (str) -> printf "String: %s\n" str; Some (Skip, s)
					| Arg3 (m) -> printf("Printing matrix:\n"); printMatrix m; Some (Skip, s)
					)
	| Seq (Skip, c) -> Some (c, s)
	| Seq (c1, c2) -> (match step_cmd c1 s with 
					| Some (c', s') -> Some (Seq (c', c2), s')
					| None -> None)
	| Skip -> None
	| Execute -> let () = printf "\n\n\n\n\n\n\n******** Program Execution Begins Here ********\n" in Some (Skip, s)
	| End -> let() = printf "\n******** Program Execution Ends Here ********\n\n\n\n\n\n\n\n" in Some (Skip, s)
	| _ -> None;;

let rec step_n_times (c:stmts) (s:state) (n: int) : (stmts * state) option =
	match step_cmd c s with
	| Some (rStmts, new_state) -> if (n>0) then step_n_times rStmts new_state (n-1) else Some(rStmts, new_state)
	| None -> None;;

let rec big_step(c: stmts) (s:state) : (stmts * state) option =
	match step_cmd c s with
	| Some (rStmts, new_state) -> big_step rStmts new_state
	| None -> None;;