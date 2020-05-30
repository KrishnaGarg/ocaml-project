(*Test case*)
(*
Execute:
	print "Matrix Implementation in OCAML"
	A = [[1;2;3];[4;5;6]]
	print A
	B = [[1;1;1];[1;1;1];[1;1;1]]
	print B
	C = Product (A, B)
	print C
	size (C)
	D = [[2;2;2];[2;2;2]]
	print D
	E = C + D
	print E
	F = D - E
	print F
	G = 2 * F
	print G
	G = 5 * ([[1; 2]; [3; 4]])
	print G
	size (G)
	H = Transpose (G)
	print H
	size (H)
	I = H
	print I
	J = Transpose ([[1; 2]; [3; 4]])
	print J
	size ([[1; 2]; [3; 4]])
End
*)

let s = big_step(Seq(Execute, 
	Seq(Put (Arg2 ("Matrix Implementation in OCAML")),
	Seq(Assign ("A", ([[1;2;3];[4;5;6]])),
	Seq(Put(ArgM("A")),
	Seq(Assign ("B", ([[1;1;1];[1;1;1];[1;1;1]])),
	Seq(Put(ArgM("B")),
	Seq(Prod ("C", "A", "B"),
	Seq(Put (ArgM ("C")),
	Seq(Assign("D", ([[2;2;2];[2;2;2]])),
	Seq(Put (ArgM ("D")),
	Seq(Add("E", "C", "D"), 
	Seq(Put (ArgM ("E")),
	Seq(Sub("F", "D", "E"),
	Seq(Put (ArgM ("F")),
	Seq(ScalarMult("G", 2, "F"),
	Seq(Put (ArgM ("G")),
	Seq(ScalarMult2("G", 5, ([[1; 2]; [3; 4]])),
	Seq(Put (ArgM("G")),
	Seq(Size ("G"),
	Seq(Tranpose("H", "G"),
	Seq(Put (ArgM ("H")),
	Seq(Size("H"),
	Seq(Assign2("I", "H"),
	Seq(Put (ArgM ("I")),
	Seq(Transpose2("J", ([[1; 2]; [3; 4]])),
	Seq(Put (ArgM ("J")),
	Seq(Size2(([[1; 2]; [3; 4]])),
	End)))))))))))))))))))))))))))) empty_state;;