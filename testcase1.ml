
(*Small test cases*)
step_cmd (Put (Arg2 ("Welcome to OCAML Matrix Evaluaton"))) empty_state;;
step_cmd (Put (Arg3([[1;2];[4;5]]))) empty_state;;
big_step (Seq (Assign ("A", ([[1;2;3];[4;5;6]])), Put(ArgM("A")))) empty_state;;
step_cmd (Put (ArgM("A"))) (update empty_state "A" ([[1;2;3];[4;5;6]]));;
big_step (Seq (Add ("B", "A", "A"), Put(ArgM("B")))) (update empty_state "A" ([[1;2];[3;4]]));;
big_step (Seq (Add2 ("A", ([[1;2;3];[3;4;3]]), ([[1;2;3];[3;4;3]])), Put(ArgM("A")))) empty_state;;
big_step (Seq (Sub ("B", "A", "A"), Put(ArgM("B")))) (update empty_state "A" ([[1;2];[3;4]]));;
big_step (Seq (Sub2 ("A", ([[1;2;3];[3;4;3]]), ([[1;2;3];[3;4;3]])), Put(ArgM("A")))) empty_state;;
big_step (Seq (Mul ("B", "A", "A"), Put(ArgM("B")))) (update empty_state "A" ([[1;2];[3;4]]));;
big_step (Seq (Mul2 ("A", ([[1;2;3];[3;4;3]]), ([[1;2;3];[3;4;3]])), Put(ArgM("A")))) empty_state;;
big_step (Seq (Prod ("B", "A", "A"), Put(ArgM("B")))) (update empty_state "A" ([[1;2];[3;4]]));;
big_step (Seq (Prod2 ("A", ([[1;2;3];[3;4;3]]), ([[1;2];[3;4];[5;6]])), Put(ArgM("A")))) empty_state;;
big_step (Seq (ScalarMult ("B", 2, "A"), Put(ArgM("B")))) (update empty_state "A" ([[1;2];[3;4]]));;
big_step (Seq (ScalarMult2 ("A", 2, ([[1;2];[3;4]])), Put(ArgM("A")))) empty_state;;
big_step (Seq (Tranpose ("B", "A"), Put(ArgM("B")))) (update empty_state "A" ([[1;2;3];[4;5;6]]));;
big_step (Seq (Transpose2 ("A", ([[1;2;3];[4;5;6];[7;8;9]])), Put(ArgM("A")))) empty_state;;
big_step (Size ("A")) (update empty_state "A" ([[1;2;3];[4;5;6]]));;
big_step (Size2 ([[1;2;3];[4;5;3];[4;5;6];[6;5;6]])) empty_state;;
big_step (Seq (Execute, Seq(Put (Arg3([[1;2];[4;5]])), End))) empty_state;;