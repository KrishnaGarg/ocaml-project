# DSL For Matrices: Implemented Using OCAML & Verified Using COQ

* Matrix operations implemented:*
- Assignment
- Addition
- Subtraction
- Product
- Scalar Multiplication
- Transpose
- Size
- Print Matrix
- Evaluate sequence of commands using big_step execution order

Refer this [report](https://github.com/KrishnaGarg/ocaml-project/blob/master/CS-476%20project%20report.pdf) for more details about the implementation.

# Usage instructions
Install ocaml following the instructions [here](https://ocaml.org/docs/install.html).

On the terminal, run *ocaml* to start the OCAML command prompt.

On the OCAML command prompt, run:
```
#use "driver.ml"
#use "testcase1.ml"
#use "testcase2.ml"
```

- driver.ml contains the main source code.
- testcase1.ml contains the test cases for testing every statement individually
- testcase2.ml contains the single test case of 29 lines.

For DSL verification, [CoqIDE](https://coq.inria.fr/download) is required.
Run `verified-dsl.v` by opening CoqIDE

# Credits
Joint work with Sutanu Ghosh for DSL implementation in OCAML