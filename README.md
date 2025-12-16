## Jane Street Advent of FPGA submission for winter 2025

**Basic points:**

1. **OCaml** is a high-level programming language. **Hardcaml** is a Jane Street library for OCaml to describe and design hardware, essentially acting like an HDL. (Files end in `.ml`.)
2. **opam** is like conda. It manages packages and creates virtual environments for OCaml installations and libraries.
3. **dune** is a build system for OCaml projects. It compiles `.ml` files.
4. **utop** is an OCaml toplevel which lets you load libraries and try single expressions. It is used for running `Hardcaml_waveterm`.
5. **hardcaml_waveterm** is a waveform simulator and visualiser for Hardcaml descriptions.

Every time we have to use Hardcaml or Hardcaml waveterm, it is required to run  `eval $(opam env)`, this just converts your shell to use the opam compiler and see opam installed libraries. For convenience just add to .bashrc.  

Vector signals or bits:
The hardcaml types Signal.t and Bits.t are used to specify vectors. Every vector has a specified width and it can be determined. There is a special signal called empty which has no value. Simplest way to create Bits.t is the of_string function: 

    # let x = of_string "11011"
    val x : t = 11011

The leftmost bit is the MSB and rightmost is LSB. We can find width with # width x. # to_unsigned_int x and # to_signed_int will convert vector x into an integer. 

Functions of_unsigned_int, of_signed_int, of_string will parse any given expression into Bits.t vector.  Examples:

	# of_unsigned_int ~width:10 514
	- : t = 1000000010

	# of_signed_int ~width(3) -1
	- : t = 111

	# of_string "5'b11"
	- : t = 00011

	# of_string "5'd13"
	- : t = 01101

An explicit length (in binary) can be specified with <length in binary>'<notation><value in that notation>. We use o for octal, d for decimal, b for binary and h for hex. Using capitals will make it signed, hence the MSB of the result will be the MSB of "value in that notation".

*Hardcaml does not include the singed/unsigned nature of vector into the vector itself, but rather the type of operand used*.  Operators ending in '+'  are signed operators (treat operand vectors like signed vectors) and operators ending in ':' are unsigned operators. Hence '<+' is signed less than and '<:' is unsigned less than.
 
Some operations like addition (+:) do not care about signed or unsigned nature since they operate the same way in each case. Addition operator requires that both vectors be of same length else it raises a runtime error.

1. +: and -: are addition and multiplication, both vectors must be of same length
2. \*: is unsigned multiplication and \*+ is signed. Operands can have arbitrary widths, result will be the sum of the widths. 
3. &: logical and, |: logical or, ^: logical xor, ~: logical not. For all binary operators, both widths must be the exact same. 
4. ==: is equality operator, <+ , <=+, >+, >=+ are signed and <:, <=:, >: and >=: are unsigned. <>:  means not equal. The result is a single bit (true or false). Both widths must be the same. 

