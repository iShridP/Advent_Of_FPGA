## Jane Street Advent of FPGA submission for winter 2025

This Readme contains basic points I found important while learning hardcaml and afterwards contains my approach and steps on how to run the code. 

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

The concatenation operator is @:, for 2 Signal.t types a and b, a@:b is [a|b] type value. Total width is width a + width b.

# Approach:

**Day 1**

Part 1: For the dial counter we had to create a dial register called *pos*, and keep track of how many times *pos* becomes zero. Since the amount of times turned by the dial can be very large, creating multiple rotations of the dial, we have to ensure that we restict this *pos* to be between 0-99. Hence we include variables *moved_pos* and *wrapped_pos*. 

Since $100*N + k$ number of dial rotations is the same as k rotations (for part 1), we calculate *reduced_amount* as amount%100. Since we cant directly implement division/modulus operations in one clock cycle, we instead use magic number division in which we estimate multiplication by 0.01 as:

$0.01 = \frac{M}{2^k}$

Choosing a suitable large accuracy of k, we can easily perform division by 100 multiplying by $M$ followed by right shift by k. This *reduced_amount* is added into current *pos* to make *moved_pos* and adjusted for 0-99 range by *wrapped_pos*. If *wrapped_pos* is zero after this computation then *pwd* register is incremented by 1. 

We also create a basic 3 state moore FSM for this computation. When finish is inputted by testbench signaling end of input dial rotations, the *pwd_valid* flag is set to vdd. 

Part 2: This is similar to the first part, except now we also care about the number of times the dial crosses 0 as well. Hence we care both about the remainder and quotient of amount%100. The quotient can directly be added into *pwd* since we are guaranteed to cross 0. 

**Day 3**

Part 1: It is assumed here, and I have designed a testbench, where the input to the FPGA is the entire *joltage_bank* as a single number represented in raw binary format. Hence when input value *bank_value* is recieved, we have to break it down into the individual digits and compare values to reach the optimal max joltage. 

The logic to finding max joltage is: Given a *bank_value* number, we start from right to left. If the final joltage is represented as AB (A*10 + B) then last digit is automatically B and second last automatically A for now. We then move from right to left. For the third digit, if it is > A then A is set to that value and B is set to A. If the third digit is < A then we do the following:
1. If third digit is also < B then we ignore that digit completely
2. If third digit is > B then we save it in register B_contender. If a new value of A is later found which is more left, then B is set to B_contender. (We cannot immediately set B to B_contender since B_contender is to left of A --> will make joltage BA and not AB).

We go from right to left in this manner until we reach end of entire bank. Each digit calculation and comparison occurs in one clock cycle, hence for a 100 digit input (as in input.txt), each bank_value requires 100 clock cycles to compute after which we take in next value. The testbench in designed such that it will input new value after 100 clock cycles and FSM is also designed as such.

For breaking up the bank into its individual digits we the double dabble method. It is cheap, although quite slow, still found better than doing modulus and division on a 330+ bit number (as given in input.txt). The testbench will wait for 2 clock cycles before giving next *bank_value* since the FSM is designed to do computation in 2 states, Accept and Calculate.

Part 2: This is an extension of Part 1 in which instead of using the raw BCD digits array for new max values, we use the masked digits BCD list as it has removed aldready used values in calculating joltage and everything to the left of it. We do this calculation for 12 selections, starting from most significant to least. 