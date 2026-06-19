## Jane Street Advent of FPGA submission for winter 2025

This Readme contains my approach and steps on how to run the code. 

# How to run the testbench

A bash script has aldready been created in each question directory. Go into the question day(day)_part(1/2) and run:

	bash runtestbench.sh

This will open gtkwave (assuming aldready installed, if not do 'sudo apt install gtkwave' and run). The waveform can be viewed, output signal ports can ebe added and by scrolling to end of waveform the final values can be seen. 

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

**Day 4**

Part 1: The logic is straightforward, we interprit each row as a bitstring @ -> 1 and . -> 0, we feed this row at each clock cycle and the FPGA will keep 3 buffers of past, current and future row (new row is added as future row). We utilise paralellism to go through all indices i = 0 to i = 139 of the bitstring row provided and check if there is paper roll or not, if there is we check the surroundings and if the condition of <4 rolls being there is fulfiled. At last we send total number of rolls in that row which can be removed, which is added to the *total_rolls* register. We use an FSM with 2 basic states, accept and calculate, which accept the new row into *next_row* and will update each of the row registers, and calculate state which will update the *total_rolls* register. When finish flag is set to 1, we will move to EndCalculate state and give a dummy all 0 row as the next_row so that the last valid row moves into current_row and its valid number of rolls is added to the sum. 

**Day 5**
We are first fed ranges of valid fresh food id's. Important thing is that these ID's can overlap, hence for any inventory checking it is important to first reduce these ranges into the simplest disjoint intervals. We do this by the following. Keep two lists of values for lower limit and upper limit of some arbitrary size (first all initalised to 0). When the first interval [a,b] comes, we save *a* in lower limit and *b* in upper limit. When next range [c,d] comes we check if the 2 intervals are disjoint. If they are then save *c* into next index of lower limit list and *d* into next index of upper limit list. If they are not disjoint then find [a,b] $\cup$ [c,d] and save its lower and upper limits into the two lists, removing the initial [a,b] save. Continue this for all ranges, Checking each new range to see if it is disjoint and if not, doing suitable operations. In the end we will have two lists of lower and upper limits, all of which define disjoint intervals. Then we can easily check any input ID is valid or not:

Since all the intervals are disjoint, find the smallest upper bound such that input ID < bound. Then check if the corresponding lower bound is less than or greater than the input ID. Is it is less than then in input ID is valid else not. Utilising FPGA parallelism this this can be done very fast. I would have used the Hardcaml RAM cell implementation for this since the number of intervals can scale very large, but doing so would mean each request to access an element of the RAM would require one clock cycle, deafeating the purpose of FPGA's and parallelism. Instead use physical registers of some given arbitrary size. This approach is fast but very expensive on FPGA's as it uses a lot of LUT's. For larger number of intervals we will have to use RAM blocks but sacrifice speed. 

The main reason I want all new IDs to be grouped into disjoint intervals upfront is that, once this is done, checking any number of IDs becomes very simple. Since the inventory does not change very often, it makes sense to do this organization once and then rely on it for repeated checks.

**Day 6**

The problem give a series of vertical math operations where groups of numbers must be either added or multiplied based on a symbol at the bottom. My implementation treats these vertical columns as parallel memory entries. I utilized 5 distinct RAM blocks: four to store the numerical operands and one to store the operator for each column.

During the loading phase, the input is parsed such that the rows of numbers fill the first four RAM blocks, and the operator row fills the fifth. For the calculation phase, the logic iterates through the memory indices. To maximize throughput, the design simultaneously computes both the sum (A+B+C+D) and the product (A×B×C×D) of the four operands in parallel. The value stored in the operator RAM then acts as a selector for a multiplexer, instantly choosing the correct result. This value is added to a running grand_total accumulator, allowing the entire worksheet to be solved in a single pipelined pass.


**Day 7**

the code tracks the entire "wavefront" of beams row by row. It uses a single wide register (*is_beam_current*) to represent the current row, where each "ON" bit means a beam is present at that column. As the hardware scans through the map, it doesn't recursively trace paths; instead, it looks at the current row's beams and "projects" them into a next register for the future—beams hitting empty space fall straight down, while beams hitting a splitter mark their left and right neighbors in the next row and increment the counter. At the end of each row, the "future" becomes the "present," allowing the FPGA to process infinite merges and splits in a single linear pass from top to bottom without ever retracing its steps.
