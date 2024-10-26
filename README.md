# OCamlEvaluator
## Build
### Prerequisites
* ocaml
* dune
* menhir
* oplot

### Build executable
Run ```dune build``` to compile the source
## Running the program
```./oeval <flags>``` 

Flags:
* ```--load <def_file>``` By giving the name of the definitions file, it loads it, so you can use definitions on that file.
* ```--file <file_name>``` Selects the file mode instead of the repl.
* [Experimental] ```--erepl``` Selects the enchanced repl mode.

## Expressions
* **Calculate**: The default mode, enter an operation to be evaluated
  * Example: ```sqrt(2)*5``` or ```0xFF+0b10```
* **DEF**: Definitions. Allows to define functions and variables
  * Example: ```DEF x 10``` or ```DEF f(x) sin(x)/x```
* **DER**: Derivative. Derivates the entered expression symbolically with respect to the specified variable
  * Example: ```DER x^2 x```
* **SIM**: Simplify. Simplifies the entered expression
  * Example: ```SIM x*0```
* **CONV**: Unit conversion. Converts from the first specified unit to the second one 
  * Example: ```CONV BIN 0xFF```
* **BASE**: Base conversion. Converts the number to the specified base (BIN, OCT, HEX)
  * Example: ```BASE BIN 0xFF```
* **SOLVE**: Solves the expression == 0 for the specified variable
  * Example: ```SOLVE 2*x+1 x```
* **PLOT**: Plots the function in a new window as a function of the specified variable on a specified range
  * Example: ```PLOT sin(x) x 0 2*pi```