# OCamlEvaluator
## Build
### Prerequisites
* ocaml
* opam
* ocamlfind
* makefile
* menhir

Execute ```eval $(opam env)```.
### Build executable
Run ```make``` to compile the source, and it will create a "oeval.native" executable
### Test the program version
Run ```make test``` to test the version
## Usage
```./oeval <flags>``` 

Flags:
* ```--load <def_file>``` By giving the name of the definitions file, it loads it, so you can use definitions on that file.
* ```--file <file_name>``` Selects the file mode instead of the repl.
* [Experimental] ```--erepl``` Selects the enchanced mode.