# OCamlEvaluator
## Build
### Prerequisites
* ocaml
* dune
* menhir
* oplot

### Build executable
Run ```dune build``` to compile the source
## Usage
```./oeval <flags>``` 

Flags:
* ```--load <def_file>``` By giving the name of the definitions file, it loads it, so you can use definitions on that file.
* ```--file <file_name>``` Selects the file mode instead of the repl.
* [Experimental] ```--erepl``` Selects the enchanced repl mode.