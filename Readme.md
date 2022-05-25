# HANDL Intro
HANDL is a high-level language inspired by the paradigms of GO. Our interest in creating HANDL was to build a language that would not be too complex for novel users to pick up. The specific group we are focusing on is musicians, and so we picked syntax that is similar to what musicians use: notes, phrases, songs, measures, etc.  We want musicians to see in HANDL something similar to what they see in their performance and compositional work. This way, they can be most comfortable learning about the syntax and norms of a programming language and also can create something truly interesting with it. They will be able to easily create guitar tabs (notation for playing guitar) using our code. Using HANDL, musicians could come up with some exciting algorithmically-generated tabs and delve into the world of computer-generated music, or even just use it for the creation of simple guitar tabs. Code will be converted by HANDL from syntax into human-readable Guitar tabs. In this way, HANDL will function somewhat as a DSL but will also be able to handle the average functions of most programming languages.

## File Structure
-  `scanner.mll`: scanner
-  `handlparse.mly`: parser
-  `ast.ml`: abstract syntax tree (AST)
-  `semant.ml`: semantic checking
-  `sast.ml`: semantically-checked AST
-  `handlirgen.ml`: IR generator
-   tests folder: has all our tests
-   `demo.sh` : script for compiling a test
-   `tests.sh` : script for compiling all tests in tests folder

## Features
- Language is strongly typed with static scoping
- Strict evaluation
- Custom types for Notes, Phrases, and Songs
- .play() method which generates guitar tabs 

### Team Members
- Henri Vrod (Project Manager): Ensured team-members were on top of each of their responsibilitys and guaranteed timely deliverables. Led creation of the Semantic checker. Coded implementation for If, Else, booleans, Logical Operators and Arrays in each level of the architecture. Set up meetings and pair-programming sessions. Made many of the basic testers.
- Pierre Rodgers (Language Guru): led the LRM creation, and helped resolve any questions for finalizing syntax. In the AST/SAST files, I focussed on loops, the Note type, and phrases. In IR gen, I worked most closely on Notes, Phrases, and Songs, getting them to store in memory. I also worked making sure that Songs could play by connecting the IR to C code, which was a tricky task involving making sure pointer types, arrays, and memory was passed correctly in the C function call in our OCaml code. I also helped to write some test scripts and a number of tests for the tests folder. 

- Max Acebal (System Architect): As System architect I ensured each phase of the compiler linked seamlessly (required a lot of going back and forth and correcting bugs within each file). I drove the endeavor for the Handl ir code generation phase by repurposing microc irgen.ml file for our use case. Helped solve significant memory errors with other team members for custom types and arrays. In the scanner and parser, I was initially responsible for Phrases, sequencing, return statements, separators, quotes, and Strings.
- Kyle Christopher (Tester):

### Building HANDL

```
ocamlbuild -use-ocamlfind handl.native -package llvm
gcc -c handlmusic.c
```

### Testing HANDL

```
sh tests.sh
./tests_results/<TEST_NAME>.exe
```
OR
```
sh demo.sh ./tests/<TEST_NAME>.handl
<TEST_NAME>.exe
```