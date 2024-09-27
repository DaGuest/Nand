#  Nand to Tetris implementations

This is my implementation of project 6,7 and 8 in the course Nand to Tetris part 2.

## Objective project 6
The object was to build an assembly program that can translate Hack assembly code into machine code.
The implementation was made using Java.

### Running the assembler
To run the assembler:
1. Compile the java code using: `javac Assembler.java`
2. Run the Assembler with: `java Assembler xxx.asm`
3. The output is written into the _xxx.hack_ file.

## Objective project 7
The object was to build the first implementation of the VMTranslator that is able to translate VM code into assembly code.
The implementation was made using C++.

### Compiling the code
The VMTranslator can be compiled using clang++ (version 16.0). 
`clang++ vmtranslator.cpp codewriter.cpp parser.cpp -o VMTranslator`

## Objective project 8
The object was to build the second implementation of the VMTranslator that is able to translate VM code into assembly code.
The second implementation involves branching and function commands.
The implementation was made using C++.