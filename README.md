# AlexC

AlexC is a compiled programming language that targets x86-64 assembly.

## Usage

I've included a makefile you can use to compile programs using AlexC. For example, if I have a file called `source.ac`:

```
fun start {
    emit 'Hello, World!\n';
    return 0;
}
```

I can compile the file with 

```
make source # Produces source.out
```

If you want to see the assembly code, you can add a `.s` extension to your make command:

```
make source.s
```

## Overview

AlexC was inspired by C, Golang, and Rust. I outline the compilation process below.

### Lexer

AlexC reads the raw source file and converts it into tokens.

### Parser

AlexC uses a hand-written PEG parser, which is by far my favorite thing to come out of this project. Despite using recursive descent, the parser elegantly handles left recursion. The parser produces both an abstract syntax tree for the program and an empty symbol table containing all scopes and variables.

### Intermediate Representation

AlexC is compiled to an intermediate representation language, which is itself compiled to X86_64 assembly. The intermediate representation language isn't a real language in the sense that you can't directly write it, but I gave the compiler the ability to print the IR language for debugging and for fun. For example, here's the intermediate representation form of the program from the usage section:

```
label _start
sbp
scope 2
prints 'Hello, World!\n'
move #0 ret
descope 2
exit ret
```

### Assembly Code

As the final step, AlexC converts the IR code into read X86_64 assembly. Managing registers was the most complicated part of this step. For example, here's the assembly version of our program, generated by AlexC:

```
.att_syntax
.text
.global _start
_start:
mov %rsp, %rbp
mov $1, %rax
mov $1, %rdi
mov $__data0, %rsi
mov $15, %rdx
syscall
mov $0, %rax
mov %rax, %rdi
mov $60, %rax
syscall
.data
__char:
.byte 0
__data0:
.ascii "Hello, World!\n"
```

## Issues

The major issues present in AlexC are:
1. Function calls with parameters don't work
2. AlexC occasionally panics and crashes due to an error with temporary values

## Future Work and Conclusion

AlexC is my first major compiler project, and wow compilers use a lot of tables!

![Tables GIF](media/tables.gif)

To get the negatives out of the way, I've lost a lot of steam for working on this project which I attribute to a distinct lack of direction going into it; the codebase changed as quickly and as often as my ideas which led to it being confusing and difficult to maintain. As such, AlexC has some issues which are likely to go unfixed until the end of time.

Overall, I'm really proud of this project and it was a smashing success in the sense that I learned a bunch about how compilers work. I'm certain to do another compiler project and when I do, this project will be an excellent launching point in terms of my abilities. Furthermore, the majority of the language works very well and I thinks it's super cool to see high-level code being converted into assembly via a process that I came up with.

## References

I used the following books as reference for this project:

- "Engineering a Compiler" 3e by Keith D. Cooper and Linda Torczon
- "Compilers: Principles, Techniques, and Tools" 2e by Alfred Aho, Monica Lam, Ravi Sethi, and Jeffrey Ullman

I was inspired by [Guido Van Rossum's PEG Parsing Series](https://medium.com/@gvanrossum_83706/peg-parsing-series-de5d41b2ed60)