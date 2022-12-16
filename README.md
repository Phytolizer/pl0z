# pl0z

PL/0 compiler in Zig. Targets C, then compiles the C with `zig cc`.

# Tasks

- [X] Lexer
- [X] Parser
- [X] Code Generator
- [X] Invoke Zig to target machine code

# Language Extensions

For the most part this is standard PL/0. However, I added some keywords
for I/O (`readchar`, `readint`, `writechar`, `writeint`).
You can see an example for their usage in [interactive.pl0](fun/interactive.pl0).
