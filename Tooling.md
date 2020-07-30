To see the assembly

 - objdump
    - -M intel: shown asm in intel syntax
    - -C show demangled names
    - -d disassemble sections which is code-like
    - -D disassemble everything


To see symbols
  - nm
    - -C demangle
    - -D dynamic symbols

  - readelf
    - -s show symbols
    - -a show the entire file (headers, symbols, sections)
    - -x <section> show the hexdump of the section asked
    - -W wide view (important to use with c++filt)


Debugging
  - gdb
    - disassemble /m : show disassembly along with source code
    - list: show the code to be executed
