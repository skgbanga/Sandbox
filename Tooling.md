Tools

  - objdump
    - -M intel: shown asm in intel syntax
    - -C show demangled names
    - -d disassemble sections which is code-like
    - -D disassemble everything
 
  - nm
    - -C demangle
    - -D dynamic symbols
 
  - readelf
    - -s show symbols
    - -a show the entire file (headers, symbols, sections)
    - -x 'section' show the hexdump of the section asked
    - -W wide view (important to use with c++filt)
  
  - gdb
    - disassemble /m : show disassembly along with source code
    - list: show the next 10 lines of the code to be executed
    - start: set a temporary background when the 'main' starts
    - starti: set a temporary background on the first instruction of program (way before main)
    - display: setup a watch window (e.g. display/10i $rip)
    - x: show memory contents (e.g. x/8c 0x400700, x/s carray)

  - hexdump
    - -C show it in canonical hex format (display printable chars)
    - -s 'offset': where to start from
    - -n 'num': read only these many bytes of data

  - hexedit (emac bindings)
    - 'Enter' -> search for address in hex

  - perf (complete this section)
    - stat
    - annotate
    - record
    - report
    - -e 'task-clock,cycles,instructions,cache-references,cache-misses,branches,branch-misses,faults,minor-faults,cs,migrations'
    - -r 'n': repeat this many times

  - taskset
