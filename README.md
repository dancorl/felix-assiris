# FELIX-ASSIRIS
# Approximate emulator of Felix-C-256 computer jobs with an Assiris assembler.

The official page of the felix-assiris emulator is at: http://dan.corlan.net/software/emulators/felix-assiris/

There is also a zenodo record at: https://zenodo.org/records/10117525

This (github page) will no longer be maintained, please follow the above links, primarily the official page,
for the latest developments.

## History of the Felix-C-256

The Felix-C-256 computer was a mainframe computer produced in Romania in the 1970ies (https://ro.wikipedia.org/wiki/Felix_C).
It was more or less a copy of the IRIS-50/IRIS-80 computers produced in France by CII. It was produced by the 'Fabrica
de Calculatoare' near Bucharest. Both the computer and the production technology were licenced from CII.
It ran the SIRIS-2 operating system that ran also on the IRIS.

At their turn the IRISes were produced under licence from SDS (Scientific Data Systems, later XDS after acquisition by Xerox)
being similar to the SDS Sigma-7 and Sigma-9 computers. There is a SIMH simulator of the Sigma computers, with an operating
system (CP-V) in binary form that works like the original.

However, the IRISes and thus the Felixes, are not binary compatible. The single instruction format has
the same field, but not in the same order. In the Sigmas the order is: I,F,B,Q,X,D. In the IRISes and Felix
it is I,B,Q,F,X,D.

The opcodes F are also different. Generally the opcodes perform the same functions, but the usual mnemonics
are different as are the codes that correspond to the same actual instruction.

Otherwise, the addressing modes, the organisation of the memory and memory management seem to be the same.

The Felix was the only computer available in Romania in the seventies and early eighties. Romania was a socialist
country from the Warsaw treaty and there were no private enterprises. The Felixes were to be found in state enterprises and
institutions, including 'county computation centers' and could not be purchesed independently. Their unreability
was legendary. However, they allowed the introduction of computer programming and education curricula relying
on a specific, practical, existing and theoretically available computer system. One had something to actually talk
about. A whole generation of computer engineers and programmers was introduced into the field on the Felix Fortran,
Cobol and Assiris, as well as Siris control language, starting from high school. Most of those who wrote programs on paper and discussed
their functioning never had the opportunity to see a Felix in real life, much less to punch their jobs on cards
and actually submit them for execution. For some, this created a lingering frustration.

While felices remained in operation, or at least on inventories, until the end of the eighties, their place
was overtaken in the romanian enterprises by PDP clones, mostly with RSX and RT-11, then by 8080 and Z80 clones running CP/M or ZX spectrum
operating systems or the probably locally developed SFDX, which was inspired by RSX, but for the Z80. Later, 8086 PC-compatible clones
overtook the landscape. A hobbist community also developed around home-made ZX Spectrum clones, CP/M and also CP/M-68k. Few
unix installations were experimented here and there, until Linux appeared in 1992 (Romania joined back the free world in december 1989).

Fabrica de Calculatoare was caught by the 1989 revolution while trying to clone a VAX. It survived for another couple
of years, than dissapeared. The whole 'Pipera platform' (industrial campus) were it was found, together with
other computing machinery factories and institutes was transformed and is today a technological campus for
(mostly international) software companies.

Of the Felixes, almost nothing remained--except the people who had been initiated in computing and their
students and a few programming books and manuals in anticariats.

We could not find any piece of software, in source or binary other than a few examples in the above mentioned
books. There are Corals and Independents still in operation in some informal museums, but no Felices. 
Almost nothing to be found about the IRISes online either.

One book tries to provide a comprehensive and detailed overview, although
the information does not ammount to a reference manual.
(FELIX C 256--Structura si programarea calculatorului, Vasile Baltac et al, 1974).

## The Felix simulator

Based on this book, we try to implement a simulator, not of the Felix (in the SIMH style), but
of one single threaded Felix job that must consist of a restriction to the most commonly used instructions and of the ASSIRIS assembler, 
together with essential control instructions and macros. The job
is described by an Ascii file (Felix used EBCDIC) on the standard input, resulting in a printout
on the standard output. The input code is assembled in machine code that is probably the same with the Felix one,
and executed through interpretation.

The simulator is currently only for Linux. It is written in Ada and published under GPLv2.
(Incidently, one of the designers of the SIRIS-3 operating system, Ichbiah, later went
on to become the chief designer of the Ada-83 language.) It is a simple program, using
only the standard libraries, it should be easy to port on other systems.

## How to install

You must install the GNU NYU Ada Translator (gnat), the Ada component of the gcc compiler.
On debian derived Linuxes use: 

    apt install gnat

Then, you unpack the distribution zip or tar.gz in a directory and give the command `make'.

To test a job say:

    ./felix <hello_world.assiris

Sau ./felix -h or felix anything to get an 'about' and then a 'help'.

For more, you should really 'use the force' (read the source).

## The output of the ". LIST 'HELP' " control card

The help is reproduced below:


    GENERALITIES 

    This version only has 64K of memory and only knows some of the instructions:
    AD4I BRU BCF BCT BAL CP1I CP1 CP2 CP4 EO2 EO4 EX2 EX4 LDC2 LDC4 LD1I LD2I LD1
    LDL2 LDH2 LD4 LDM LD4I MG2 MG4 ST1 ST4 STH2 STM SB4I SB4
    to which we added a couple more (see below): PRINT HALT

    Only the direct and indirect addressing modes are implemented in this version.

    The indirect addressing mode works for exactly one indirection.


    CONTROL CARDS 

    The following cards (LIST and CONF) can appear anywhere in the input stream:

    . LIST opt{,opt}* where opt can be:

         'HELP'     -- include this help text in the listing      
         'ABOUT'    -- introduction to the FELIX/ASSIRIS system
         'SYMS'     -- the symbols table
         'LINKS'    -- the linkings performed by the link editor up to that point
         'DUMP'     -- `VIDAGE MEMOIRE' at that point
          MSG:'x'    -- display the message at that point

    . CONF opt{,opt}* -- currently is ignored, it will provide for configuration of the system

    The following cards can only appear in a specifc sequence.
    The sequence is: JOB, COMPILE, LINK, RUN, EOJ; then, you may repeat.
    COMPILE must be follwed by ASSIRIS (. COMPILE ASSIRIS)
    RUN will admit one option, KINS:n where n is the number of thousands of
    instructions to run. Without it, the simulator only runs 500 instructions
    then stops (as was necessary in early tests).

    Otherwise, you may add any options to the cards, but they are currently ignored.
    
    
    DIRECTIVES

    Directives are cards that can occur only between '. COMPILE ASSIRIS' and 'END'
    The directives: ORG, EQU, DS, DB, ALIGN, work as expected. EQU defines a symbol.
    ORG changes the address (that must be its argument) where the assembler generates code
    DS is followed by a '-delimited string, the ASCII characters of which it puts into memory
    in succesive locations

    DB is followed by a sequence of byte-sized numbers (0..255), comma separated, that ar put into memory in sequence.
    the number can be: [-]ddd, decimal numbers; X'xx' hexadecimal, C'c' characters.

    ALIGN is essential. Before assemblying machine code, the assembly address must be aligned to 4 bytes.
    After DSs and DBs, always use ALIGN to synchronise the address to a 4-byte alignment, if code follows.
    ALIGN take an optional argument about at what pace to align, in bytes. The default is 4.

    END X must include this X which is the address, usually a label, where RUN will start execution.

    CSECT is ignored.

    Label expressions are not implemented, you can't say 'BRU ADDR+8' or something


    NEW INSTRUCTIONS

    HALT will finish running

    PRINT,r will print the LSB of register r as an ascii character on stdout

    PRINT,r C'd' will print the register r as a decimal on stdout

    PRINT,r C'xy' will print the two ASCII characters x and y on stdout






