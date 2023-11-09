# felix
Approximate emulator of Felix-C-256 computer jobs with an Assiris assembler.

## The Felix-C-256

The Felix-C-256 computer was a mainframe computer produced in Romania in the 1970ies (https://ro.wikipedia.org/wiki/Felix_C).
It was more or less a copy of the IRIS-50/IRIS-80 computers produced in France by CII. It was produced by the 'Fabrica
de Calculatoare' near Bucharest. Both the computer and the production technology were licenced from CII.
It ran the SIRIS-2 operating system that ran also on the IRIS.

At their turn the IRISes were produced under licence from SDS (Scientific Data Systems, later XDS after acquisition by Xerox)
being similar to the SDS Sigma-7 and Sigma-9 computers. There is a SIMH simulator of the Sigma computers, with an operating
system (CP-V) in binary form that works like the original.

However, the IRISes and thus the Felixes, are not binary compatible. The single instruction format has
the same field, but not in the same order. In the Sigmas the order is: I,F,B,Q,X,D. In the IRISes and Felix
it is I,B,Q,F,X,D. Credits for this incredible innovation must probably be awarded to the CII engineers.

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

While felixes remain in operation, or at least on inventories, until the end of the eighties, their place
was overtaken in the romanian enterprises by PDP clones, mostly with RSX and RT-11, than by 8080 and Z80 clones running CP/M or ZX spectrum
operating systems or the probably locally developed SFDX, which was inspired by RSX. Then, 8086 PC-compatible clones
overtook the landscape. A hobbist community also developed around home-made ZX Spectrum clones, CP/M and also CP/M-68k. Few
unix installations were experimented here and there, until Linux appeared in 1992 (Romania joined back the free world in december 1989).

Fabrica de Calculatoare was caught by 1989 revolution while trying to clone a VAX. It survived for another couple
of years, than dissapeared. The whole 'pipera platform' (industrial campus) were it was found, togheter with
other computing machinery factories and institutes was transformed and is today a technological campus for
(mostly international) software companies.

Of the Felixes, almost nothing remained--except the people who had been initiated in computing and their
students and a few programming books and manual in anticariats.

We could not find any piece of software, in source or binary. There are Corals and Independents still
in operation in some informal museums, but no Felix. Almost nothing to be found about the IRISes online
either.

One book tries to provide a comprehensive and detailed overview, although
the information does not ammount to a reference manual.
(FELIX C 256--Structura si programarea calculatorului, Vasile Baltac et al, 1974).

## The Felix simulator

Based on this book, we try to implement a simulator, not of the Felix (in the SIMH style), but
of one single threaded Felix job that must consist of a restriction to the most commonly used instructions and of the ASSIRIS assembler, 
together with essential control instructions and macros. The job
is described by an Ascii file (Felix used EBCDIC) on the standard input, resulting in a printout
on the standard output. In the first version, there will be no other peripherals.
The input code is assembled in machine code that is probably the same with the Felix one,
and executed through interpretation.

The simulator is currently only for Linux. It is written in Ada and published under GPLv2.
(Incidently, one of the designers of the SIRIS-3 operating system, Ichbiah, later went
on to become the chief designer of the Ada-83 language.)



