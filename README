           ThinLisp
             1.0

  What is it?
  -----------

ThinLisp is an open source Lisp to C translator for delivering commercial
quality, Lisp-based applications.  It implements a subset of Common Lisp with
extensions.  ThinLisp itself is written in Common Lisp, and so must run on top
of an underlying Common Lisp implementation such as Allegro, MCL, or CMU Lisp.
The C code resulting from a translation can then be independently compiled to
produce a small, efficient executable image.

ThinLisp is not a typical Lisp implementation in that it does not implement a
garbage collector or many of the other run-time development features of other
Lisps.  ThinLisp is designed for producing high quality deliverable C libraries
and executables from Lisp sources.  Originally designed for real-time control
applications, ThinLisp stresses run-time performance at the expense of some
development time conveniences.  This is exactly the opposite design philosophy
from many Lisps, but that's what makes this one thin!

ThinLisp is the current generation (4th) of a series of Lisp to C
translators used to deploy a large Lisp system (1 million lines) built
starting around 1990.

The emitted C code has been tested in the past on 19 different platforms
including Windows 95/98 and Windows NT for Intel and Alpha under both Cygnus and
Visual C; UNIXes from Linux, FreeBSD, Sun, HP, IBM, SGI, Aviion, DEC UNIX, OSF,
NEC, and Motorola; and VMS for VAX and Alpha.  Though it has been some time
since those platforms have been checked, we believe that there has been little
change to the underlying C structures used, and so the generated C code should
remain extremely portable.

ThinLisp produces compile time warnings for uses of inherently slow
Lisp operations, for consing operations, and for code that cannot be optimized
due to a lack of sufficient type declarations.  These warnings can be suppressed
by improving the code, or through use of lexical declarations acknowledging that
the code is only of prototype quality.  Code meeting the stringent requirements
imposed by ThinLisp generally cannot by sped up by rewriting it by hand in C.

The development environment for a project using ThinLisp is provided by
your favorite Common Lisp implementation.  You author, debug, and test
your program in that environment and when you wish to deploy the program
you translate it to C and compile and link it in on your the platform of
choice.  During it's history programs in the ThinLisp dialect have been
deployed on over a dozen different platforms.

The copyrights to the ThinLisp sources are held by the ThinLisp Group.

  Contacts
  --------

Web Site: http://www.thinlisp.org/

See there for all mailing list details, distributions, bug reporting etc.

  Documentation
  -------------

The documentation is maintained as a set of info files. The
file src/docs/tl-manual.info is the good starting point.

[[ The doc is not currently deployed on the project website ]]

  Installation
  ------------

See the INSTALL file in this directory, i.e. the thinlisp-1.0 module.


  Licensing
  ---------

See the LICENSE file in this directory.


  Acknowledgments
  ----------------

[[tbd]]

