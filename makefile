# Makefile for ThinLisp, requires the GNU varient of make.

MF = makefile-`uname -s | tr 'A-Z' 'a-z'`

ifdef THREAD
THREADARG = THREAD=1
else
THREADARG =
endif

all : TAGS docs/tl-manual.pdf lisp bin

TAGS : tlt/lisp/*.lisp tl/lisp/*.lisp tl/c/*.c lecho/lisp/*.lisp lecho/c/*.c makefile
	find tl tlt lecho \( -name '*.[ch]' -o -name '*.lisp' \) -print | etags -

docs/tl-manual.pdf : docs/tl-manual.texinfo
	cd docs && $(MAKE) -k

lisp :
	./bootstrap

bin :
	cd tl/bin && $(MAKE) -kf $(MF)
	cd lecho/bin && $(MAKE) -kf $(MF)

opt :
	cd tl/opt && $(MAKE) -kf $(MF) OPT=1
	cd lecho/opt && $(MAKE) -kf $(MF) OPT=1

clean :
	csh -f -c 'rm -rf {tlt,tl,lecho}/{dev,macro,bin,opt} dist src TAGS'
	rm -rf lecho/c/*.tlt tl/c/*.tlt
	cd docs && $(MAKE) clean

lisp-clean :
	csh -f -c 'rm -rf {tlt,tl,lecho}/{dev,macro,bin,opt} dist src'
	rm -rf lecho/c/*.tlt tl/c/*.tlt

# To make a distribution, edit the VERSIONS file in this directory to set the
# version number for the release, check it in, then follow the directions in the
# VERSIONS file.  Those instructions will eventually require you to run make on
# the distributions target below.

distribution : 
	if [ -d dist ] ; then rm -rf dist ; fi
	if [ ! -d src ] ; then echo You must export an SRC before making distribution ; exit 1 ; fi
	cp ../README src
	mkdir dist
	cp src/VERSION dist/VERSION
	mkdir thinlisp-`head -1 dist/VERSION`
	cp src/INSTALL dist/thinlisp-`head -1 dist/VERSION`-install.txt
	mv src thinlisp-`head -1 dist/VERSION`/src
	tar czvf dist/thinlisp-`head -1 thinlisp-*/src/VERSION`.tgz thinlisp-`head -1 dist/VERSION`
	zip -r dist/thinlisp-`head -1 VERSION`.zip thinlisp-`head -1 dist/VERSION`
	bzip2 -cz `find thinlisp-* -type f -print` > dist/thinlisp-`head -1 dist/VERSION`.bz2
	rm dist/VERSION
