spacetime	= pkg

R		= R

default:
	rm -fr $(spacetime)*tar.gz
	make build
	make install

cl:
	(cd $(spacetime); svn2cl; mv ChangeLog inst/ )

release:
	make cl
	make build

nv:
	rm -fr spacetime_1*tar.gz
	$(R) CMD build --no-vignettes --force $(spacetime) # build latest version 
	$(R) CMD INSTALL spacetime_1*tar.gz

build:
	_R_BUILD_RESAVE_DATA_=best _R_BUILD_COMPACT_VIGNETTES_=qpdf $(R) CMD build $(spacetime)

stangle:
	(cd spacetime/inst/doc; echo "library(tools); Stangle(\"spacetime.Rnw\")" | R --no-save --no-restore)

install:
	$(R) CMD INSTALL spacetime_*tar.gz

check:
	rm -fr $(spacetime)*tar.gz
	make build
	_R_CHECK_CRAN_INCOMING_USE_ASPELL_=true $(R) CMD check --as-cran spacetime*gz

fullcheck:
	TZ="" make check
	TZ="CET" make check
	TZ="BST" make check
	TZ="GMT" make check
