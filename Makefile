RSCRIPT = Rscript --no-init-file

all: install

document:
	${RSCRIPT} -e "roxygen2::roxygenise()"

install: document
	R CMD INSTALL .

test:
	${RSCRIPT} -e "devtools::test()"

check:
	${RSCRIPT} -e "devtools::check(document = FALSE)"

demo: install
	${RSCRIPT} inst/examples/make_demo.R

.PHONY: all document install test check demo
