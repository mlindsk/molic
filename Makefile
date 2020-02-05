.PHONY: compile doc check install build_site run_main test clean

all: compile doc check

compile:
	Rscript -e "Rcpp::compileAttributes()"

doc:
	Rscript -e "devtools::document()"

check: 
	Rscript -e "devtools::check()"

check_fast: 
	Rscript -e "devtools::check(build_args = c('--no-build-vignettes'), args = c('--no-build-vignettes'))"

install:
	Rscript -e "devtools::install()"

build_site:
	Rscript -e "pkgdown::build_site()"

run_main:
	Rscript src/mainR.R

test:
	Rscript -e "devtools::test('~/Documents/phd/publications/molic')"

readme:
	Rscript -e "rmarkdown::render('README.Rmd')" #; \
	# convert man/figures/README-gengraph-1.png -gravity south -chop 0x100 man/figures/README-gengraph-1.png; \
	# convert man/figures/README-gengraph-1.png -gravity north -chop 0x80 man/figures/README-gengraph-1.png; \
	# convert man/figures/README-acc-1.png -gravity south -chop 0x100 man/figures/README-acc-1.png; \
	# convert man/figures/README-acc-1.png -gravity north -chop 0x80 man/figures/README-acc-1.png; \
	# convert man/figures/README-unacc-1.png -gravity south -chop 0x100 man/figures/README-unacc-1.png; \
	# convert man/figures/README-unacc-1.png -gravity north -chop 0x80 man/figures/README-unacc-1.png; \
	# convert man/figures/README-var-select1-1.png -gravity south -chop 0x100 man/figures/README-var-select1-1.png; \
	# convert man/figures/README-var-select1-1.png -gravity north -chop 0x80 man/figures/README-var-select1-1.png; \
	# convert man/figures/README-var-select2-1.png -gravity south -chop 0x100 man/figures/README-var-select2-1.png; \
	# convert man/figures/README-var-select2-1.png -gravity north -chop 0x80 man/figures/README-var-select2-1.png; \
	firefox README.html

clean:
	rm -f README.html
	cd src/ && rm -f *.o && rm -f *.so
