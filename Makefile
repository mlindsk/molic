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

test:
	Rscript -e "devtools::test('~/Documents/phd/publications/molic')"

readme:
	Rscript -e "rmarkdown::render('README.Rmd')"

build_site:
	Rscript -e "pkgdown::build_site()"

build:
	Rscript -e "devtools::build()"; \
	cd /home/mads/Documents/phd/software/; \
	R CMD check --as-cran molic_2.0.0.tar.gz

clean:
	rm -f README.html
	cd src/ && rm -f *.o && rm -f *.so
