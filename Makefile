.PHONY: compile doc check install build_site run_main clean

all: compile doc check

compile:
	Rscript -e "Rcpp::compileAttributes()"

doc:
	Rscript -e "devtools::document()"

check: 
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install()"

build_site:
	Rscript -e "pkgdown::build_site()"

run_main:
	Rscript src/mainR.R

clean:
	cd src/ && rm -f *.o && rm -f *.so
