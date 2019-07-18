.PHONY: compile doc check run_main clean

all: compile doc check

compile:
	Rscript -e "Rcpp::compileAttributes()"

doc:
	Rscript -e "devtools::document()"

check: 
	Rscript -e "devtools::check()"

run_main:
	Rscript src/mainR.R

clean:
	cd src/ && rm -f *.o && rm -f *.so
