.PHONY: clean compile_program run_program

#all: clean compile_program run_program

check: 
	Rscript -e "devtools::check()"

compile:
	Rscript -e "Rcpp::compileAttributes(); devtools::document()"

run:
	Rscript src/mainR.R

clean:
	cd src/ && rm -f *.o && rm -f *.so
