.PHONY: clean compile_program run_program

#all: clean compile_program run_program

compile_program:
	Rscript -e "Rcpp::compileAttributes(); devtools::check()"

run_program:
	Rscript src/mainR.R

clean:
	cd src/ && rm -f *.o && rm -f *.so
