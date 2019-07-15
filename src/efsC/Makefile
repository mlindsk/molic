# https://www.gnu.org/software/make/manual/make.html#Introduction

.PHONY: all compile_program run_program

all: compile_program run_program

compile_program:
	g++ -o run \
	main.cpp \
	class_decomposable_graph.cpp \
	class_clique_graph.cpp \
	class_data_matrix.cpp \
	class_efs_graph.cpp \
	unordered_set_ops.cpp \
	misc_utils.cpp \
	entropy.cpp

run_program:
	./run

clean:
	rm *.o
