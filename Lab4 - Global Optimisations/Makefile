# USES llvm-3.8.0
INCLUDES = -I/usr/lib/llvm-3.8/include -I gnu/ -I include/
CFLAGS = -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
CXXFLAGS =  -fPIC -fvisibility-inlines-hidden -pedantic -std=c++11 -ffunction-sections -fdata-sections -O2 -g -DNDEBUG  -fno-exceptions -D_GNU_SOURCE 

cc: src/cc.cpp gnu/c.tab.cpp gnu/c.lex.cpp src/semanticAnalysis.cpp src/localOpt.cpp src/optPasses.cpp src/codeGeneration.cpp 
	@clang++ -g gnu/c.tab.cpp gnu/c.lex.cpp src/semanticAnalysis.cpp src/localOpt.cpp src/optPasses.cpp src/codeGeneration.cpp src/cc.cpp -lm -ll -lfl $(INCLUDES) $(CXXFLAGS) $(CFLAGS) `llvm-config --ldflags --system-libs --libs all` -o $@

gnu/c.tab.cpp gnu/c.tab.hpp: gnu/c.y
	@bison -o gnu/c.tab.cpp -d gnu/c.y

gnu/c.lex.cpp: gnu/c.l gnu/c.tab.hpp
	@flex -o gnu/c.lex.cpp -l gnu/c.l

tests: cc
	@echo "TEST CASE 1:"
	./cc tests/sel_sort.c
	@echo "\n"
	@echo "TEST CASE 1: GENERATED CODE SIZE:"
	@wc generated_code.txt
	@echo "\n"
	@echo "TEST CASE 1: GENERATED CODE RESULTS:"
	@lli generated_code.txt
	@rm generated_code.txt
	@echo "\n"
	@echo "TEST CASE 1: OPTIMIZED CODE SIZE:"
	@wc optimised_code.txt
	@echo "\n"
	@echo "TEST CASE 1: OPTIMIZED CODE RESULTS:"
	@lli optimised_code.txt
	@rm optimised_code.txt
	@echo "\n"

	@echo "TEST CASE 2:"
	./cc tests/fib_recursive.c
	@echo "\n"
	@echo "TEST CASE 2: GENERATED CODE SIZE:"
	@wc generated_code.txt
	@echo "\n"
	@echo "TEST CASE 2: GENERATED CODE RESULTS:"
	@lli generated_code.txt
	@rm generated_code.txt
	@echo "\n"
	@echo "TEST CASE 2: OPTIMIZED CODE SIZE:"
	@wc optimised_code.txt
	@echo "\n"
	@echo "TEST CASE 2: OPTIMIZED CODE RESULTS:"
	@lli optimised_code.txt
	@rm optimised_code.txt
	@echo "\n"

	@echo "TEST CASE 3:"
	./cc tests/array_try.c
	@echo "\n"
	@echo "TEST CASE 3: GENERATED CODE SIZE:"
	@wc generated_code.txt
	@echo "\n"
	@echo "TEST CASE 3: GENERATED CODE RESULTS:"
	@lli generated_code.txt
	@rm generated_code.txt
	@echo "\n"
	@echo "TEST CASE 3: OPTIMIZED CODE SIZE:"
	@wc optimised_code.txt
	@echo "\n"
	@echo "TEST CASE 3: OPTIMIZED CODE RESULTS:"
	@lli optimised_code.txt
	@rm optimised_code.txt
	@echo "\n"

clean::
	rm -f gnu/c.tab.cpp gnu/c.tab.hpp gnu/c.lex.cpp cc c.output code.txt generated_code.txt ssa.txt optimised_code.txt
