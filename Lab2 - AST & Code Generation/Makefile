# USES llvm-3.8.0
INCLUDES = -I/usr/lib/llvm-3.8/include  -I gnu/ -I include/
CFLAGS = -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
CXXFLAGS = -fPIC -fvisibility-inlines-hidden -pedantic -std=c++11 -ffunction-sections -fdata-sections -O2 -g -DNDEBUG  -fno-exceptions -D_GNU_SOURCE 

cc: src/cc.cpp gnu/c.tab.cpp gnu/c.lex.cpp src/semanticAnalysis.cpp src/codeGeneration.cpp
	clang++ -g gnu/c.tab.cpp gnu/c.lex.cpp src/semanticAnalysis.cpp src/codeGeneration.cpp src/cc.cpp -lm -ll -lfl $(INCLUDES) $(CXXFLAGS) $(CFLAGS) `llvm-config --ldflags --system-libs --libs core` -o $@

gnu/c.tab.cpp gnu/c.tab.hpp: gnu/c.y
	bison -o gnu/c.tab.cpp -d gnu/c.y

gnu/c.lex.cpp: gnu/c.l gnu/c.tab.hpp
	flex -o gnu/c.lex.cpp -l gnu/c.l

tests: cc
	./cc tests/sel_sort.c
	lli generated_code.txt
	rm generated_code.txt

	./cc tests/fib_recursive.c
	lli generated_code.txt
	rm generated_code.txt

	./cc tests/array_try.c
	lli generated_code.txt
	rm generated_code.txt

clean::
	rm -f gnu/c.tab.cpp gnu/c.tab.hpp gnu/c.lex.cpp cc c.output code.txt
