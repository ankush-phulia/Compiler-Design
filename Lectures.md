# Compiler Design
## COL 728 Lecture Notes
### Basics

* Compilers vs Interpreters, and partial evaluation

  ##### Interpreters

  * Like a function, which maps code(program) and data(input to the program) to output


* * "Online" evaluation
  * Interpret : (Program, Data) → Output
  * Interpret : Program → Data → Output
  * Interpret : Program → (Data → Output)
  * Interpret : Program → Executable

  ##### Compilers

  * "Offline evaluation"

    * Compile : Program → Executable

    ##### Partial Evaluation

    * A program can be seen as a mapping of input data to output data
    * Prog : (I~static~ x I~dynamic~) → Output
    * Residual Program, possibly more efficient, Prog~*~ :  I~dynamic~ → Output
    * Specialiser : (Prog, I~static~) → Prog~*~ a partial evaluator

***



### Futamura Projections

* Describe when "Prog" is an interpreter, i.e. if I~static~ is source code, partial evaluation to a "Prog~*~" gives a version of the interpreter running only that code

* Essentially, I~dynamic~ is neglected

* This "specialised" version of the interpreter is written in the language in which the interpreter has been written (e.g. Assembly for JAVA)

* "Prog~*~" is effectively the compiled version of I~static~

  #### The projections

  1. Specialising an interpreter for given source code, yields an executable
     * Interpret : Prog → Exec
     * Specialising Interpret for prog, Spec1 : (Interpret, Prog) → Exec
     * Spec1 : Interpret → (Prog → Exec) [= Compile]
  2. Specialising the specialiser for the interpreter, yields a compiler
     * Spec1 : Interpret → Compile
     * Spec2 : (Spec1, Interpret) → Compile
     * Spec2 : Spec1 → (Interpret → Compile)
  3. Specialising the specialiser for itself, yields a tool that can convert any interpreter to an equivalent compiler
     * Spec2 : Spec1 → InterpretToCompileConverter

  #### Take-aways

  * Executable is the partial-evaluation of an interpreter for a program's source code, generated during *compilation*

  * Execution of the executable should ideally be faster than the execution of the interpreter on the original program, and this speed-up determines the "quality of compilation". A compiler must aim to optimise in both *space* and *time*.

    * A trivial compilation stores the interpreter and source code as a tuple, so on execution the entire thing runs again, leading to worse run-time
    * Smarter compilation involves substitution of I~static~ into the interpreter's implementation, which may involve things like constant-propagation, loop-unrolling, local optimisations for static statements, etc
    * Caching Generated code (very effective for loops), to avoid regenerating the same machine code many times

       * Static Compilation - Ahead-Of-Time - cache translation
          * *Offline*, no compilation cost at run time
           * *Expensive*, given a time budget, can't decide which part to optimise more (don't know which region of code is "hot")
           * Compile + Exec may be slower than plain interpreted (*no loops*)
        * Dynamic Compilation - *Just-In-Time* - machine code generated at run time, while caching translations for each program segment
           * Can spend more effort on optimising "hot" regions
            * If generated code is larger than program, more *space efficient* (FB 250MB vs. 90MB)
            * Pay compilation *cost at run time* - not too bad for small app running long (due to loops), can also benefit from multiple cores
     * Global optimisations - spanning multiple code segments due to

        * Sub-optimal code like `x = 1; x = 2`
        * Machine representation being richer than source code syntax (complex opcodes)
        * Optimisations leading to others (one substitution followed by many more). *Caveat* - optimisation can preclude others (e.g. *shift* precludes *multiply-and-add*)
        * Generating an optimal implementation for a given program specification in Turing's model is undecidable, and NP-hard for the finite model

***



### Why Compilers?

* Moore's Law running out of steam as transistor scaling approaches physical limits - source-to-drain leakage, limited gate metals/channel materials
* Process and materials engineers' innovations meant that architecture and systems was always playing catch-up, the OS and programming stacks of today largely similar to those in the 60's
* Many innovative ideas, haven't managed mainstream adoption, as we can't map human-written programs to these efficient architectural structures, and coding directly with these in mind is difficult/non-intuitive
* Compiler designs of today are reminiscent of yore, which handled simple opcodes, and have features like vectorisation, parallel constructs, etc. tacked on, leading them to become very large and complex pieces of code
* Program complexity has drastically increased, from calculators to NLP, leading to programming in higher levels of abstraction - objects and automatic memory management, MapReduce and functional languages, ML constructs like neural networks
* Higher levels of abstraction increase the gap between the programming language and the architectural abstractions, making the search space for optimal implementations even larger
* Domain-specific languages like ForTran for scientific computing(support for FLOPs, arrays, parallelism), SQL for business apps(persistence, report generation), C/C++ for systems programming(fine-grained control over resources), HTML for web dev, etc. all of which require different types of optimisation support. Building a general framework that covers all domains, yet provides the best optimisation is very hard problem.

------



### ForTran - Birth and Basic Design

* 1954 : IBM develops the 704, the first commercially-successful machine. It found that software costs greatly exceeded the hardware costs
* 1953 : John Backus introduces "Speed coding" - a small interpreted language much easier to program in than directly programming the machine. Didn't catch on as -
  - Much slower, 10 - 20x
  - Interpreter itself took 300 bytes of memory (which was 30% of all available memory)
* Leveraging this experience, and the fact that most computation was of scientific formulae, Backus developed a higher-level programming language to translate formulae into machine code, called it "ForTran"
* Developed from 1954 to 1957; by 1958, 50% of all programs were in ForTran1
* Compilers today mostly preserve the outline of ForTran - 
* * Lexical Analysis - recognising words/lexemes and grouping into tokens
  * Parsing - understand sentence *structure* and diagramming into trees
  * Semantic Analysis - understanding the *meaning*; reduces to equivalence checking, an undecidable problem. Involves type-checking, scope etc.
    * Limited extent performed by compilers - catches inconsistencies(types, etc.)
    * Ambiguity - variable binding(which one is being referred to?); commonly bind to the inner-most definition
    * Static analysis - program specification, concurrency safety, etc/
  * Optimisation - reduce time and space complexities, power consumption, resource usage(network, disk access). It requires precise semantic modelling, various subtleties emerge
    * Can `for (unsigned i = 0; i < n + 1; i++)` → `for (unsigned i = 0; i <= n; i++) ` ? No
    * Can `for (int i = 0; i < n + 1; i++) ` → `for (int i = 0; i <= n; i++) ` ? Yes
    * Can `(2 * i)/2` be changed to `i` ? No
    * Can `Y * 0` be changed to `0` ? No for floating point
  * Code Generation - translation to low-level, machine-understandable code

***
