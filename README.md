# About this compiler and Lacs:

## Complier features:
All the declaration is in *src/cs241e/assignments/ProgramRepresentation.scala*

### Assembler
This is a simple complier that supports our own Lacs language based on MIPS arch. We implemented several basic assembly instructions at first:
+ ADD
+ SUB
+ MULT
+ MULTU
+ DIV
+ DIVU
+ MFHI
+ MFLO
+ LIS
+ LW
+ SW
+ SLT
+ SLTU
+ BEQ
+ BNE
+ JR
+ JALR
All the other abstractions are developed from these instructions above.

### Label
We developed our first abstraction **label**, which is an abstraction for any offset in the sequence of code. 
Usage: 
>ADD(R1, R2, R3)
>Define(label1)
>...
>LIS(R3)
>Use(label1)
>JR(R3)
The code above can let us jump to the position where we define the label. The implementation is in *src/cs241e/assignments/Assembler.scala*

### Local variable and Stack
In order to store more information, 34 registers are not enough, so we developed local variables and store them on the Stack. Our Stack grows from the bottom of the memory to up. Every time we wants to store the variables, we have to store them in a chunk first, then we call Stack.allocate(chunk) to allocate space for the chunk. We call Stack.pop() to pop the space for the chunk.
The implementation of local variable is in *src/cs241e/assignments/Transformation.scala* and the implementation of Stack is in *src/cs241e/assignments/MemoryManagement.scala*

### Code Block, Scope and if statement
We implemented several code sequence abstractions:
+ Block: block is just a sequence of code, it is also a code
+ Scope: scope is a sequence of variable plus a code, where the code can use the variables in the scope
+ If statement: if statement took 5 parts: expression 1, a comparable operator, expression 2, then statement, else statement. If cmp(ep1, ep2) stores non-zero in R3, we will jump to then statement, else we jump to else statement
The implementation is in *src/cs241e/assignments/Transformation.scala*

### Procedure, nested procedure and closure
+ A procedure is an abstraction that encapsulates a reusable unit of code. The code that calls a procedure (the caller ) transfers control to the code of the procedure (also called the callee ) by modifying the program counter. The caller may also pass arguments for the parameters of the procedure. When the code of the procedure finishes executing, control transfers back to the instruction after the call of the procedure. Our procedure can also contain sub procedures, which can use the arguments or local variables defined in the main procedure.
+ A closure is a pair of a function pointer (address
of the code of the body of the function) and an environment (mapping free variables to their values), which can be stored in local variable. The implementation is in *src/cs241e/assignments/Transformation.scala*

### How to run these code?
In test/src, created a scala file and import all the dependencies.
Then define all the procedures you need in our abstraction. Call complierA6(seq(procs)) to convert them into machine code. Then call A4.loadAndRun(machineCode, debug = true/false) to get the end state.
> val machineCode = compilerA6(Seq(main, f,g, h))
> val endState = A4.loadAndRun(machineCode, debug = true)
> println(decodeSigned(endState.reg(3)))
The tests are located in *test/src/*


## Lacs Programming Language Specification
Lacs is a simple programming language. In Assignments 7-11, we will write a compiler from Lacs to MIPS machine language, using the code generation code that we have already implemented in Assignments 1-6. The main features of Lacs are:

+ values and variables of type integer and closures
+ arithmetic expressions
+ if-then-else expressions
+ procedures, including recursive procedures, nested procedures with nested scopes, and closures

### Lexical specification
A Lacs program is a sequence of tokens. Every valid token is one of the following:

+ ID: a string consisting of a letter (in the range a-z or A-Z) +  followed by zero or more letters and digits (in the range 0-9), but not equal to any of the keywords def, var, Int, if, else.
+ DEF: the string (keyword) def
+ VAR: the string (keyword) var
+ INT: the string (keyword) Int
+ IF: the string (keyword) if
+ ELSE: the string (keyword) else
+ NUM: a string consisting of a single digit (in the range 0-9) or two or more digits the first of which is not 0
+ LPAREN: the string (
+ RPAREN: the string )
+ LBRACE: the string {
+ RBRACE: the string }
+ BECOMES: the string =
+ EQ: the string ==
+ NE: the string !=
+ LT: the string <
+ GT: the string >
+ LE: the string <=
+ GE: the string >=
+ PLUS: the string +
+ MINUS: the string -
+ STAR: the string *
+ SLASH: the string /
+ PCT: the string %
+ COMMA: the string ,
+ SEMI: the string ;
+ COLON: the string :
+ ARROW: the string =>
+ COMMENT: the string // followed by any characters other than the newline character (ascii 10, \n in Scala)
+ WHITESPACE: one of the following characters: tab (ascii 9, \t in Scala), newline (ascii 10, \n in Scala), carriage return (ascii 13, \r in Scala), space (ascii 32)

Tokens that contain letters are case-sensitive; for example, Int is an INT token, while int is not.

A Lacs program must not contain a pair of consecutive tokens that both come from one of the following sets:

{ID, DEF, VAR, INT, IF, ELSE, NUM}
{EQ, NE, LT, LE, GT, GE, BECOMES, ARROW}
It is possible to write a program in which two such tokens appear one after the other by separating them with one or more WHITESPACE or COMMENT tokens.

WHITESPACE and COMMENT tokens are irrelevant to the syntactic specification of Lacs and to the meaning of a valid Lacs program. Therefore, they should be removed from the sequence of tokens after a Lacs program has been determined to satisfy the lexical specification.

### Syntactic specification
A context-free grammar for a valid Lacs program is:

+ terminal symbols: the set of valid tokens above, except WHITESPACE and COMMENT
+ nonterminal symbols: {defdefs, defdef, parmsopt, parms, vardef, type, typesopt, types, vardefsopt, defdefsopt, expras, expra, expr, term, factor, test, argsopt, args}
+ start symbol: defdefs
+ production rules:
+ defdefs -> defdef defdefs
+ defdefs -> defdef
+ defdef -> DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE
+ parmsopt -> parms
+ parmsopt -> 
+ parms -> vardef COMMA parms
+ parms -> vardef
+ vardef -> ID COLON type
+ type -> INT
+ type -> LPAREN typesopt RPAREN ARROW type
+ typesopt -> types
+ typesopt -> 
+ types -> type COMMA types
+ types -> type
+ vardefsopt -> VAR vardef SEMI vardefsopt
+ vardefsopt -> 
+ defdefsopt -> defdefs
+ defdefsopt -> 
+ expras -> expra SEMI expras
+ expras -> expra
+ expra -> ID BECOMES expr
+ expra -> expr
+ expr -> IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE
+ expr -> term
+ expr -> expr PLUS term
+ expr -> expr MINUS term
+ term -> factor
+ term -> term STAR factor
+ term -> term SLASH factor
+ term -> term PCT factor
+ factor -> ID
+ factor -> NUM
+ factor -> LPAREN expr RPAREN
+ factor -> factor LPAREN argsopt RPAREN
+ test -> expr NE expr
+ test -> expr LT expr
+ test -> expr LE expr
+ test -> expr GE expr
+ test -> expr GT expr
+ test -> expr EQ expr
+ argsopt -> args
+ argsopt -> 
+ args -> expr COMMA args
+ args -> expr

### Context-sensitive specification
A symbolic representation of the context-sensitive representation in the form of type inference rules is also available.

+ Each sequence derived from defdef is a declaration of a procedure. Each sequence derived from vardef is a declaration of a variable. The name of the procedure or variable is the lexeme corresponding to the child ID of the defdef or vardef. Names are case sensitive; for example, FOO and foo are distinct names.

+ Each procedure is associated with exactly one scope. The declaration subtrees of a procedure are the sequences derived from the children parmsopt, vardefsopt, and defdefsopt of the declaration of p. A procedure q whose declaration is in the sequences derived from the declaration subtrees of p is said to be nested in p. The scope of a procedure p contains all variables and procedures whose declarations are in the sequences derived from the declaration subtrees of p, except for those variables and procedures whose declarations are in the subsequences derived from the declaration subtrees of some other procedure q that is nested in p. A scope declares the names of all of the variables and procedures that it contains.

+ A scope must not contain two or more declarations with the same name.

+ A name is said to be used in procedure q when it appears as an ID in a sequence derived from the child expras of the declaration of q. When a name n is used in procedure q, a declaring scope of the use of n is a scope that declares n, and is either the scope of q or of some other procedure p in which q is nested. Every use of a name must have some declaring scope. The closest declaring scope of a use of n is the scope of some procedure p whose scope is a declaring scope of the use of n, and such that no other procedure q nested in p has a scope that is a declaring scope of the use of n. Each use of a name denotes the variable or procedure with that name declared in the closest declaring scope of the use.

+ A type is a sequence derived from type. A type is called a procedure type if it is derived from LPAREN typesopt RPAREN ARROW type. The sequence of types derived from the child typesopt of the procedure type is called the parameter types. The child type of the procedure type is called the return type. The type of a variable is the sequence derived from the child type of the vardef that declares the variable. The type of a procedure p is a procedure type whose parameter types are the types of the variables declared in the sequence derived from the child parmsopt of the declaration of p, and whose return type is the sequence derived from the child type of the declaration of p.

+ ID
The type of an ID that is a use of a name is the type of the variable or procedure denoted by the use of the name. The context-free grammar ensures that whenever an ID is a use of a name, the ID is the child of either an expra or a factor.
factor
The type of a factor deriving an ID is the type of that ID.
The type of a factor deriving NUM is Int.
The type of a factor deriving LPAREN expr RPAREN is the type of the expr.
When a factor derives factor LPAREN argsopt RPAREN, the type of the child factor must be a procedure type whose parameter types are the same as the types of the exprs occurring as children of the args derived from the argsopt. The type of a factor deriving factor LPAREN argsopt RPAREN is the return type of the child factor.
+ term
The type of a term that derives a factor is the type of the factor.
When a term derives term STAR factor, term SLASH factor, or term PCT factor, the types of the child term and factor must be Int, and the type of the resulting term is Int.
+ expr
The type of an expr that derives a term is the type of the term.
When an expr derives expr PLUS term, or expr MINUS term, the types of the child expr and term must be Int, and the type of the resulting expr is Int.
When an expr derives IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE, the type of the two exprs derived from test must be Int, the types of the two child expras must be the same, and the type of the resulting expr is the type of the two child expras.
+ expra
When an expra derives ID BECOMES expr, the type of the ID and the expr must be the same, and the ID must denote a variable, not a procedure. An expra always derives either expr or ID BECOMES expr; in both cases, the type of the expra is the type of the child expr.
+ expras
The type of an expras that derives expra SEMI expras is the type of the child expras.
The type of an expras that derives expra is the type of the expra.
defdef
+ A defdef derives DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE. The type of the child expras must be the same as the child type.
The first procedure declared in a Lacs program is the main procedure. The type of the main procedure must be (Int, Int) => Int.

### Semantics
Any Lacs program that obeys the lexical, context-free, and context-sensitive specifications above is also a sequence of valid Scala functions, with one exception. Scala requires every variable to have an initializer (e.g. var x: Int = 42;), but Lacs prohibits such initializers (e.g. var x: Int;). To transform a Lacs program into Scala functions, add the initializer 0 for every variable of type Int, and the initializer null for every variable of procedure type.

The meaning of a Lacs program is defined to be identical to the meaning of the corresponding Scala functions. A Lacs program is given two integers as input (in registers 1 and 2), and produces an integer as output (in register 3). The output value computed by the Lacs program must be the same as the return value when the first of the Scala functions is called with the same two input integers. If the execution of the Scala function aborts with an exception, then the meaning of the Lacs program is undefined.