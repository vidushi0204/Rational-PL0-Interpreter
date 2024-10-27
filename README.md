-> STEPS TO RUN THE INTERPRETER

1. On command line, open the directory containing the interpreter and type
    -$ sml a.sml
*This command will compile all code files, create the Abstract Syntax Tree and then run the parser on this tree.*

2. You wil be prompted to enter the name of input file (.rat) and output file (.txt).

3. The input will be read from input file and the output is written in output file by function *interpret(inputfile,outputfile)* defined in calc.sml.


-> DESIGN DECISIONS
1. Multi/Single line COMMENTS are enclosed within (* *)
2.  While executing read/print/make_rat command, the variable must be enclosed in round parentheses.
3. Extension of input file -> .rat ; output file -> .txt


-> GRAMMAR (Keywords enclosed in <> for clarity)
start       =>  blk 
blk         =>  dec comseq
dec         =>  vardecls procdecls
procdecls   =>  proclist
proclist    =>  ε | procdef; proclist
procdef     =>  <procedure> VARIABLE blk 

vardecls    =>  ratdecls intdecls booldecls 
ratdecls    =>  ε | <rational> varlist; 
intdecls    =>  ε | <integer> varlist; 
booldecls   =>  ε | <boolean> varlist; 
varlist     =>  VARIABLE | VARIABLE,varlist  

comseq      =>  {commands} 
commands    =>  ε | command; commands 
command     =>  <print>(expression)  
                | VARIABLE  := expression 
                | <while> expression <do> comseq <od>
                | <if> expression <then> comseq <else> comseq <fi>
                | <call> VARIABLE 
                | <read> (VARIABLE) 

expression         => (expression)
                | expression < = expression 
                | expression < expression
                | expression  = expression 
                | expression <> expression
                | expression > expression 
                | expression > = expression 
                | expression + expression 
                | expression - expression
                | expression * expression  
                | expression / expression
                | expression % expression
                | expression .+. expression
                | expression .-. expression
                | expression .*. expression 
                | expression ./. expression
                | <inverse> expression
                | ~ expression
                | expression && expression
                | expression || expression 
                | ! expression 
                | tt 
                | ff  
                | VARIABLE          
                | INT
                | RAT
                | <rat> expression 
                | <make_rat> (expression,expression) 

-> Definition of terminals and non-terminals
%term   LBRACE | RBRACE | LPAREN | RPAREN | EOF | NOT | OR | AND| WRITE | DO | OD | ASSIGN | BOOLEAN | INTEGER |
        LT | LEQ | NEQ | EQ | GT | GEQ | WHILE | SEMI | COMMA | PRINT | CALL | IF | THEN | ELSE | FI | READ |
        PLUS | MINUS | TIMES | DIV | MOD | RPLUS | RMINUS | RTIMES | RDIV | INVERSE | NEG | VOOR | MAKERAT | SHOWRAT | SHOWDEC |
        FROMDEC | TODEC | RAAT | TRUE | FALSE | PROCEDURE | RATIONAL  | RAT of Rational.rational| INT of Bigint.bigint | VARIABLE of string
        
%nonterm start of AST | blk of BLK | vardecls of VARDECLS | comseq of CMD list | commands of CMD list | command of CMD |
         expression of Type * exp | intdecls of INTDECLS | ratdecls of RATDECLS | booldecls of BOOLDECLS | varlist of string list | dec of DEC | procdecls of PROCDECLS | proclist of PROCDEF list | procdef of PROCDEF


-> FILES
1. a.sml
2. calc.grm
3. calc.lex
4. calc.sml
5. datatypes.sml
6. Rational.sml
7. README.md
8. sources.cm


-> ACKNOWLEDGEMENTS
1. https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=43247bea84122c52aa2d86aa86a8f4997825d419
2. https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html
3. https://www.cs.princeton.edu/~appel/modern/ml/ml-lex/manual.html 
4. https://github.com/ChinmayMittal/COL226/tree/main/Assignment%203