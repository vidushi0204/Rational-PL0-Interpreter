Part of Programming Languages Course (COL226) in 2022-23, Semester II at IIT Delhi

## Steps to Run the Interpreter

1. **Open the Command Line and Navigate to the Interpreter Directory**

    ```bash
    $ sml a.sml
    ```

    *This command will compile all code files, create the Abstract Syntax Tree, and then run the parser on this tree.*

2. **Provide Input and Output File Names**

    You will be prompted to enter the name of the input file (`.rat`) and the output file (`.txt`).

3. **Execution**

    The input will be read from the input file and the output will be written to the output file by the function `interpret(inputfile, outputfile)` defined in `calc.sml`.

## Design Decisions

1. **Comments**
    - Multi-line and single-line comments are enclosed within `(* *)`.

2. **Variable Syntax**
    - While executing `read`, `print`, or `make_rat` commands, the variable must be enclosed in round parentheses.

3. **File Extensions**
    - Input file extension: `.rat`
    - Output file extension: `.txt`

## Grammar

*Keywords are enclosed in `<>` for clarity.*

```plaintext
start       => blk 
blk         => dec comseq
dec         => vardecls procdecls
procdecls   => proclist
proclist    => ε | procdef; proclist
procdef     => <procedure> VARIABLE blk 

vardecls    => ratdecls intdecls booldecls 
ratdecls    => ε | <rational> varlist; 
intdecls    => ε | <integer> varlist; 
booldecls   => ε | <boolean> varlist; 
varlist     => VARIABLE | VARIABLE, varlist  

comseq      => {commands} 
commands    => ε | command; commands 
command     => <print>(expression)  
                | VARIABLE := expression 
                | <while> expression <do> comseq <od>
                | <if> expression <then> comseq <else> comseq <fi>
                | <call> VARIABLE 
                | <read> (VARIABLE) 

expression         => (expression)
                | expression < = expression 
                | expression < expression
                | expression = expression 
                | expression <> expression
                | expression > expression 
                | expression >= expression 
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
                | <make_rat> (expression, expression)
```

## Definition of Terminals and Non-Terminals

### Terminals

```plaintext
%term   LBRACE | RBRACE | LPAREN | RPAREN | EOF | NOT | OR | AND | WRITE | DO | OD | ASSIGN | BOOLEAN | INTEGER |
        LT | LEQ | NEQ | EQ | GT | GEQ | WHILE | SEMI | COMMA | PRINT | CALL | IF | THEN | ELSE | FI | READ |
        PLUS | MINUS | TIMES | DIV | MOD | RPLUS | RMINUS | RTIMES | RDIV | INVERSE | NEG | VOOR | MAKERAT | SHOWRAT | SHOWDEC |
        FROMDEC | TODEC | RAAT | TRUE | FALSE | PROCEDURE | RATIONAL | RAT of Rational.rational | INT of Bigint.bigint | VARIABLE of string
```

### Non-Terminals

```plaintext  ← (Added this line to open the code block)
%nonterm start of AST | blk of BLK | vardecls of VARDECLS | comseq of CMD list | commands of CMD list | command of CMD |
         expression of Type * exp | intdecls of INTDECLS | ratdecls of RATDECLS | booldecls of BOOLDECLS | varlist of string list | dec of DEC | procdecls of PROCDECLS | proclist of PROCDEF list | procdef of PROCDEF
``` 

