
structure DataTypes = 
struct
datatype AST = PROG of BLK
and BLK = BLK of (DEC)*(CMD list) 
and DEC = DEC of (VARDECLS)*(PROCDECLS)
and VARDECLS = VARDECLS of RATDECLS*INTDECLS*BOOLDECLS
and PROCDECLS = PROCDECLS of PROCDEF list
and PROCDEF = PROCDEF of string*BLK
and INTDECLS = INTDECLS of (string list)*Type
and BOOLDECLS = BOOLDECLS of (string list)*Type
and RATDECLS = RATDECLS of (string list)*Type 
and CMD = Assign of string*exp | While of exp* (CMD list ) | Print of exp | Read of string | Call of string | ITE of exp*(CMD list)*(CMD list)
and Type = bigint | Bool | rational
and exp =  PLUS of exp*exp
        | MINUS of exp*exp
        | TIMES of exp*exp
        | DIV of exp*exp
        | MOD of exp*exp
        | TT 
        | FF
        | EQ of exp*exp
        | NEQ of exp*exp
        | LT of exp*exp
        | LEQ of exp*exp
        | GT of exp*exp
        | GEQ of exp*exp
        | VAR of string
        | INTI of Bigint.bigint
        | RATI of Rational.rational
        | NOT of exp
        | AND of exp*exp
        | OR of exp*exp
        | RPLUS of exp*exp
        | RMINUS of exp*exp
        | RTIMES of exp*exp
        | RDIV of exp*exp
        | INVERSE of exp
        | NEG of exp
        | RAAT of exp
        | MAKERAT of exp*exp
        | SHOWRAT of exp
        | TODEC of exp
        | SHOWDEC of exp
        | FROMDEC of string
        
end ; 