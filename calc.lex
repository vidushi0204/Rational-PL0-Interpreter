structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])
%%
%header (functor CalcLexFun(structure Tokens: Calc_TOKENS));
%state COMMENT;
alpha=[A-Za-z];
digit=[0-9];
alphanum = [A-Za-z0-9] ;
comment = [A-Za-z0-9];
ws = [\ \t];

%%
<INITIAL>\n       => (pos := (!pos) + 1; lex());
<INITIAL>{ws}+    => (lex());


<INITIAL>"integer"       => (Tokens.INTEGER(!pos , !pos )) ;
<INITIAL>"boolean"       => (Tokens.BOOLEAN(!pos , !pos )) ;
<INITIAL>"rational"       => (Tokens.RATIONAL(!pos , !pos )) ;

<INITIAL>"tt" => ( Tokens.TRUE(!pos , !pos ) ) ;
<INITIAL>"ff" => (Tokens.FALSE(!pos , !pos )) ;

<INITIAL>"if"       => (Tokens.IF(!pos , !pos )) ;
<INITIAL>"then"       => (Tokens.THEN(!pos , !pos )) ;
<INITIAL>"else" => (Tokens.ELSE(!pos , !pos )) ; 
<INITIAL>"fi"        => (Tokens.FI(!pos , !pos )) ;

<INITIAL>"while"       => (Tokens.WHILE(!pos , !pos )) ;
<INITIAL>"do"       => (Tokens.DO(!pos , !pos )) ;
<INITIAL>"od"       => (Tokens.OD(!pos , !pos )) ;
<INITIAL>"procedure" => (Tokens.PROCEDURE(!pos , !pos )) ; 
<INITIAL>"print" => (Tokens.PRINT(!pos , !pos )) ; 
<INITIAL>"call" => (Tokens.CALL(!pos , !pos )) ; 
<INITIAL>"read" => (Tokens.READ(!pos , !pos )) ; 

<INITIAL>"var" => (Tokens.VOOR(!pos , !pos )) ;

<INITIAL>"~"       => (Tokens.NEG(!pos , !pos )) ;

<INITIAL>"inverse"       => (Tokens.INVERSE(!pos , !pos )) ;
<INITIAL>".+."       => (Tokens.RPLUS(!pos , !pos )) ;
<INITIAL>".-."       => (Tokens.RMINUS(!pos , !pos )) ;
<INITIAL>".*."       => (Tokens.RTIMES(!pos , !pos )) ;
<INITIAL>"./."       => (Tokens.RDIV(!pos , !pos )) ;

<INITIAL>"make_rat"       => (Tokens.MAKERAT(!pos , !pos )) ;
<INITIAL>"rat"       => (Tokens.RAAT(!pos , !pos )) ;
<INITIAL>"showRat"       => (Tokens.SHOWRAT(!pos , !pos )) ;
<INITIAL>"showDecimal"       => (Tokens.SHOWDEC(!pos , !pos )) ;
<INITIAL>"fromDecimal"       => (Tokens.FROMDEC(!pos , !pos )) ;
<INITIAL>"toDecimal"       => (Tokens.TODEC(!pos , !pos )) ;

<INITIAL>"+"       => (Tokens.PLUS(!pos , !pos )) ;
<INITIAL>"-"       => (Tokens.MINUS(!pos , !pos )) ;
<INITIAL>"*"       => (Tokens.TIMES(!pos , !pos )) ;
<INITIAL>"/"       => (Tokens.DIV(!pos , !pos )) ;
<INITIAL>"%"       => (Tokens.MOD(!pos , !pos )) ;


<INITIAL>"!"      => (Tokens.NOT(!pos , !pos )) ; 
<INITIAL>"&&"       => ( Tokens.AND(!pos , !pos )) ; 
<INITIAL>"||"       => (Tokens.OR(!pos , !pos )) ;

<INITIAL>"="       => (Tokens.EQ(!pos , !pos )) ;
<INITIAL>"<>"       => (Tokens.NEQ(!pos , !pos )) ;
<INITIAL>"<"       => (Tokens.LT(!pos , !pos )) ;
<INITIAL>"<="       => (Tokens.LEQ(!pos , !pos )) ;
<INITIAL>">="       => (Tokens.GEQ(!pos , !pos )) ;
<INITIAL>">"       => (Tokens.GT(!pos , !pos )) ;

<INITIAL>":="       => (Tokens.ASSIGN(!pos , !pos )) ;

<INITIAL>"("      => (Tokens.LPAREN(!pos , !pos ));
<INITIAL>")"      => (Tokens.RPAREN(!pos , !pos )) ; 
<INITIAL>"{"      => (Tokens.LBRACE(!pos , !pos )) ; 
<INITIAL>"}"      => (Tokens.RBRACE(!pos , !pos )) ; 

<INITIAL>";"       => (Tokens.SEMI(!pos , !pos )) ;
<INITIAL>","       => (Tokens.COMMA(!pos , !pos )) ;


<INITIAL>{alpha}{alphanum}* => (Tokens.VARIABLE(yytext,!pos,!pos));
<INITIAL>[~]?{digit}+ =>  (Tokens.INT (Bigint.fromString(yytext), !pos, !pos));
<INITIAL>[~]?{digit}*"."{digit}*"("{digit}+")" => (Tokens.RAT(Rational.fromDecimal(yytext), !pos, !pos));

<INITIAL>"(*" => (YYBEGIN COMMENT; lex());
<COMMENT>\n+		=> (pos := (!pos) + (String.size yytext); lex());
<COMMENT>[^()*\n]+	=> (lex());
<COMMENT>"(*"		=> (lex());
<COMMENT>"*)"		=> (YYBEGIN INITIAL; lex());
<COMMENT>[*()]	=> (lex());

.        => (error ("ignoring bad character "^yytext,!pos,!pos); lex());
