structure CalcLrVals = CalcLrValsFun(
structure Token = LrParser.Token);
structure CalcLex = CalcLexFun(
structure Tokens = CalcLrVals.Tokens);
structure CalcParser = Join(
structure ParserData = CalcLrVals.ParserData
structure Lex=CalcLex
structure LrParser=LrParser);
   
    structure Calc :
    sig val compile : string -> DataTypes.AST
    end =
        struct
        exception CalcError;
        fun compile (fileName) =
        let
            val inStream = TextIO.openIn fileName;
            val grab : int -> string = fn
            n => if TextIO.endOfStream inStream
                then ""
                else TextIO.inputN (inStream,n);
            val printError : string * int * int -> unit = fn
            (msg,line,col) =>
            print (fileName^"["^Int.toString line^":"
            ^Int.toString col^"] "^msg^"\n");
            val (tree,rem) = CalcParser.parse
            (15,
            (CalcParser.makeLexer grab ),
            printError,
            ()) 
            handle CalcParser.ParseError => raise CalcError ; 
            val _ = TextIO.closeIn inStream;

        in 
            tree 
        end
    end;
fun interpret(infile:string,outfile:string)=
let
    fun WriteInFile(smth,str)= TextIO.output(smth,str)
    val os = TextIO.openOut(outfile^".txt");
    val strt = Calc.compile(infile^".rat");

    exception VARIABLE_REDCLARATION_ERROR of string ; 
    exception TYPE_MISMATCH_ERROR ; 
    exception PROCEDURE_REDCLARATION_ERROR of string ;
    exception VARIABLE_UNDECLARED_ERROR of string;
    exception PROCEDURE_UNDECLARED_ERROR of string;
    exception INVALID_INPUT_ERROR;


    val ProcTable : (string, DataTypes.BLK) HashTable.hash_table =
        HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") ; 
    fun insertInProc( id : string , value : DataTypes.BLK) = 
        if isSome (HashTable.find ProcTable (id)) then raise PROCEDURE_REDCLARATION_ERROR(id)
        else (HashTable.insert ProcTable ( id , value ));

    val typeTable : (string, int) HashTable.hash_table =
        HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") ; 
        fun insertInTable( idList : string list , idType : int) = 
            if ( null idList ) then () else ( if isSome (HashTable.find typeTable (hd idList)) then raise VARIABLE_REDCLARATION_ERROR( hd idList) else 
                    HashTable.insert typeTable ( hd idList , idType ) ; insertInTable( tl idList , idType) 
                    ) ; 
        fun getType ( id : string ) = 
            if( isSome (HashTable.find typeTable  id)) 
            then HashTable.lookup typeTable id 
            else raise VARIABLE_UNDECLARED_ERROR(id) ; 

    val IntTable : (string, Bigint.bigint) HashTable.hash_table =
        HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") ; 
        fun insertInTableI( idList : string list) = 
            if ( null idList ) then () else ( if isSome (HashTable.find IntTable (hd idList)) 
            then raise VARIABLE_REDCLARATION_ERROR( hd idList)
            else (HashTable.insert IntTable( hd idList , Bigint.fromString("0") ); insertInTableI( tl idList) )) ; 
        fun updateValueI(id:string, value: Bigint.bigint) =
            if isSome(HashTable.find IntTable (id)) 
            then 
                (HashTable.remove IntTable(id);  HashTable.insert IntTable(id ,value))
            else raise VARIABLE_UNDECLARED_ERROR(id);

    val RatTable : (string, Rational.rational) HashTable.hash_table =
        HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") ; 
        fun insertInTableR( idList : string list) = 
            if ( null idList ) then () else ( if isSome (HashTable.find RatTable (hd idList)) 
            then raise VARIABLE_REDCLARATION_ERROR( hd idList) 
            else (HashTable.insert RatTable( hd idList , Rational.fromDecimal("0.(0)")) ;insertInTableR( tl idList) ));
        fun updateValueR(id:string, value: Rational.rational) =
            if isSome(HashTable.find RatTable (id)) 
            then 
                (HashTable.remove RatTable(id); HashTable.insert RatTable(id,value)) 
            else raise VARIABLE_UNDECLARED_ERROR(id);

    val BoolTable : (string, bool) HashTable.hash_table =
        HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") ; 
        fun insertInTableB( idList : string list) = 
            if ( null idList ) then () else ( if isSome (HashTable.find BoolTable (hd idList)) 
            then raise VARIABLE_REDCLARATION_ERROR( hd idList) 
            else (HashTable.insert BoolTable( hd idList , true ); insertInTableB( tl idList)) ) ; 
        fun updateValueB(id:string, value: bool) =
            if isSome(HashTable.find BoolTable (id)) 
            then 
                (HashTable.remove BoolTable(id); HashTable.insert BoolTable(id,value))
            else raise VARIABLE_UNDECLARED_ERROR(id);
    fun notExp(str)=
        if str=true then false else true;
    fun TypeExp(exp)=
        case exp of DataTypes.PLUS(exp1,exp2) => 1
        | DataTypes.MINUS(exp1,exp2) => 1
        | DataTypes.TIMES(exp1,exp2) => 1
        | DataTypes.DIV(exp1,exp2) => 1
        | DataTypes.MOD(exp1,exp2) => 1
        | DataTypes.TT => 0
        | DataTypes.FF => 0
        | DataTypes.EQ(exp1,exp2) => 0
        | DataTypes.NEQ(exp1,exp2) => 0
        | DataTypes.LT(exp1,exp2) => 0
        | DataTypes.LEQ(exp1,exp2) => 0
        | DataTypes.GT(exp1,exp2) => 0
        | DataTypes.GEQ(exp1,exp2) => 0
        | DataTypes.VAR(exp1) => getType(exp1)
        | DataTypes.INTI(exp1) => 1
        | DataTypes.RATI(exp1) => 2
        | DataTypes.NOT(exp1) => 0
        | DataTypes.AND(exp1,exp2) => 0
        | DataTypes.OR(exp1,exp2) => 0
        | DataTypes.RPLUS(exp1,exp2) => 2
        | DataTypes.RMINUS(exp1,exp2) => 2
        | DataTypes.RTIMES(exp1,exp2) => 2
        | DataTypes.RDIV(exp1,exp2) => 2
        | DataTypes.INVERSE(exp1) => 2
        | DataTypes.NEG(exp1) => TypeExp(exp1)
        | DataTypes.MAKERAT(exp1,exp2) => 2
        | DataTypes.RAAT(exp1) => 2
        

    and intcomp(exp)=
        case exp of DataTypes.PLUS(exp1,exp2)=> Bigint.add_int(intcomp(exp1),intcomp(exp2))
        | DataTypes.MINUS(exp1,exp2)=> Bigint.sub_int(intcomp(exp1),intcomp(exp2))
        | DataTypes.TIMES(exp1,exp2)=> Bigint.mult_int(intcomp(exp1),intcomp(exp2))
        | DataTypes.DIV(exp1,exp2)=> #1(Bigint.div_int(intcomp(exp1),intcomp(exp2)))
        | DataTypes.MOD(exp1,exp2)=> #2(Bigint.div_int(intcomp(exp1),intcomp(exp2)))
        | DataTypes.NEG(exp1) => Bigint.neg_int(intcomp(exp1))
        | DataTypes.INTI(exp1)=> exp1
        | DataTypes.VAR(exp1) => (HashTable.lookup IntTable exp1)
        | _ => intcomp(exp)

        
    and ratcomp(exp)=
        case exp of DataTypes.RPLUS(exp1,exp2)=>  Rational.add(ratcomp(exp1),ratcomp(exp2)) 
        | DataTypes.RMINUS(exp1,exp2)=>  Rational.subtract(ratcomp(exp1),ratcomp(exp2))
        | DataTypes.RTIMES(exp1,exp2)=>  Rational.multiply(ratcomp(exp1),ratcomp(exp2))
        | DataTypes.RDIV(exp1,exp2)=>  valOf(Rational.divide(ratcomp(exp1),ratcomp(exp2)))
        | DataTypes.INVERSE(exp1)=>  valOf(Rational.inverse(ratcomp(exp1)))
        | DataTypes.NEG(exp1) =>  Rational.neg(ratcomp(exp1))
        | DataTypes.RATI(exp1)=>  exp1
        | DataTypes.VAR(exp1) => (HashTable.lookup RatTable exp1)
        | DataTypes.FROMDEC(str) => Rational.fromDecimal(str)
        | DataTypes.RAAT(exp) => valOf(Rational.rat(intcomp(exp)))
        | DataTypes.MAKERAT(exp1,exp2) => valOf(Rational.make_rat(intcomp(exp1),intcomp(exp2)))
        | _ => ratcomp(exp)

    and boolcomp(exp)=
        case exp of DataTypes.NOT(exp) => (if boolcomp(exp)=true then false else true)
        | DataTypes.AND(exp1,exp2)=>  (if boolcomp(exp1)=true then boolcomp(exp2) else false)
        | DataTypes.OR(exp1,exp2)=>  (if boolcomp(exp1)=false then boolcomp(exp2) else true)
        | DataTypes.TT => true
        | DataTypes.FF => false
        | DataTypes.VAR(exp1) => (HashTable.lookup BoolTable exp1)

        | DataTypes.EQ(exp1,exp2)=> (if TypeExp(exp1)=1 then Bigint.equal_int(intcomp(exp1),intcomp(exp2)) else Rational.equal(ratcomp(exp1),ratcomp(exp2)))
        | DataTypes.NEQ(exp1,exp2)=> notExp(if TypeExp(exp1)=1 then Bigint.equal_int(intcomp(exp1),intcomp(exp2)) else Rational.equal(ratcomp(exp1),ratcomp(exp2)))
        | DataTypes.LT(exp1,exp2)=> (if TypeExp(exp1)=1 then notExp(Bigint.geq(intcomp(exp1),intcomp(exp2))) else Rational.less(ratcomp(exp1),ratcomp(exp2)))
        | DataTypes.LEQ(exp1,exp2)=> (if TypeExp(exp1)=1 then Bigint.geq(intcomp(exp2),intcomp(exp1)) else notExp(Rational.less(ratcomp(exp2),ratcomp(exp1))))
        | DataTypes.GT(exp1,exp2)=> not(if TypeExp(exp1)=1 then Bigint.geq(intcomp(exp2),intcomp(exp1)) else notExp(Rational.less(ratcomp(exp2),ratcomp(exp1))))
        | DataTypes.GEQ(exp1,exp2)=> not(if TypeExp(exp1)=1 then notExp(Bigint.geq(intcomp(exp1),intcomp(exp2))) else Rational.less(ratcomp(exp1),ratcomp(exp2)))
        | _ => boolcomp(exp)

    and getexp(exp)=
        case TypeExp(exp) of 0 => let val next1=boolcomp(exp) in (if next1=true then "tt" else "ff") end
        | 1 => let val next1=intcomp(exp) in Bigint.toString(next1) end
        | 2 => let val next1=ratcomp(exp) in Rational.toDecimal(next1) end
        

    and getbooldecls(booldecls)=
        let
            val DataTypes.BOOLDECLS(vars,typ)=booldecls
            val next1=insertInTable(vars,0)
            val next2=insertInTableB(vars)
        in
            ()
        end
    and getintdecls(intdecls)=
        let
            val DataTypes.INTDECLS(vars,typ)=intdecls
            val next1=insertInTable(vars,1)
            val next2=insertInTableI(vars)
        in
            ()  
        end
    and getratdecls(ratdecls)=
        let
            val DataTypes.RATDECLS(vars,typ)=ratdecls
            val next1=insertInTable(vars,2)
            val next2=insertInTableR(vars)
        in
            ()  
        end

    and getprocdef(procdeflist)=
        if null(procdeflist)
        then ()
        else
            let
                val  DataTypes.PROCDEF(str,blk)=hd(procdeflist)
                val store  = insertInProc(str,blk)    
                val next1 = getprocdef(tl procdeflist)        
            in
                ()
            end
        
    and getprocdecls(procdecls)=
        let
            val  DataTypes.PROCDECLS(procdeflist)=procdecls
            val next1 = getprocdef(procdeflist)
        in
            ()
        end

    and getvardecls(vardecls)=
        let
            val  DataTypes.VARDECLS(ratdecls,intdecls,booldecls)=vardecls
            val next1 = getratdecls(ratdecls)
            val next2 = getintdecls(intdecls)
            val next3 = getbooldecls(booldecls)
        in
            ()
        end

    and getcomseq(command)=
        if null(command)
        then ()
        else
            case hd(command) of DataTypes.Assign(str,exp)=>
                let val typ= TypeExp(exp)
                    val idk = if typ=getType(str) 
                    then (if typ = 0 then updateValueB(str,boolcomp(exp)) else if typ =1 then updateValueI(str,intcomp(exp)) else updateValueR(str,ratcomp(exp))) else raise TYPE_MISMATCH_ERROR    
                    val next2=getcomseq(tl command) 
                in 
                    ()
                end
            | DataTypes.While(exp,comseq) =>
                let val next1=getexp(exp)
                    val cond= if next1="tt" then (getcomseq(comseq);getcomseq(command)) else getcomseq(tl command)
                in 
                    ()
                end
            | DataTypes.ITE( exp,x,y) =>
                let val next1=getexp(exp)
                    val typ = TypeExp(exp)
                    val idk = (if typ=0 then (if next1="tt" then getcomseq(x) else getcomseq(y)) else raise TYPE_MISMATCH_ERROR)
                    val next2=getcomseq(tl command)
                in 
                    ()
                end
            | DataTypes.Call(str) =>
                let 
                    val next1 = (if isSome (HashTable.find ProcTable (str)) then getblk(HashTable.lookup ProcTable str) else raise PROCEDURE_UNDECLARED_ERROR(str))
                in getcomseq(tl command) end

            | DataTypes.Read(str) =>
                let
                    val msg=print("Enter value of "^str^" and press ENTER \n")
                    val stri= valOf(TextIO.inputLine TextIO.stdIn)
                    val input = String.substring( stri,0, (String.size stri)-1)
                    val typ= getType(str)  
                    val idk = if typ = 0 then (updateValueB(str,if input="tt" then true else if input="ff" then false else raise INVALID_INPUT_ERROR))
                    else if typ = 1 then updateValueI(str,Bigint.fromString(input))
                    else updateValueR(str,Rational.fromDecimal(input))
                    val next2=getcomseq(tl command)   
                in
                    ()
                end
            | DataTypes.Print( exp) =>
                let val car = getexp(exp)
                    val idk = WriteInFile (os,car^"\n")
                    val next2=getcomseq(tl command)
                in ()
                end

    and getdec(dec)=
        let 
            val DataTypes.DEC(vd,pd)=dec
            val next1 = getvardecls(vd)
            val next2 = getprocdecls(pd)
        in ()
        end

    and getblk(blk)=
        let 
            val DataTypes.BLK(dec ,cmd)=blk
            val next1=getdec(dec)
            val next2=getcomseq(cmd)
        in ()
        end

    and getstart(start)=
        let 
            val DataTypes.PROG(blk)=start
            val next=getblk(blk)
        in ()
        end
in 
    getstart(strt);
    TextIO.closeOut(os)
end;

let 
    val msg=print("Enter name of input file and press ENTER \n")
    val stri= valOf(TextIO.inputLine TextIO.stdIn)
    val input = String.substring( stri,0, (String.size stri)-1)

    val msg2=print("Enter name of output file and press ENTER \n")
    val stri2= valOf(TextIO.inputLine TextIO.stdIn)
    val input2 = String.substring( stri2,0, (String.size stri2)-1)
in
    interpret(input,input2)
end;
