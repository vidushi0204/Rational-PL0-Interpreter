signature BIGINT =
    sig
        type bigint
        exception big_error
        val geq: bigint * bigint -> bool
        val remove_zero: bigint -> bigint
        val absolute: bigint -> bigint
        val add_int: bigint * bigint -> bigint
        val sub_int: bigint * bigint -> bigint
        val mult_int: bigint * bigint -> bigint
        val div_int: bigint * bigint -> bigint * bigint
        val abs_gcd:bigint * bigint -> bigint 
        val equal_int: bigint * bigint -> bool
        val nine_rep: bigint -> bigint
        val is_neg : bigint -> bool
        val neg_int : bigint -> bigint
        val toString : bigint -> string
        val fromString : string -> bigint
        val max_ten : bigint ->  bigint
    end;

structure Bigint : BIGINT =
    struct
    type bigint = string
    exception big_error
        fun digit_mult(a: int,l: bigint,carry: int)=
            if a=0
            then
                "0"
            else if size(l)=0
            then 
                if carry=0 then "" else Int.toString(carry)
            else
                let
                    val toad=carry+a*(ord(String.sub(l,size(l)-1))-48)
                in
                    digit_mult(a,substring(l,0,size(l)-1),toad div 10)^Int.toString(toad mod 10)
                end;
        fun count_zero(l:bigint,a:int)=
            if size(l)=a+1 orelse String.sub(l,a)<> #"0"
            then
                a
            else
                count_zero(l,a+1);
        fun remove_zero(a)=
            let
                val count=count_zero(a,0)
            in
                substring(a,count,size(a)-count)
            end;
        fun geq(num1,num2)=
            if size(num1)>size(num2) 
            then
                true
            else if size(num1)<size(num2)
            then
                false
            else
                if String.sub(num1,0)>String.sub(num2,0)
                then
                    true
                else if String.sub(num1,0)<String.sub(num2,0)
                then
                    false
                else
                    if size(num1)=1
                    then
                        true
                    else
                        geq(substring(num1,1,size(num1)-1),substring(num2,1,size(num2)-1));
        
        fun digifind(l:bigint,x:int,bigl:bigint)=
            if x=0 orelse geq(bigl,digit_mult(x,l,0)) then x else digifind(l,x-1,bigl);
        fun mod_add (num1:bigint,num2:bigint)=
            let 
                val len1 = size(num1)
                val len2 = size(num2)
                val max_len = if len1>len2 then len1 else len2         

                fun add_zero_to_left(num : bigint, len : int) =
                    if size(num) = len then num
                    else add_zero_to_left("0" ^ num, len)  

                fun add_digit(dig1 : int, dig2 : int, carry:int) =
                    ((dig1+dig2+carry) mod 10,(dig1+dig2+carry) div 10)
                
                fun add_help(a : bigint, b : bigint, carry : int,sum : bigint) =
                    if a="" andalso b=""
                    then 
                        if carry=1 then "1"^sum else sum
                    else
                        let
                            val tup = add_digit(ord(String.sub(a,size(a)-1))-48,ord(String.sub(b,size(b)-1))-48,carry)
                        in
                            add_help(substring(a, 0, size(a)-1),substring(b,0,size(b)-1),#2 tup,Int.toString(#1 tup)^sum)
                        end
            in
                add_help(add_zero_to_left(num1,max_len),add_zero_to_left(num2,max_len),0,"")         
            end;
        fun mod_sub(num1:bigint,num2:bigint)=
            let 
                val len1 = size(num1)
                fun add_zero_to_left(num : bigint, len : int) =
                    if size(num) = len then num
                    else add_zero_to_left("0" ^ num, len)  

                fun sub_digit(dig1 : int, dig2 : int, carry:int) =
                    let
                        val diff=dig1-dig2-carry
                        val car=if diff<0 then 1 else 0
                    in
                        ((diff+10) mod 10,car)
                    end
                fun sub_help(a : bigint, b : bigint, carry : int,diff : bigint) =
                    if a="" andalso b=""
                    then 
                        diff
                    else
                        let
                            val tup = sub_digit(ord(String.sub(a,size(a)-1))-48,ord(String.sub(b,size(b)-1))-48,carry)
                        in
                            sub_help(substring(a, 0, size(a)-1),substring(b,0,size(b)-1),#2 tup,Int.toString(#1 tup)^diff)
                        end 

                val p= sub_help(num1,add_zero_to_left(num2,len1),0,"") 
                val a=count_zero(p,0)
            in
                substring(p,a,size(p)-a)
            end;

        fun absolute(a)=
            if String.sub(a,0)= #"~" orelse String.sub(a,0)= #"+"
            then substring(a,1,size(a)-1) else a;

        fun add_int(num1,num2)=
            let
                val a=absolute(num1)
                val b=absolute(num2)
            in
                if String.sub(num1,0)= #"~" 
                then
                    if String.sub(num2,0)= #"~"
                    then
                        "~"^mod_add(a,b)
                    else
                        if geq(b,a) then mod_sub(b,a) else "~"^mod_sub(a,b)
                else if String.sub(num2,0)= #"~"
                then
                    if geq(a,b) then mod_sub(a,b) else "~"^mod_sub(b,a)
                else
                    mod_add(a,b)
            end;   
        fun sub_int(num1,num2)=
            if String.sub(num2,0)= #"~" 
            then
                add_int(num1,absolute(num2))
            else 
                add_int(num1,"~"^absolute(num2));
        fun mult_int(num1,num2)=
            let
                fun ans_pos(a:bigint,b:bigint)=
                    if String.sub(a,0)= #"~" 
                    then
                        if String.sub(b,0)= #"~" then "" else "~"
                    else if String.sub(b,0)= #"~" then "~" else ""
                fun mult_help(a:bigint,b:bigint)=
                    if size(b)=1
                    then
                        digit_mult(ord(String.sub(b,0))-48,a,0)
                    else
                        mod_add(mult_help(a,substring(b,0,size(b)-1))^"0",digit_mult(ord(String.sub(b,size(b)-1))-48,a,0))
            in  
                if absolute(num1)="0" orelse absolute(num2)="0" then "0" else (ans_pos(num1,num2))^mult_help(absolute(num1),absolute(num2))
            end;
        
        fun div_help(prefix:bigint,num:bigint, divisor:bigint, quo:bigint)=
            let
                val dig=digifind(divisor,9,prefix)
            in  
                if size(num) = 1
                then 
                    div_help(remove_zero(mod_sub(prefix,digit_mult(dig,divisor,0))^substring(num,0,1)),"",divisor,quo^Int.toString(dig))
                else if size(num)>1
                then
                    div_help(remove_zero(mod_sub(prefix,digit_mult(dig,divisor,0))^substring(num,0,1)),substring(num,1,size(num)-1),divisor,quo^Int.toString(dig))
                else
                    (remove_zero(quo^Int.toString(dig)),mod_sub(prefix,digit_mult(dig,divisor,0)))
            end;
            
        fun div_int(num1,num2)=
            let
                val a=absolute(num1)
                val b=absolute(num2)
                val anew=if size(a) = 1 then "" else substring(a,1,size(a)-1)
                val tup=div_help(substring(a,0,1),anew,b,"0")
            in
                if String.sub(num1,0)= #"~" 
                then
                    if String.sub(num2,0)= #"~"
                    then
                        (#1 tup,"~"^(#2 tup))
                    else
                        ("~"^(#1 tup),"~"^(#2 tup))
                else if String.sub(num2,0)= #"~"
                then
                    ("~"^(#1 tup),#2 tup)
                else
                    tup
            end;
        
        fun abs_gcd(num1,num2)=
            if num1 = "0" then num2
            else if num2 = "0" then num1
            else if geq(num1,num2) then abs_gcd(num2,#2(div_int(num1,num2)))
            else abs_gcd(num1,#2(div_int(num2,num1)));
        fun equal_int(num1,num2)=
            if absolute(sub_int(num1,num2)) = "0"
            then true else false      
        fun helplen(a:bigint,b:bigint)=
            if #2(div_int(a,b))="0" then a else helplen(a^"9",b);
        fun nine_rep(b)=
            helplen("9",b);
        fun is_neg(str)= if String.sub(str,0) = #"~" then true else false;
        fun neg_int(str)= mult_int(str,"~1");
        fun toString(num)= num;
        fun fromString(num)= num;
        fun max_ten(denom)=
            let
                val numb=abs_gcd("10",denom)
            in
                if numb = "1" then denom else max_ten(#1(div_int(denom,numb)))
            end;

    end; 
functor Rational_num (Bigint: BIGINT) :
    sig
        type bigint = Bigint.bigint
        type rational = bigint * bigint
        exception rat_error
        exception ZERO_ERROR
        val make_rat: bigint * bigint -> rational option
        val rat: bigint -> rational option
        val reci: bigint -> rational option
        val neg: rational -> rational
        val inverse : rational -> rational option
        val equal : rational * rational -> bool 
        val less : rational * rational -> bool 
        val add : rational * rational -> rational
        val subtract : rational * rational -> rational
        val multiply : rational * rational -> rational
        val divide : rational * rational -> rational option
        val showRat : rational -> string
        val showDecimal : rational -> string
        val fromDecimal : string -> rational
        val toDecimal : rational -> string
    end
    =struct
        open Bigint
        type bigint = Bigint.bigint
        type rational = bigint * bigint
        exception rat_error
        exception ZERO_ERROR
        fun make_help(nom:bigint,denom:bigint)=
            let 
                val abs_nom=remove_zero(absolute(nom))
                val abs_denom=remove_zero(absolute(denom))
                val gcd_nums=abs_gcd(abs_nom,abs_denom)
                val tup=(#1(div_int(abs_nom,gcd_nums)),#1(div_int(abs_denom,gcd_nums)))
            in  

                if is_neg(nom)
                then
                    if is_neg(denom)
                    then
                        tup
                    else
                        (neg_int(#1 tup),#2 tup)
                else if is_neg(denom)
                then
                    (neg_int(#1 tup),#2 tup)
                else 
                    tup               
            end;

        fun make_rat(nom,denom)=
            if toString(denom)="0" then raise ZERO_ERROR else
            SOME (make_help(nom,denom));
            
        fun rat(num)=
            make_rat(num,fromString("1"));
        fun reci(num) = 
            if toString(num)="0" then raise ZERO_ERROR else
            make_rat(fromString("1"),num);
        fun neg(nom,denom)=
            if is_neg(nom)
            then
                (absolute(nom),denom)
            else
                (neg_int(nom),denom);
        fun inverse(nom,denom)=
            if toString(nom)="0" then raise ZERO_ERROR else
            make_rat(denom,nom);
        fun equal((nom1,denom1),(nom2,denom2))=
            if equal_int(mult_int(nom1,denom2),mult_int(nom2,denom1)) then true else false;
        fun less((nom1,denom1),(nom2,denom2))=
            if is_neg(nom1) 
            then if is_neg(nom2) then less((absolute(nom2),denom2),(absolute(nom1),denom1)) else true
            else if is_neg(nom2) then true
            else
                if geq(mult_int(nom1,denom2),mult_int(denom1,nom2)) 
                then false else true
        fun add((nom1,denom1),(nom2,denom2))=
            let 
                val r1= make_rat(nom1,denom1)
                val r2= make_rat(nom2,denom2)
            in
                make_help(Bigint.add_int(Bigint.mult_int(nom1,denom2),Bigint.mult_int(nom2,denom1)),Bigint.mult_int(denom1,denom2))
            end;
        fun subtract(r1,r2)=
            add(r1,neg(r2));
        fun multiply((nom1,denom1),(nom2,denom2))=
            make_help(mult_int(nom1,nom2),mult_int(denom1,denom2));
        fun divide((nom1,denom1),(nom2,denom2))=
            if toString(nom2)="0" then raise ZERO_ERROR else
            SOME (multiply((nom1,denom1),(denom2,nom2)));
        fun showRat(nom,denom)=
            if toString(denom)="0" then raise ZERO_ERROR else
            toString(nom)^"/"^toString(denom);
        fun decbreak(predec:string,remstr:string)=
            if substring(remstr,0,1)= "."
            then (predec,substring(remstr,1,size(remstr)-1))
            else decbreak(predec^substring(remstr,0,1),substring(remstr,1,size(remstr)-1));
        fun recurbreak(postdec:string,remstr:string)=
            if substring(remstr,0,1)= "("
            then (postdec,substring(remstr,1,size(remstr)-2))
            else recurbreak(postdec^substring(remstr,0,1),substring(remstr,1,size(remstr)-1));
        fun str_rep (num: int,str:string)=
            if num= 0 then ""
            else str_rep(num-1,str)^str;
        
        fun fromDecimal(str)=  
            let
              val tup1= decbreak("",str)
              val beforedec = #1 tup1
              val tup2 = recurbreak("",#2 tup1)
              val afterdec = #1 tup2
              val recur = #2 tup2
              val num2 = absolute(fromString(beforedec^afterdec))
              val num1 = absolute(fromString(beforedec^afterdec^recur))
              val denom =mult_int(fromString("1"^str_rep(size(afterdec),"0")),fromString(str_rep(size(recur),"9")))
            in
                if is_neg(fromString(beforedec)) then make_help(neg(sub_int(num1,num2),denom))  else   make_help(sub_int(num1,num2),denom)
            end;
        fun search(remlist: bigint list,key: bigint)=
            if length(remlist) = 0
            then false
            else if equal_int(hd(remlist),key)
            then true
            else search(tl(remlist),key);
        fun fml(remlist: bigint list,quo: bigint, rem: bigint,denom: bigint)=
            let
              val tup = div_int(mult_int(rem,fromString("10")),denom)
              val s = search(remlist,#2 tup) 
            in
                if s then add_int(mult_int(quo,fromString("10")),#1 tup)
                else fml(remlist@[#2 tup],add_int(mult_int(quo,fromString("10")),#1 tup),#2 tup,denom)
            end
        fun kuch_toh(nom:bigint,denom:bigint)=
            let
                val hai = mult_int(nom,fromString("10"))
            in
                if geq(hai,denom) then ""
                else "0"^kuch_toh(hai,denom)
            end ;
        fun toDecimal(nom,denom)=
            let 
                val tup = make_help(absolute(nom),absolute(denom))
                val mix_frac = div_int(tup)

                val maxten=max_ten(#2 tup)
                val recurlen =if equal_int(#2(mix_frac),fromString("0")) then 1 else size(toString(nine_rep(maxten)))
                val postdec = if toString(#2(mix_frac))= "0" then "0" else kuch_toh(#2 mix_frac, #2 tup)^toString(fml([#2 mix_frac],fromString("0"),#2 mix_frac,#2 tup))
                val nonrep = if size(postdec) = recurlen then "" else substring(postdec,0,size(postdec)-recurlen)
                val signa = if is_neg(nom) then "~" else "" 
            in  
                signa^toString(#1 mix_frac)^"."^nonrep^"("^substring(postdec,size(postdec)-recurlen,recurlen)^")"
            end;
        fun showDecimal(r1)=
            toDecimal(r1)
    end;
structure Rational = Rational_num(Bigint);
