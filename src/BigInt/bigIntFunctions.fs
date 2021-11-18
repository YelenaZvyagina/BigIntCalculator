namespace BigIntCalculator
module BigIntFunctions =

    open System
    open MyList

    type BigInt =
        val digits : MyList<int>
        val isPos : Boolean
        new (lst, isneg) = {digits = lst; isPos = isneg}
        
    let ml1Greater (ml1 : MyList<_>) (ml2 : MyList<_>) =
        if length ml1 <> length ml2 then length ml1 > length ml2
        else
            let rec go (ml1 : MyList<_>) (ml2 : MyList<_>) =
                match ml1, ml2 with
                | First x, First y -> x > y
                | Cons (h1, t1), Cons (h2, t2) -> if h1 <> h2 then h1 > h2 else (go t1 t2)
                | _ -> failwith "Got first and cons at the same time, this case cannot be achieved"
            go ml1 ml2
            
    let isEqual (ml1 : MyList<_>) (ml2 : MyList<_>) =
        if length ml1 <> length ml2 then false
        else
            let rec go (ml1 : MyList<_>) (ml2 : MyList<_>) = 
                match ml1, ml2 with
                | First x, First y -> x = y
                | Cons (h1, t1), Cons (h2, t2) -> (h1 = h2) && (go t1 t2)
                | _ -> failwith "this case cannot be achieved"
            go ml1 ml2  
                
    let rec toFirst (lst : MyList<_>) =
        match lst with
        | First x -> x
        | Cons (h, t) -> 
            match t with
            | First ft -> toFirst (First (h*10 + ft))
            | Cons (ht, tt) ->
                toFirst (Cons( (h*10 + ht), tt)) 
        
    let removeZeros (ml : MyList<_>) =
        let rec go (dgts : MyList<int>) =
            match dgts with
            | First x -> First x
            | Cons(h, t) -> if h = 0 then go t else Cons (h, t)
        go ml
        
    let reverseSign (bnt : BigInt) =
        if bnt.digits = First 0
        then bnt
        else
            let sign = not bnt.isPos
            BigInt (bnt.digits, sign)
        
    let isOdd (bnt : BigInt) =
        let rev = reverse bnt.digits
        match rev with
        | First x -> x % 2 <> 0 
        | Cons (h, _) -> h % 2 <> 0 
        
    let zeroComplete (ml : MyList<_>) num toEnd =
        let temp = List.init num (fun i -> i*0)
        match toEnd with
        | true -> concat ml (sysListToMyList temp)
        | false -> concat (sysListToMyList temp) ml
        
    let becomeEqual (ml1 : MyList<_>) (ml2 : MyList<_>) =
        if length ml1 = length ml2 then (ml1, ml2)
        elif length ml1 > length ml2
        then
            ( ml1, (zeroComplete ml2 (length ml1 - length ml2) false ) )
        else
            ( ml2, (zeroComplete ml1 (length ml2 - length ml1) false) )

    let transferOdd (ml : MyList<_>) = 
        let rec go (ml : MyList<_>) acc =
           match ml with
           | First x ->
               if x + acc >= 10
               then
                   let a = if ((x + acc) / 10) >= 10 then go (First ((x + acc) / 10) ) 0 else First ((x + acc) / 10)
                   Cons ( ( (x + acc) % 10), a)
               else First (x + acc)
           | Cons (h, t) -> Cons ( ( (h + acc) % 10), ( go t ((acc + h) / 10) ))
        go ml 0

    let bntEqual (bnt1 : BigInt) (bnt2 : BigInt) =
        isEqual bnt1.digits bnt2.digits && bnt1.isPos = bnt2.isPos

    let sumMl (ml1 : MyList<_>) (ml2 : MyList<_>) = 
        let ml3, ml4 = becomeEqual ml1 ml2
        let rec go (ml3 : MyList<_>) (ml4 : MyList<_>) = 
            match ml3, ml4 with
            | First x, First y -> First (x+y)
            | Cons (h1, t1), Cons (h2, t2) -> Cons ( (h1 + h2), go t1 t2 )
            | _, _ -> failwith "This case cannot be achieved"
        reverse (transferOdd (go (reverse ml3) (reverse ml4)) )
     
    let subMl (ml1 : MyList<_>) (ml2 : MyList<_> ) =
        let ml3, ml4 = becomeEqual ml1 ml2
        let rec go (ml1 : MyList<_>) (ml2 : MyList<_>) acc =
            match ml1, ml2, acc with
            | First x, First y, acc -> First (x - y + acc)
            | Cons(h1, t1), Cons(h2, t2), acc ->
                if h1 - h2 + acc < 0
                then Cons ( (acc + h1 - h2 + 10), go t1 t2 (- 1) ) 
                else Cons ((h1 - h2 + acc), go t1 t2 0 )
            | _ -> failwith "this case cannot be achieved"
        removeZeros (reverse (go (reverse ml3) (reverse ml4) 0) )

    let sumBint (bnt1 : BigInt) (bnt2 : BigInt) =
        match bnt1.isPos, bnt2.isPos with
        | false, false -> BigInt((sumMl bnt1.digits bnt2.digits), false)
        | true, true -> BigInt((sumMl bnt1.digits bnt2.digits), true)
        | false, true | true, false ->
            let eq = isEqual bnt1.digits bnt2.digits
            if ml1Greater bnt1.digits bnt2.digits
            then BigInt ((subMl bnt1.digits bnt2.digits), eq || bnt1.isPos)
            else BigInt ((subMl bnt2.digits bnt1.digits), eq || bnt2.isPos)

    let subBint (bnt1 : BigInt) (bnt2 : BigInt) =
        match bnt1.isPos, bnt2.isPos with
        | false, false | true, true ->
            let eq = isEqual bnt1.digits bnt2.digits
            if ml1Greater bnt1.digits bnt2.digits
            then BigInt((subMl bnt1.digits bnt2.digits), eq || bnt1.isPos)
            else BigInt ((subMl bnt2.digits bnt1.digits), eq || not bnt1.isPos)
        | false, true | true, false -> BigInt ((sumMl bnt1.digits bnt2.digits), bnt1.isPos)

    let multToNumMl (ml : MyList<_>) num =
        let rec go (ml : MyList<_>) num =
           match ml with
           | First x -> First (x * num)
           | Cons (h, t) -> Cons ((h * num), go t num)
        reverse (transferOdd (reverse (go ml num)))
        
    let zeroConcat acc =
        let rec go acc resstr =
            match acc with
            | 0 -> resstr
            | _ ->
                let s1 = resstr + "0"
                go (acc - 1) s1
        (go acc "1")    
        
    let multMl (a: MyList<_>) (b: MyList<_>) =
        let rec go (a: MyList<_>) (b: MyList<_>) (sum: MyList<_>) acc = 
            match (a, b) with
            | a, First b ->
                let temp = zeroConcat acc
                let deg = strToMyList temp
                let res = 
                    if deg = First 1 then multToNumMl a b
                    else concat (multToNumMl a b) (slice deg 1 ((length deg) - 1))                
                sumMl res sum           
            | a, Cons(hd_b, tl_b) ->
                let temp = zeroConcat acc
                let deg = strToMyList temp
                let res = 
                    if deg = First 1 then multToNumMl a hd_b
                    else concat (multToNumMl a hd_b) (slice deg 1 ((length deg) - 1))
                go a tl_b (sumMl sum res) (acc + 1)   
        if  ml1Greater a b 
        then reverse (transferOdd (reverse (go a (reverse b) (First 0) 0)))
        else reverse (transferOdd (reverse (go b (reverse a) (First 0) 0)))
         
    let multBnt (bnt1 : BigInt) (bnt2 : BigInt) =
        match bnt1.isPos, bnt2.isPos with
        | true, true | false, false -> BigInt( (multMl bnt1.digits bnt2.digits), true)
        | true, false | false, true ->
            let sign = (multMl bnt1.digits bnt2.digits) = First 0 
            BigInt( (multMl bnt1.digits bnt2.digits), sign)
        
    let select (big : MyList<_>) (small : MyList<_>) =
        let rec go (big : MyList<_>) (small : MyList<_>) (acc : MyList<_>) = 
            if (ml1Greater big (multMl small acc)) then go big small (sumMl acc (First 1))
            elif (isEqual big (multMl small acc)) then go big small (sumMl acc (First 1))
            else subMl acc (First 1)
        go big small (First 1)

    let divRemMl (ml1 : MyList<_>) (ml2 : MyList<_>) = 
        if removeZeros ml2 = First 0 then failwith "dividing by 0"
        if isEqual ml1 ml2 then (First 1, First 0)
        elif not (ml1Greater ml1 ml2) then (First 0, ml1)
        else 
            let rec go (big : MyList<int>) (small : MyList<int>) (res : MyList<int>)  =
                match big with
                | First x ->
                    let q = (select (reverse (transferOdd (First x))) small )
                    let r = subMl (reverse (transferOdd (First x)))  (reverse (multMl small q))  
                    ((concat res q), r)  
                | Cons (h, t) ->
                    if ml1Greater (reverse (transferOdd (First h))) small
                    then
                        let q = (select (reverse (transferOdd (First h))) small )
                        let r = subMl (reverse (transferOdd (First h)))  (multMl small q)
                        match r with
                        | First x ->
                            if x = 0 then go t small (concat res q)
                            else
                                match t with
                                | First onet -> go (First (x*10 + onet)) small (concat res q)
                                | Cons (ht, tt) ->
                                    let l = Cons ((x*10 + ht), tt)
                                    go l small (concat res q)
                        | Cons _ ->
                             let fstR = toFirst r  
                             match t with
                                | First onet -> go (First (fstR*10 + onet) ) small (concat res q)
                                | Cons (ht, tt) -> go (Cons ((fstR*10 + ht ), tt)) small (concat res q)
                    elif isEqual (reverse (transferOdd (First h))) small
                    then go t small (concat res (First 1))
                    else
                        match t with
                        | First x -> go (First (h*10 + x)) small (concat res (First 0))
                        | Cons (ht, tt) -> go (Cons ((h*10 + ht), tt ) ) small (concat res (First 0))
            let d = fst (go ml1 ml2 (First 0) )
            let r = subMl ml1 (multMl d ml2)
            (removeZeros (getTail d), r)
                                     
    let divBnt (bnt1 : BigInt) (bnt2 : BigInt) =
        match bnt1.isPos, bnt2.isPos with
        | true, true | false, false -> BigInt( fst (divRemMl bnt1.digits bnt2.digits), true)
        | true, false | false, true ->
            let sign = fst (divRemMl bnt1.digits bnt2.digits) = First 0 
            BigInt( fst (divRemMl bnt1.digits bnt2.digits), sign)
        
    let remBnt (bnt1 : BigInt) (bnt2 : BigInt) =
        match bnt1.isPos, bnt2.isPos with
        | true, true -> BigInt( snd (divRemMl bnt1.digits bnt2.digits), true)
        | false, false ->
            if ml1Greater bnt2.digits bnt1.digits then bnt1
            elif isEqual bnt1.digits bnt2.digits then BigInt (First 0, true)
            else
                let sign = snd (divRemMl bnt1.digits bnt2.digits) = First 0 
                BigInt( snd (divRemMl bnt1.digits bnt2.digits), sign)
        | true, false ->
            BigInt( snd (divRemMl bnt1.digits bnt2.digits), true)
        | false, true ->
            if ml1Greater bnt2.digits bnt1.digits then
                sumBint bnt1 (BigInt ( (multMl (fst (divRemMl bnt1.digits bnt2.digits) ) bnt2.digits ), true))
            elif 
                (subMl bnt1.digits bnt2.digits = First 0) || ((snd (divRemMl bnt1.digits bnt2.digits)) = First 0)  then BigInt (First 0, true)
            else BigInt (snd (divRemMl bnt1.digits bnt2.digits), false)
                
    let absBnt (bnt : BigInt) = BigInt (bnt.digits, true)

    let toBinary (x : BigInt) = 
        let rec go l r =
            match l with
            | First 0 -> r
            | _ ->
                let divd = divBnt (BigInt(l, true)) (BigInt(First 2, true))
                let rem = remBnt (BigInt(l, true)) (BigInt(First 2, true))
                go divd.digits (Cons(getHead rem.digits, r))
        let divd = divBnt (BigInt (x.digits, true)) (BigInt (First 2, true))
        let rem = remBnt (BigInt (x.digits, true)) (BigInt (First 2, true))
        BigInt (go divd.digits (First (getHead rem.digits)), x.isPos)

    let toPower (n: BigInt) (pow: BigInt) =
        let rec go r (p: BigInt) =
            match p.digits with
            | First 0 -> BigInt(First 1, true)
            | First 1 -> r
            | _ ->
                let rm, div = (remBnt p (BigInt ( First 2, true) )), (divBnt p (BigInt ( First 2, true)) )
                let nr = go r div
                if rm.digits = (First 0) then multBnt nr nr else multBnt n (multBnt nr nr) 
        if not pow.isPos then failwith "Positive power expected"
        else go n pow
           
    let strToBigint (s : String) =
        let pos = s.[0] <> '-' 
        if s.Length = 1 then
            let lst = [(int s.[0] - int '0')]
            let ml = sysListToMyList lst
            BigInt (ml, true)
        else 
            let list = 
                [
                  for i = 1 to s.Length - 1 do 
                  int s.[i] - int '0'
                ]
            let ml = 
                if (s.[0] <> '+') && (s.[0] <> '-')
                then
                    let b = (First (int s.[0] - int '0'))
                    concat b (sysListToMyList list)
                else (sysListToMyList list)
            BigInt (ml, pos)
        
    let bntToString (bnt : BigInt) =
        let s = (myListToSystemList bnt.digits)
        let convertToString l = l |> List.map (fun i -> i.ToString()) |> String.concat ""
        if not bnt.isPos then "-" + (convertToString s)
        else convertToString s               