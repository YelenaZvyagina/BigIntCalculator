namespace bigIntCalculator
module bigIntFunctions =

    open System
    open myList

    type bInt =
        val digits : MyList<int>
        val isPos : Boolean
        new (lst, isneg) = {digits = lst; isPos = isneg}
        
    let reverse (ml : MyList<int>) =
        sysListToMyList ( List.rev (myListToSystemList ml) )
        
    let strToBigint (s : String) =
        let pos = not (s.[0] = '-') 
        if s.Length = 1 then
            let lst = [(int s.[0] - int '0')]
            let ml = sysListToMyList lst
            bInt (ml, true)
        else 
            let list = 
                [
                  for i = 1 to s.Length - 1 do 
                  int s.[i] - int '0'
                ]
            let ml = 
                if (s.[0] <> '+') && (s.[0] <> '-')
                then
                    let b = (First (int s.[0]- 48))
                    concat b (sysListToMyList list)
                else (sysListToMyList list)
            bInt (ml, pos)
        
    let bntToString (bnt : bInt) =
        let s = (myListToSystemList bnt.digits)
        let convertToString l = l |> List.map (fun i -> i.ToString()) |> String.concat ""
        if not bnt.isPos then "-" + (convertToString s)
        else convertToString s
        
    let removeZeros (ml : MyList<_>) =
        let rec go (dgts : MyList<int>) =
            match dgts with
            | First x -> First x
            | Cons(h, t) -> if h = 0 then go t else Cons (h, t)
        go ml
        
    let reverseSign (bnt : bInt) =
        if bnt.digits = First 0
        then bnt
        else
            let sign = not bnt.isPos
            bInt (bnt.digits, sign)
        
    let isOdd (bnt : bInt) =
        let rev = reverse bnt.digits
        match rev with
        | First x -> not (x % 2 = 0) 
        | Cons (h, t) -> not (h % 2 = 0) 
        
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

    let bntEqual (bnt1 : bInt) (bnt2 : bInt) =
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

    let sumBint (bnt1 : bInt) (bnt2 : bInt) =
        match bnt1.isPos, bnt2.isPos with
        | false, false -> bInt((sumMl bnt1.digits bnt2.digits), false)
        | true, true -> bInt((sumMl bnt1.digits bnt2.digits), true)
        | false, true | true, false ->
            if ml1Greater bnt1.digits bnt2.digits
            then bInt ((subMl bnt1.digits bnt2.digits), bnt1.isPos)
            else bInt ((subMl bnt2.digits bnt1.digits), bnt2.isPos)

    let subBint (bnt1 : bInt) (bnt2 : bInt) =
        match bnt1.isPos, bnt2.isPos with
        | false, false | true, true ->
            if ml1Greater bnt1.digits bnt2.digits
            then bInt((subMl bnt1.digits bnt2.digits), bnt1.isPos)
            else bInt ((subMl bnt2.digits bnt1.digits), not bnt1.isPos)
        | false, true | true, false -> bInt ((sumMl bnt1.digits bnt2.digits), bnt1.isPos)

    let multToNumMl (ml : MyList<_>) num =
        let rec go (ml : MyList<_>) num =
           match ml with
           | First x -> First (x * num)
           | Cons (h, t) -> Cons ((h * num), go t num)
        reverse (transferOdd (reverse (go ml num)))
      
    let multMl (ml1 : MyList<_>) (ml2 : MyList<_>) =
        let ml3, ml4 =
            if ml1Greater ml1 ml2 then (ml1, ml2) else (ml2, ml1)
        let rec go (ml1 : MyList<_>) (ml2 : MyList<_>) acc (sum : MyList<_>) =
            match ml1, ml2 with
            | ml1, First x ->
                removeZeros (sumMl sum (multToNumMl (multToNumMl ml1 x) (int (10.0**acc)) )) 
            | ml1, Cons (h1, t1) ->
                let temp = sumMl sum (multToNumMl (multToNumMl ml1 h1) (int (10.0**acc)) )
                go ml1 t1 (acc + 1.0) (removeZeros temp)
        (go ml3 (reverse ml4) 0.0 (First 0))
        
    let multBnt (bnt1 : bInt) (bnt2 : bInt) =
        match bnt1.isPos, bnt2.isPos with
        | true, true | false, false -> bInt( (multMl bnt1.digits bnt2.digits), true)
        | true, false | false, true -> bInt( (multMl bnt1.digits bnt2.digits), false)
        
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
                        let r = subMl (reverse (transferOdd (First h)))  ( (multMl small q))
                        match r with
                        | First x ->
                            if x = 0 then go t small (concat res q)
                            else
                                match t with
                                | First onet -> go (First (x*10 + onet)) small (concat res q)
                                | Cons (ht, tt) ->
                                    let l = Cons ((x*10 + ht), tt)
                                    go l small (concat res q)
                        | Cons (hr, tr) ->
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
                                     
    let divBnt (bnt1 : bInt) (bnt2 : bInt) =
        match bnt1.isPos, bnt2.isPos with
        | true, true | false, false -> bInt( fst (divRemMl bnt1.digits bnt2.digits), true)
        | true, false | false, true -> bInt( fst (divRemMl bnt1.digits bnt2.digits), false)
        
    let remBnt (bnt1 : bInt) (bnt2 : bInt) =
        match bnt1.isPos, bnt2.isPos with
        | true, true | false, false -> bInt( snd (divRemMl bnt1.digits bnt2.digits), true)
        | true, false | false, true -> bInt( snd (divRemMl bnt1.digits bnt2.digits), false)
       
    let absBnt (bnt : bInt) = bInt (bnt.digits, true)

    let toBinary (x : bInt) = 
        let rec go l r =
            match l with
            | First 0 -> r
            | _ ->
                let divd = divBnt (bInt(l, true)) (bInt(First 2, true))
                let rem = remBnt (bInt(l, true)) (bInt(First 2, true))
                go divd.digits (Cons(getHead rem.digits, r))
        let divd = divBnt (bInt (x.digits, true)) (bInt (First 2, true))
        let rem = remBnt (bInt (x.digits, true)) (bInt (First 2, true))
        bInt (go divd.digits (First (getHead rem.digits)), x.isPos)

    let toPower (bnt : bInt) (num : bInt) =
        if not num.isPos then failwith "the power should be positive"
        let rec go (ml : MyList<_>) (pow : MyList<_>) =
            match pow with
            | First 0 -> First 1
            | First 1 -> ml
            | _ ->
                let odd, rem = divRemMl pow (First 2)
                let temp = go ml odd
                let temp1 = multMl temp temp
                if rem = First 0 then temp1 else (multMl temp1 ml)
        match bnt.isPos with
        | true -> bInt ((go bnt.digits num.digits), true)
        | false -> if isOdd num then bInt ((go bnt.digits num.digits), true) else bInt ((go bnt.digits num.digits), false)

        
         
                