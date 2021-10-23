namespace bigIntCalculator

module myList =

    type MyList<'t> =
        | First of 't
        | Cons of 't * MyList<'t>

    let rec fold f acc l =
        match l with
        | First x -> f acc x
        | Cons(h, t) -> fold f (f acc h) t

    let length l =
        fold (fun acc _ -> acc + 1) 0 l

    let rec iter l f =
       match l with
       | First x -> f x
       | Cons(h, t) ->
            f h
            iter t f

    let rec sysListToMyList l =
        match l with
        | [] -> failwith "list shouldn't be empty"
        | [x] -> First x
        | h :: t -> Cons(h, sysListToMyList t)

    let rec myListToSystemList l =
        match l with
        | First x -> [x]
        | Cons(h, t) -> h :: myListToSystemList t

    let rec concat l1 l2 =
        match l1 with
        | First x -> Cons(x, l2)
        | Cons(h, t) -> Cons(h, concat t l2)

    let rec map f l =
        match l with
        | First x -> First(f x)
        | Cons(h, t) -> Cons(f h, map f t)

    let sort l =
        let rec _go lst =
            match lst with
            | First x -> First x
            | Cons(h, Cons(h1, t)) ->
                if h >= h1
                then Cons(h1, _go (Cons(h, t)))
                else Cons(h, _go (Cons(h1, t)))
            | Cons(h, First t) ->
                if h >= t
                then Cons(t, First h)
                else lst
        let rec go (ls:MyList<_>) n =
            if n <> length ls
            then go (_go ls) (n + 1)
            else ls
        go l 0

    let isEqual (ml1 : MyList<_>) (ml2 : MyList<_>) =
        if length ml1 <> length ml2 then false
        else
            let rec go (ml1 : MyList<_>) (ml2 : MyList<_>) = 
                match ml1, ml2 with
                | First x, First y -> x = y
                | Cons (h1, t1), Cons (h2, t2) -> (h1 = h2) && (go t1 t2)
                | _ -> failwith "this case cannot be achieved"
            go ml1 ml2
            
    let ml1Greater (ml1 : MyList<_>) (ml2 : MyList<_>) =
        if length ml1 <> length ml2 then length ml1 > length ml2
        else
            let rec go (ml1 : MyList<_>) (ml2 : MyList<_>) =
                match ml1, ml2 with
                | First x, First y -> x > y
                | Cons (h1, t1), Cons (h2, t2) -> if h1 <> h2 then h1 > h2 else (go t1 t2)
                | _ -> failwith "Got first and cons at the same time, this case cannot be achieved"
            go ml1 ml2   
            
    let getHead (ml : MyList<_>) =
        match ml with
        | First x ->  x
        | Cons (h, t) ->  h
        
    let getTail (ml : MyList<_>) =
        match ml with
        | First x -> First x
        | Cons(h, t) -> t
        
    let rec toFirst (lst : MyList<_>) =
        match lst with
        | First x -> x
        | Cons (h, t) -> 
            match t with
            | First ft -> toFirst (First (h*10 + ft))
            | Cons (ht, tt) ->
                toFirst (Cons( (h*10 + ht), tt)) 