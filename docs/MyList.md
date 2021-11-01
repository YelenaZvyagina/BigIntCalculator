# MyList

MyList is a library, implementing functions for non-empty lists. It's used for bigInt functions.

# Type

`MyList` is a discriminated union with `First of 't` or `Cons of 't * MyList<t>`. 

# Functions

* `fold (f:'a -> 'b -> 'a) (acc:'a) (l:MyList<'b>)` - implementation of List.fold for MyList
* `length (l:MyList<'t>)` - returns a length of a list
* `iter (l:MyList<'a'>) (f:'a -> unit)` - applies a given function to each element of a list
* `sysListToMyList (l:MyList<'a'>)` - converts `List` to `MyList`
* `myListToSystemList (l:MyList<'a'>)` - converts `MyList` to `List` 
* `concat (l1:MyList<'a'>) (l2:MyList<'a'>)` - concatinates two lists
* `map (f:'a -> 'b) (x:MyList<'a>)` - returns a list to whose elements the given function has been applied 
* `sort (l:MyList<'t>)` - returns a sorted list
* `getHead (ml:MyList<_>)` - returns the head of list
* `getTail (ml:MyList<_>)` - returns the tail of list
* `reverse (ml:MyList<int>)` - returns a reversed list