namespace Cata

module List =
    type List =
        | Cons of int*List
        | Nil

    let rec length = function
        | Nil          -> 0
        | Cons (_, l') -> 1 + length l'
        
    let rec listCata fNil fCons = function
        | Nil          -> fNil()
        | Cons (e, l') ->
            let value = (listCata fNil fCons l')
            fCons e value
            
    let lenghtCata list =        
        let fNil () = 0
        let fCons elem a = 1 + a 
        listCata fNil fCons list
    let sumCata list =        
        let fNil () = 0
        let fCons elem a = elem + a 
        listCata fNil fCons list
        
    let rec fold fNil fCons acc = function
        | Nil          -> fNil acc
        | Cons (e, l') ->
            let acc' = fCons acc e
            fold fNil fCons acc' l'

    let sumFold list =
        let fNil a = a+0
        let fCons a elem = a + elem
        fold fNil fCons 0 list