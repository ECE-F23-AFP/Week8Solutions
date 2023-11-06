module AlgebraicDataTypes.fs

type FileSys = Element list
and Element  =
    | File of string * int
    | Dir of string * FileSys
        
(* val elem: Element =
  Dir
    ("root",
     [File ("A", 2);
      Dir ("B", [File ("C", 3); Dir ("D", [File ("E", 4); File ("F", 5)])])])  

*)

// 1
let rec size = function
    | File (_, s) -> s
    | Dir (_, fs) -> sizeList fs
and sizeList = function
    | [] -> 0
    | a :: rest -> (size a) + (sizeList rest) 

// 2
let rec traverse fnFile empty merge = function
    | File (name, size) -> fnFile name size
    | Dir (name, elements) -> traverseE name fnFile empty merge elements
and traverseE name fnFile empty merge = function
    | [] -> empty
    | a :: rest -> merge (traverse fnFile empty merge a) (traverseE name fnFile empty merge rest)
(*
traverse (fun n s -> s) 0 (fun a b -> a + b) elem
*)

// 3
