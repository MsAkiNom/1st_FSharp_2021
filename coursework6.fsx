(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Tail recursion

  ------------------------------------------------
  Name: Monika Shrestha
  Student ID: moshre
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework6.fsx in directory coursework6.

  The deadline for completing the above procedure is Sunday, November 7, 2021
  (postponed to November 14).

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1: func (first: Int) -> ((Int) -> ((Bool) -> String))

  Write a function
  pHoldsForAllSequentialElements : (int -> int -> bool) -> int list -> bool

  that when calling the function

  pHoldsForAllSequentialElements p xs

  will check if predicate p holds for all consequtive elements in the list. If it does,
  the function should return true, else false.

  In the case there is less than two elements in the list it is assumed that
  the predicate does not hold as there are no sequential elements present.

  NB! You are required to implement the function in a tail recursive way using
  explicit recursion.
*)

let rec pHoldsForAllSequentialElements (p: int -> int -> bool)(xs: int list): bool =
  if List.length xs = 0 || List.length xs = 1
  then false
  else
  let rec  pHoldsForAllSequential e p =
    match e with
      |[] -> true
      |x :: [] -> true
      |x::(y::m as ys) -> if (p x y) then pHoldsForAllSequential ys p else false
  pHoldsForAllSequential xs p

   

(*
  Task 2:

  Write a function
  createTwoTuplesOfList : 'a -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s and returns a list of two element tuples of 'a-s that are taken
  sequentially from the list passed as the second argument to the function.
  In case the list has odd number of elements make the first argument of the
  function be the second element in the last tuple. 
  Make sure your implementation uses explicit tail recursion.
*)
let createTwoTuplesOfList m  xs =
  let rec recur a xs =
    match xs with 
    | [] -> a
    | [x] -> (x, m)::a
    | x::y::tail -> recur ((x,y)::a)tail
  xs
    |> recur []
    |> List.rev

createTwoTuplesOfList 2  [4;4;4;5;6;6;12;5]

(*
  Task 3:

  Write a function
  createTwoTuplesOfListFold : 'a -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s and returns a list of two element tuples of 'a-s that are taken
  sequentially from the list passed as the second argument to the function. In case
  the list has odd number of elements make the first argument of the function be the
  second element in the last tuple. 
  Make sure your implementation uses List.fold or List.foldBack appropriately.
  Test yourself if this implementation appears to be tail recursive.
*)

let createTwoTuplesOfListFold at xs =
  let state = ([],[])
  let funcl m n =
    match m with 
    | [],xs -> [n],xs
    | [a],xs -> [],(a, n)::xs
    | _ -> [n],[]
  let f, t = xs
                |> List.fold funcl state
  if f = List.Empty then t else (f.Head, at)::t
  |> List.rev

createTwoTuplesOfListFold 2  [4;4;4;5;6;6;12;5]



(*
  Task 4:

  Below you find the definition of a type Tr of leaf-labeled trees. Write a
  function
  
  medianAndAverageInTree : int Tr -> int * float
  
  that returns a pair where the first element is the median label in the
  given tree and the second an average across all nodes. The median is the label
  for which the difference in counts of elements to the right and left is
  either 0 or the count of elements to the right is exactly 1 greater than the
  count of elements to the left. The average is the sum of all elements divided with
  the number of elements.
  Use continuation-passing style in your implementation and perform the operations
  in a single pass of the tree.
*)

type 'a Tr =
  | Lf   of 'a
  | Br of 'a Tr * 'a Tr

let medianAndAverageInTree (tree: int Tr) : (int * float) =
    let rec recl (tree: int Tr) f (lt: int list) : int list =
        match tree with
        | Br (x, y) -> recl y ( recl x f ) lt
        | Lf x -> f (x :: lt)

    let st = recl tree id []
    let size = float (st.Length)
    let sum = float (List.sum st)
    let mean = sum / size

    let median =
         
        if st.Length % 2 = 0 then
            st.Item(st.Length / 2 - 1)
        else
            st.Item((st.Length - 1) / 2)

    (median, mean);;







