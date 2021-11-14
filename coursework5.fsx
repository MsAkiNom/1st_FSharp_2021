(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Performing queries on FSharpON data structures

  ------------------------------------
  Name: Monika Shrestha
  Tallinn University of Technology Student ID
  or Uni-ID: moshre
  ------------------------------------


  Answer the questions below. You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework5/coursework5.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.

  Your solution must not use mutable state and imperative features
  like for loops.
*)


(*

For introduction to FSharpON please check coursework4.fsx for references.

In this coursework we continue with the topic of trees and object notation. This
time the task is, given a description of a set of values in an Ecma,
how to retrieve, modify and delete those values. This is a bit similar
to questions 6 and 7 in coursework 4.

The following material of basic definitions is taken from CW4. You are
free to use your solution to CW4 or try something different.

*)





type Name = string


// 0. Define data structure(s) for representing FsharpON (same as in CW4)
//

type Ecma = 
          | Obj of (Name * Ecma) list
          | Str of string
          | Arr of Ecma list 
          | Fl of float
          | Boolean  of bool
          | Null

(*
Define the function

  mkObject : unit -> Ecma

that creates a representation for an empty object structure.
*)

let mkObject () = Obj[]

(*
Define the function

  mkNumber : float -> Ecma

that creates a representation for the given floating-point number.
*)

let mkNumber (x:float): Ecma = Fl x

(*
Define the function

  mkBool : bool -> Ecma

that creates a representation for the given Boolean value.
*)

let mkBool = function
  | true -> Boolean true
  | false -> Boolean false

(*
Define the function

  mkString : string -> Ecma

that creates a representation for the given string value.
*)

let mkString (s: string) = Str s

(*
Define the function

 mkArray : Ecma list -> Ecma

that creates a representation for an array whose elements are
represented by the given list of `Ecma` values.
*)

let mkArray (ls: Ecma list) = Arr ls

(*
Define the function

  mkNull : unit -> Ecma

that creates a representation of the ECMA-404 `null` value.
*)

let mkNull () = Null

// Define the function
//
//   addNameValue : Name * Ecma -> Ecma -> Ecma
//
// so that
//
//   addNameValue (n, v) e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an object representation
//
// - a representation for the object e extended with the name-value
//   pair (n, v), otherwise.

let addNameValue ((n,v):Name*Ecma) (e: Ecma): Ecma = 
  match e with
  | Obj [] -> Obj [(n,v)]
  | Obj e -> Obj (e @ [(n,v)])
  | _ -> e



// Define the function
//
//   addValue : Ecma -> Ecma -> Ecma
//
// so that
//
//   addValue v e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an array representation
//
// - a representation for the array e with the value v added as the last
//   element, otherwise.


let addValue (v: Ecma) (e: Ecma): Ecma =
  match e with 
  | Arr [] -> Arr [v]
  | Arr a -> Arr (a @ [v])
  | a -> a




////////////////////////////////////////////////////////////////////////
// The content of this coursework begins here //////////////////////////


// You are given a type of expressions.

type BExpr = True
           | Not      of BExpr
           | And      of BExpr * BExpr
           | Or       of BExpr * BExpr
           | HasKey   of Name
           | HasStringValue of string
           | HasNumericValueInRange of (float*float)
           | HasBoolValue of bool
           | HasNull

(*

The type BExpr is just a discriminated union. The intended
interpretation of values of type BExpr is as predicates on values of
type Ecma.

 - True: evaluates to true on any Ecma

 - Not b: evaluates to true on precisely those Ecma for which b
          evaluates to false

 - And (b1, b2): evaluates to true on precisely those Ecma for which
                 both b1 and b2 evaluate to true

 - Or (b1, b2): evaluates to true on precisely those Ecma for which at least
                one of b1 and b2 evaluates to true

 - HasKey k: evaluates to true on precisely those Ecma that are objects and
             that contain a key k.

 - HasStringValue s: evaluates to true on precisely those Ecma that are 
            either Ecma strings with value s, objects that contain a value s,
            or arrays that contain a value s.

 - HasNumericValueInRange (xmin,xmax): evaluates to true on precisely those Ecma
               that either are
               numeric Ecma with value in closed range xmin,xmax,
               objects with a numeric value in closed range xmin,xmax,
               arrays with a numeric value in closed range xmin,xmax.

  - HasBoolValue b: evaluates to true on precisely those Ecma that are either
                    Boolean Ecma with value b,
                    objects that contain a Boolean value b,
                    arrays that contain a Boolean value b.
  - HasNull : evaluates to true on precisely those Ecma that are either
                    null Ecmas,
                    objects that conitain a null value,
                    arrays that contain a null value.

*)



// Here is a type of selector expressions.

type Selector = Match     of BExpr
              | Sequence  of Selector * Selector
              | OneOrMore of Selector


(*

The type Selector is just a discriminated union. The intended
interpretation of values of type Selector on values of type Ecma is as
sets of values in that Ecma. We also refer to the set of values
described by s : Selector as the set of values selected by s.

 - Match b: the singleton set consisting of the root value if the
            expression b evaluates to true and the empty set otherwise.
 
 - Sequence (s, s'): the set consisting of those values in the Ecma tree
                     that are selected by the selector s' starting from
                     any child value of a value that is selected by the
                     selector s (starting from the root value).

                     In other words, first determine the set of values
                     selected by s (starting from the root value). For
                     every child c of a value in this set, determine
                     the set of values selected by s' (starting from c)
                     and take the union of such sets as the result.

                     In other words, start from the root value with the
                     selector s. For the values that it selects,
                     continue with selector s' from their child values
                     and collect the results.
 
 - OneOrMore s: select the values selected by the selector s and, in
                addition, from the child values of the values in this
                set select the values selected by OneOrMore s.
 
                Thus, you can think of the values selected by OneOrMore s
                as the union of the following sets:
                - values selected by s
                - values selected by Sequence (s, OneOrMore s)
*)




// 1. Translate the following informal descriptions into values of
// type BExpr and Selector.
// 
// Define the values b1, b2 and b3 of type BExpr so that:
// 
//  - b1 evaluates to true on those Ecma that are object values
//    containing the keys "blue" and "left" but do not have the key "red".
// 
//  - b2 evaluates to true on those Ecma that are numeric values with
//    the value in the range [-5, 5).
// 
//  - b3 evaluates to true on those Ecma that have the string value "b3"
//    or that are object values which have the key "b3".
//
// Define the values s1, s2 and s3 of type Selector so that:
// 
//  - s1 selects all object values with key "abc" that are at depth 3
// 
//  - s2 selects all values v such that v is a child of some value
//    and all of the ancestors of v have the string value "xyz"
// 
//  - s3 selects all values v such that:
//    * v is a child of a value t
//    * t does not have a string value "xyz"
//    * t is the root value
// 
// We consider the root value to be at depth 1.

let b1 =And ( And (HasKey "blue", HasKey "left"), Not (HasKey "red"))

let b2 = HasNumericValueInRange (-5.0,4.999999999)

let b3 = Or (HasStringValue "b3", HasKey "b3")

let s1 = Sequence (Sequence (Match True, Match True),Match (HasKey "abc")) 

let s2 = Sequence (Match (HasStringValue "xyz"), Match True)

let s3 = Sequence (Match (Not (HasStringValue "xyz")), Match True)





// 2. Define the function

// eval : BExpr -> Ecma -> bool

// which evaluates the given expression on the given Ecma.

// Evaluating a BExpr only considers properties of the root value of
// the Ecma and its immediate child values that are leaves (if there are any).

// In other words, for any  b : BExpr  and  e : Ecma

//    eval b e = eval b e'

// where e' is e with all of its non-leaf child values replaced
// with the representation for null.

let rec eval (b: BExpr) (e: Ecma): bool =
    match b with
    | True -> true
    | Not a -> not (eval a e)
    | And (c1,c2) -> (eval c1 e) && (eval c2 e)
    | Or (m1,m2) -> (eval m1 e) || (eval m2 e)
    | HasKey k ->
        match e with
        | Obj e -> List.exists(fun e -> fst(e)=k) e
        | _ -> false
    | HasStringValue s ->
        match e with
        | Str st1 -> s=st1
        | Arr a ->
            List.exists(fun el ->
                match el with
                | Str st1 -> s=st1
                | _ -> false ) a
        | Obj v ->
            List.exists(fun (_key, el) ->
                match el with
                | Str st1 -> s=st1
                | _ -> false) v
        | _ -> false
    | HasNumericValueInRange (min,max) ->
        match e with
        | Fl f -> f >= min && f <= max
        | Arr a ->
            List.exists(fun el ->
                match el with
                | Fl a -> a >= min && a <= max
                | _ -> false ) a
        | Obj v ->
            List.exists(fun (_key, el) ->
                match el with
                | Fl a -> a >= min && a <= max
                | _ -> false) v
        | _ -> false
    | HasBoolValue bo ->
        match e with
        | Boolean t -> t = bo
        | Arr a ->
            List.exists(fun el ->
                match el with
                | Boolean t -> t = bo
                | _ -> false ) a
        | Obj v ->
            List.exists(fun (_key, el) ->
                match el with
                | Boolean t -> t = bo
                | _ -> false) v
        | _ -> false
    | HasNull ->
        match e with
        | Null -> true
        | Arr a ->
            List.exists(fun el ->
                match el with
                | Null -> true
                | _ -> false ) a
        | Obj v ->
            List.exists(fun (_key, el) ->
                match el with
                | Null -> true
                | _ -> false) v
        | _ -> false


type Description = Key   of string
                 | Index of int

type Path = Description list


// 3. Define the function
//
// select : Selector -> Ecma -> (Path * Ecma) list
//
// that computes the set of values in the given Ecma described by the
// given Selector. The result is a list of pairs where the second
// component is a selected value and the first component is the full path
// to this value.
//
// The path to the root value is the empty list.
//
// If you follow a child value of an object, then you add the key of
// that value to the path. If you follow a child of an array, then you
// add the index of that value in the array to the path (the oldest value
// has index 0).
//
// The order of values in the result list must respect the order of values
// in the given Ecma. More precisely, in the result list:
// - a value must appear before any of its children
// - a value must not appear before its older siblings and their
//   descendants
//
// This task is similar to evaluating a BExpr on an Ecma. The difference is
// that instead of a BExpr we have a Selector and instead of a bool we
// compute a (Path * Ecma) list. In this case we also consider child
// values.


let select (selector:Selector) (e:Ecma):(Path * Ecma) list =
    let rec select' (s: Selector) (e: Ecma) (p: Path): (Path * Ecma) list =
        match s, e with
        | Match a, _ ->
          match eval a e with
          | true -> [(p, e)]
          | false -> []
        | Sequence (s, s'), Obj _ 
        | Sequence (s, s'), Arr _ ->
          let ecmaListPath = select' s e p
          List.fold(fun b ecmaPath -> 
            let (path, e') = ecmaPath
            let res = 
              match e' with
              | Obj v -> List.fold(fun b (key, el) -> b @ select' s' el (path @ [Key key]) ) [] v 
              | Arr e -> snd (List.fold(fun (m,b) el -> (m+1, b @ select' s' el (path @ [Index m]))) (0, []) e)
              | _ -> []

            b @ res
              
          ) [] ecmaListPath
        | Sequence (s, s'), _ -> []
        | OneOrMore (OneOrMore s), _ 
        | OneOrMore s, _ -> select' s e p @ select' (Sequence (s, OneOrMore s)) e p

    select' selector e []


// 4. Define the function
//
// update :  (string -> string)
//        -> (float  -> float)
//        -> Selector
//        -> Ecma
//        -> Ecma
//
// such that
//
//    update su nu s e
//
// evaluates to an Ecma that is otherwise the same as e except that,
// for the values selected by s, the string values and numeric values
// of that value have been updated according to the functions su and nu.

let update (su: string->string) (nu: float->float) (s: Selector) (e: Ecma) :Ecma =
  let rec pathUpdate (p:Path) (ec: Ecma) (ecma:Ecma) :Ecma =
    match p, ecma with
    | Index key::tail, Arr arr ->
      arr |> List.indexed |> List.collect(fun (indx, itm) -> if indx=key then [pathUpdate tail ec itm] else [itm]) |> Arr
      
    | Key key::tail, Obj obj ->
      obj |> List.fold(fun f ecma ->
        if key = fst ecma then f @ [fst ecma, pathUpdate tail ec (snd ecma) ] else f @ [ecma])[] |> Obj
      
    | [], _ when ec=ecma -> ecmaUpdate ecma
    | [], Obj _ -> match ec with Obj _ -> ecmaUpdate ecma | _ -> ecma
    | [], Arr _ -> match ec with Arr _ -> ecmaUpdate ecma | _ -> ecma
    | _ -> ecma
      
  and ecmaUpdate (ecma: Ecma) =
    match ecma with
    | Str str -> Str(su str) 
    | Fl num -> Fl(nu num) 
    | Obj obj -> 
      obj |> List.fold(fun f (name, ecma) ->
        match ecma with
        | Str _ | Fl _ -> f @ [name, ecmaUpdate ecma] 
        | _ -> f @ [name, ecma] ) [] |> Obj
    | Arr arr -> 
      arr |> List.fold(fun f ecma ->
        match ecma with
        | Str _ | Fl _ -> f @ [ecmaUpdate ecma] 
        | _ -> f @ [ecma] ) [] |> Arr
    | _ -> ecma
  
  select s e |> List.rev |> List.fold(fun f (p, ecmaSelected) -> pathUpdate p ecmaSelected f ) e
  
    

// 5. Define the function
//
// delete : Selector -> Ecma -> Ecma option
//
// which removes from the given Ecma all values that are selected by
// the given Selector. Removing a value means removing the entire
// subtree rooted at that value.
//
// The result should be `None` when after the delete operation there
// is no `Ecma` value left. Otherwise use `Some`.


let delete (s: Selector) (e: Ecma) : Ecma option =
    let rec pathDelete (p: Path) (ec: Ecma) (ecma: Ecma option) : Ecma option =
        match p, ecma with
        | Index key :: tail, Some (Arr arr) ->
            Some(
                Arr(
                    List.fold
                        (fun (f, count) ecma ->
                            if count = key then
                                let deleteValue = pathDelete tail ec (Some ecma)

                                match deleteValue with
                                | Some m -> (f @ [ m ], count + 1)
                                | None -> f, count + 1
                            else
                                f @ [ ecma ], count + 1)
                        ([], 0)
                        arr
                    |> fst
                )
            )

        | Key key :: tail, Some (Obj obj) ->
            Some(
                Obj(
                    obj
                    |> List.fold
                        (fun f (name, ecma) ->
                            if key = name then
                                let deleteValue = pathDelete tail ec (Some ecma)

                                match deleteValue with
                                | Some m -> f @ [ name, m ]
                                | None -> f
                            else
                                f @ [ name, ecma ])
                        []
                )
            )

        | [], Some m when m = ec -> None
        | [], Some (Obj _)
        | [], Some (Arr _) -> None
        | _ -> ecma

    select s e |> List.rev  
    |> List.fold (fun f (p, selectedEcma) -> pathDelete p selectedEcma f) (Some e)




// 6. Using the function update, define the functions
//
//   toZero : float -> Selector -> Ecma -> Ecma
//
// and
//
//   truncate : int -> Selector -> Ecma -> Ecma
//
// so that
//
//   toZero x s e
//
// evaluates to an Ecma that is otherwise the same as e except that the
// values selected by s have each of their numeric values y replaced by 0
// if y is in the range [-x, x].
//
//   truncate n s e
//
// evaluates to an Ecma that is otherwise the same as e except that the
// values selected by s have each of their string values y truncated to
// length n.
//
// These functions should not be defined recursively; define them in
// terms of update.

let toZero (x: float) (s: Selector) (e: Ecma): Ecma =
  update id (fun frange -> if frange >= -x && frange <= x  then 0.0 else frange) s e


let truncate (n: int) (s: Selector) (e: Ecma): Ecma =
  update (fun fr-> if  fr <> null && fr.Length >= n then fr.Substring(0, n) else fr) id s e





// 7. Using the function update, define the function
// 
//   mapEcma : (string -> string) -> (float -> float) -> Ecma -> Ecma
//
// such that
//
//   mapEcma f g e
//
// evaluates to an Ecma obtained by updating every value in the
// given Ecma value according to f and g.
//
// This function should not be defined recursively; define it in
// terms of update.

let mapEcma (f: string -> string)(g: float -> float) (e: Ecma): Ecma =
  update f g (OneOrMore(Match True)) e

