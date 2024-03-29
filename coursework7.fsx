(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 7: property-based testing

  ------------------------------------
  Name: Monika Shrestha
  Student ID: moshre
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework7/coursework7.fsx .
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is
  incorrect it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

  NB! Do not delete the stubs we have provided! If you did not manage
  to complete the implementation then keep the incomplete stub and
  make sure that it still is typeable as described in the question.
*)

#if INTERACTIVE 
//#r    "FsCheck.dll" // in .Net Framework and .Net core versions prior to 5.0
#r    "nuget: FsCheck, Version=2.14.3" // You can also use nuget directly with F# 5 and 6.
#load "FileSystem.fs"
#endif

open FsCheck

open FileSystem



(*
   Question 1

   Define the predicates

   fsTreeWf : FsTree -> bool

   pathWf   : Path -> bool

   that evaluate to true on precisely those values of FsTree and Path
   that are well-formed.

   The directory names in a well-formed Path cannot be empty.

   A value of type FsTree is well-formed when:
   - the path to any directory in the filesystem is well-formed
   
   - the filesystem is deterministic (for any given path, there is at
     most one directory in the filesystem with that path)

   Define these predicates so that they traverse their input only
   once.
*)

let rec fsTreeWf (fs: FsTree) =
   match (fs.name,fs.children) with
   |("",_) -> false
   |(_, []) -> true
   |(_,list) -> isBranch list []
and isBranch (fsList: FsTree list) (p: Path) =
   match fsList with
   |h::t -> match (List.contains h.name p) with
               |true -> false
               |false -> (fsTreeWf h) && (isBranch t (h.name::p ))
   |[] -> true

let pathWf (p: Path) =
   match p with
   | [] -> false
   |_->  (List.contains "" >> not) p



(*
   Question 2

   Define an FsCheck property

   createIsWf : Path -> FsTree -> Property

   which checks that creating a well-formed path p in a well-formed
   filesystem fs results in a well-formed filesystem.

   Define this using a conditional property (==>).

   Is this a good way to check such a property? Why?

   What percentage of the generated test inputs trivially satisfy this
   property?
*)

let createIsWf (p: Path) (fs: FsTree) = (pathWf p && fsTreeWf fs) ==> fsTreeWf (create p fs)



(*
   Question 3

   Define a generator

   wfTrees : Gen<FsTree>

   that only generates well-formed filesystems.


   Define a generator

   wfPaths : Gen<Path>

   that only generates well-formed paths.


   Define these generators in such a way that none of the generated
   data is wasted (i.e., discarded). In other words, all the data that
   you (randomly) generate must be used in the the output of the
   generator.

   You may wish to use the predicates defined above to check that the
   generators indeed only generate well-formed data. Or that the
   predicates are defined correctly.
*)

let wfTrees: Gen<FsTree> =
   failwith "sth"

let wfPaths: Gen<Path> = 
   failwith "sth"


(*
   Question 4

   Define an FsCheck property

   deleteIsWellFormed : Path -> FsTree -> bool

   which checks that given
   
   p  : Path
   fs : FsTree

   we have that the result of deleting p from fs is well-formed.

   You may assume that this property is only used with "well-formed"
   generators (meaning that p and fs are well-formed).
*)


let deleteIsWellFormed (p: Path) (fs: FsTree) = fsTreeWf fs && fsTreeWf (delete p fs)




(*
   Question 5

   Define an FsCheck property

   createCreates : Path -> FsTree -> bool

   which checks that given

   p  : Path
   fs : FsTree

   we have that the path p is included (exactly once) in the
   result of show after we have created the directory p in fs.

   You may assume that this property is only used with "well-formed"
   generators (meaning that p and fs are well-formed).
*)

let createCreates (p: Path) (fs: FsTree) = fs |> create p |> show |> List.contains(p)



(*
   Question 6

   Define an FsCheck property

   deleteDeletes : Path -> FsTree -> bool

   which checks that given

   p  : Path
   fs : FsTree

   we have that the path p is not in the result of show after we have
   deleted the directory p from fs.

   You may assume that this property is only used with "well-formed"
   generators (meaning that p and fs are well-formed).
*)


let deleteDeletes (p: Path) (fs: FsTree) = not (fs |> delete p |> show |> List.contains(p))



(*
   Question 7

   Define an FsCheck property

   showShowsEverything : FsTree -> bool

   which checks that given an
   
   fs : FsTree

   we have that by deleting one by one all of the items in the result
   of show fs we end up with an empty filesystem.

   You may assume that this property is only used with "well-formed"
   generators (meaning that fs is well-formed).
*)

let rec showShowsEverything (fs: FsTree) =
   if isEmpty fs then false else
    fsTreeWf fs && (let s = show fs
                    let rec elementDel es fs =
                        match es with
                        |[] -> fs
                        |head::tail -> elementDel tail (delete head fs)
                    elementDel s fs = {name = " "; children = []})

   

(*
   Question 8

   Define an FsCheck property

   createAndDelete : FsTree -> Path -> Path -> Property

   which checks that given
   
   fs : FsTree
   p1 : Path
   p2 : Path

   we have that, if p1 is not a prefix of p2, then

   1) creating directory p1 in fs
   2) creating directory p2 in the result
   3) deleting p1 from the result

   produces a filesystem that still contains p2.

   You may assume that this property is only used with "well-formed"
   generators (meaning that fs, p1 and p2 are well-formed).
*)

// let createAndDelete (fs: FsTree) (p1: Path) (p2: Path) = 
//    failwith "sth"


let createAndDelete (fs: FsTree) (p1: Path) (p2: Path) =
   let rec prefix p1 p2 =
      match p1,p2 with
      | [] , _ -> true
      | _ , [] -> false
      | x::xs, y::ys when x = y -> prefix xs ys
      | _ , _ -> false
   let _fs = (lazy(fs |> create p1 |> create p2 |> delete p1 |> show))  
   not(prefix p1 p2) ==> (lazy(_fs.Value |> List.contains(p2) && not(_fs.Value |> List.contains(p1))))

