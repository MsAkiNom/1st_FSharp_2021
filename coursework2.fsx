(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

  ------------------------------------
  Name: Monika Shrestha
  Tallinn University of Technology Student ID
  or Uni-ID: moshre
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2021 under your name, into a file coursework2/coursework2.fsx.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Deadline for submitting the solution is September 30 AoE, 2021.
*)

// You are given a type BibliographyItem that has the following structure:
// string list * string * (int * int) * int
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents a pair containing the starting page number and ending page number of the paper.
// * The fourth field represents the year of publication

type BibliographyItem = string list * string * (int * int) * int

// 1. Create a value bibliographyData : BibliographyItem list that contains
// at least 7 different publications on your favourite topic from https://dblp.uni-trier.de/ 
// Please note that you need not read the whole papers, just pick 7 papers that look interesting to you from the database.

let bibliographyData :
 BibliographyItem list = 
 [
     (["Adithyalal, P. M"; "Balachandran, Shankar"; "Singh, Virendra"], "A Soft Error Resilient Low Leakage SRAM Cell Design", (133, 138), 2015); 
     (["Pravika, M"; "Jeevamma, Jacob"; "Paul, Joseph K"], "The Impact of Manually Controlled Portable Duodopa Pump on Levodopa Pharmacokinetics and Pharmacodynamics", (696, 701), 2019); 
     (["Zambrano, Ana Maria"; "Vizuete, Oscar Marcelo Zambrano"; "Mejía, Eduardo Luis Ortiz"; "Xavier, Calderón-Hinojosa"], "SIGPRO: A Real-Time Progressive Notification System Using MQTT Bridges and Topic Hierarchy for Rapid Location of Missing Persons", (149190, 149198), 2020);
     (["Anish, Kumar M."; "Jung, Soyoun"; "Ji, Taeksoo"], "Protein Biosensors Based on Polymer Nanowires, Carbon Nanotubes and Zinc Oxide Nanorods", (5087, 5111), 2011);
     (["Lenord, Melvix J. S. M."; "Vikas, Lokesh"; "Polyzos, George C."], "Energy Efficient Context based Forwarding Strategy in Named Data Networking of Things", (249, 254), 2016);
     (["Sundarramurthi, M."; "Nihar, M"; "Giridharan, Anandi"], "Personalised Food Classifier and Nutrition Interpreter Multimedia Tool Using Deep Learning", (881, 884), 2020);
     (["Wang, Kaili"; "José, Oramas M."; "Tuytelaars, Tinne"], "Multiple Exemplars-Based Hallucination for Face Super-Resolution and Editing", (258, 273), 2020);
  ]

// 2. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns 
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You need to use the support honouring the default culture for ordering strings, i.e. the built in
// compare does not work properly by default. Look at the
// documentation of the .Net comparison for strings: System.String.Compare
// If the first authors are the same
// then the precedence should be determined by the next author.
// Please note that your implementation should be recursive over the input lists.
//
// The sort order in .Net is defined using System.Globalization.CultureInfo:
// https://docs.microsoft.com/en-us/dotnet/api/system.globalization
// Please note that your solution should not force a particular sort order!

let rec compareLists (s1: string list) (s2: string list) : int =
  match s1,s2 with
  | [],[] -> 0
  | [], _ -> -1
  | _,[]-> 1
  |x::xs, y::ys -> 
  let com = System.String.Compare(x,y) in if com = 0 then compareLists xs ys else com
  

// 3. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// Use solution from task 2.

let compareAuthors (a: BibliographyItem) (b: BibliographyItem): int =
 match a,b with 
 | (x,_,_,_),(y,_,_,_)-> compareLists x y

// 4. Make a function
// compareAuthorsNumPages : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are 
// the same then according to the number of pages in the publication.

let compareAuthorsNumPages (p1: BibliographyItem) (p2: BibliographyItem): int =
  let (_,_,x,_) = p1 
  let (_,_,y,_) = p2 
  if compareAuthors p1 p2 = 0 then compare ((snd x) - (fst x)) ((snd y) - (fst y)) 
  else compareAuthors p1 p2
  
// 5. Make a function 
// sortBibliographyByNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the number of pages in the
// publication in ascending order.
// If two items are at the same level in the sort order, their order should be preserved.

let sortBibliographyByNumPages pg: BibliographyItem list = List.sortBy(fun (_,_,(f),_) -> ((snd f) - (fst f))) pg

// 6. Make a function 
// sortBibliographyByAuthorNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and number of pages in the publication in ascending order
// If two items are at the same level in the sort order, their order should be preserved.

let sortBibliographyByAuthorNumPages ap:BibliographyItem list = List.sortBy(fun (a,_,(f),_) -> (a,((snd f) - (fst f)))) ap

// 7. Make a function
// groupByAuthor : BibliographyItem list -> (string * BibliographyItem list) list
// where the return list contains pairs where the first element is the name of a single
// author and the second element a list of bibliography items that the author has co-authored.

let rec distinctAuthor distA =
    match distA with
    |[] -> []
    |(x,_,(_,_),_)::tail -> List.distinct (List.append x (distinctAuthor tail))

let groupOfAuthor (y: BibliographyItem list):(string * BibliographyItem) list = 
    let dauthor = distinctAuthor y
    Seq.toList(
        seq {for d in dauthor do
               for (author,_,(_,_),_) as i in y do
                if (List.contains d author) then
                    yield (d,(i))
                    })

let groupByAuthor dauthor =
      groupOfAuthor dauthor
      |> List.groupBy fst  
      |> List.map (fun (x1,y1) -> x1, List.map snd y1)


