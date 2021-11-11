(*
  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------
  Coursework 0: Getting started
  ------------------------------------
  Name: Monika Shrestha
  Student ID: moshre@ttu.ee
  ------------------------------------
  Answer the questions below.  You answers to questions 2--8 should be
  correct F# code written after the question. The F# code for question
  1 is written for you and serves as an example. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.
  This coursework will NOT be graded but we encourage you to do it,
  you will not succeed in this course if you don't practice, and
  there's no time like the present! Also, you may find that parts of
  it appear in later courseworks. *)

// 0. Find your way to the fsharp interactive (fsi) command prompt.
// I.e. log in to a lab machine and start Visual Studio, install
// VSCode/Ionide and .net 5.0 on your laptop, etc.


// 1. Load the  following function into fsi
let greeting name = printfn "Hello: %s" name

// 2. Run the function greeting and  say hello to yourself.
greeting "monika"

// 3. Create a value myName : string that contains your name.
let myName : string = "Monika"

// 4.Define
// splitAtChar : text:string -> sep:char -> list<string>
// is equivalent to
// splitAtChar : text:string -> sep:char -> string list
let splitAtChar (text: string) (sep:char) = text.Split sep |> Array.toList 

// 5. Write a function splitAtSpaces in such a way that it uses splitAtChar
// Hint: we defined splitAtSpaces in the lecture, now you need to modify it.
let splitAtSpaces (text: string) = splitAtChar text ' '

// 6. Define sentenceCount : text:string -> int

let splitAtSentence (text:string) =
  text.Split([|". "; "! "; "? "|], System.StringSplitOptions.RemoveEmptyEntries)
 
let sentenceCount (text:string) =
  splitAtSentence(text).Length


// 7. Define stats : text:string -> unit
// which prints the same stats as showWordCount and
// the number of sentences and average length of sentences
// hint: try float: int -> float
let wordCount text =
  let words = text |> splitAtSpaces
  let numWords = words.Length
  let wordSet = Set.ofList words
  let numDups = numWords - wordSet.Count
  (numWords, numDups)

let averageSentenceLength(text:string) =
  float(splitAtSpaces(text).Length) / float(sentenceCount(text))
 
let stats (text:string) =
  let numWords, numDups = wordCount text
  printfn "--> %d words in text" numWords
  printfn "--> %d duplicate words" numDups
  printfn "--> %d number of sentences" (sentenceCount(text))
  printfn "--> %f average length of sentence" (averageSentenceLength(text))
 
// 8. Use the 'http' function from the lecture to download the file
// http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt as a string
// NOTE: you cannot use this function in tryfsharp. Instead you can
// paste the text into your file as a string and process it locally
open System.IO
open System.Net

// get the contents of the url via a web request
let http (url: string) =
  let req = WebRequest.Create(url)
  let resp = req.GetResponse()
  let stream = resp.GetResponseStream()
  let reader = new StreamReader(stream)
  let html = reader.ReadToEnd()
  resp.Close()
  html

let file = http "http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt"

// 9. run stats on the downloaded file
stats file