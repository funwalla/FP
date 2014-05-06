let func = (fun x -> printfn "times = %d" x )

let rec forLoop body times =
    if times <= 0 then
        body times
    else
        body times
        forLoop body (times - 1)

open System.Text.RegularExpressions

let (===) str (regex : string) =
    Regex.Match(str, regex).Success

"the quick brown fox" === "The (.*) fox"


List.fold (+) 0 [1 .. 10]

List.fold (*) 1 [1 .. 5]

let minus = (-)

List.fold minus 10 [3;2;1]

open System
open System.IO

let sizeOfFolderPiped folder =
  
  let getFiles path =
    Directory.GetFiles(path, "*.*", SearchOption.AllDirectories)

  let totalSize =
    folder
    |> getFiles
    |> Array.map (fun file -> new FileInfo(file))
    |> Array.map (fun info -> info.Length)
    |> Array.sum
    
  totalSize

sizeOfFolderPiped "C:\Users\John\Documents\Library\Computer Science\Programming"

List.iter
    (fun s -> printfn "s has length %d" s.Length)
    ["Pipe"; "Forward"]
  
["Pipe"; "Forward"] |>
List.iter (fun s -> printfn "s has length %d" s.Length)

// Forward composition operator

open System.IO

let sizeOfFolderComposed  =
  
    let getFiles folder =
        Directory.GetFiles(folder, "*.*", SearchOption.AllDirectories)

    getFiles
    >> Array.map (fun file -> new FileInfo(file))
    >> Array.map (fun info -> info.Length)
    >> Array.sum
  
sizeOfFolderComposed (Environment.GetFolderPath(Environment.SpecialFolder.MyPictures)) 


!5

// Example 3-1
let square x = x * x

let imperativeSum numbers =
    let mutable total = 0
    for i in numbers do
        let x = square i
        total <- total + x
    total

let functionalSum numbers =
    numbers
    |> Seq.map square
    |> Seq.sum

open System.Text.RegularExpressions

let (===) str (regex : string) =
  Regex.Match(str, regex).Success

"The quick brown fox" === "The (.*) fox"

// sum a list using the (+) symbolic function
List.fold (+) 0 [1 .. 5]

// multiply the members of a list using (*)
List.fold (*) 1 [1 .. 5]


[1 .. 3] |> List.iter (printfn "%d")
[1 .. 3] |> List.map (printfn "%d")

open System
open System.IO

let sizeOfFolderPiped folder =
  
  let getFiles path =
    Directory.GetFiles(path, "*.*", SearchOption.AllDirectories)
    
  let totalSize =
    folder
    |> getFiles
    |> Array.map (fun file -> new FileInfo(file))
    |> Array.map (fun info -> info.Length)
    |> Array.sum

  totalSize

sizeOfFolderPiped (Environment.GetFolderPath(Environment.SpecialFolder.MyPictures))

sizeOfFolderPiped "c:/Users/John/Documents/projects/progfs"

List.iter
    (fun (s:string) -> printfn "s has length %d" s.Length)
    ["Pipe"; "Forward"]

    ["Pipe"; "Forward"] |> List.iter (fun s -> printfn "%d" s.Length)

// Forward composition operator

open System
open System.IO

let sizeOfFolderComposed =

  let getFiles folder =
    Directory.GetFiles(folder, "*.*", SearchOption.AllDirectories)

  getFiles
  >> Array.map (fun file -> new FileInfo(file))
  >> Array.map (fun info -> info.Length)
  >> Array.sum

  sizeOfFolderComposed
    (Environment.GetFolderPath(Environment.SpecialFolder.MyPictures))

  let square x = x * x
  let toString (x : int) = x.ToString()

  
