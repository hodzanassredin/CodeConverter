// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open Languages.CPWriter
open Languages.CPAst

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    //let ast = 
    //printfn "Hello world %s" message
    0 // return an integer exit code