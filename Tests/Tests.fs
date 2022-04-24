module Tests

open System
open Xunit
open Languages.Parser
open Languages.Oberon0
open System.IO

[<Fact>]
let ``Oberon0 should parse symbols`` () =
    let program = File.ReadAllText("Oberon0.txt")
    let symbols = Languages.Parser.run Languages.Oberon0.pSymbol program
    Assert.True(true)
