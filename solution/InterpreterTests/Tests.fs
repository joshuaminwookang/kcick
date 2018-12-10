namespace InterpreterTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectInterpreter

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ValidDataLineReturnsMapOfMaps () =
        let input = [|"fit in oranges 8.0"|]
        let emptydb = Map.empty<string, float>
        let result = parselines (input, Map.empty<string, Map<string, float>>)
        let expectedinner = emptydb.Add("oranges", float 8.0)
        let expectedempty =  Map.empty<string, Map<string, float>>
        let expected = expectedempty.Add("fit in", expectedinner)
        Assert.AreEqual(expected, result)
