namespace DataParserTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open DataParser

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ValidInputReturnsADataEntry () =
        let input = "fit in rockets 100000.0"
        let expected = ("fit in", "rockets", float 100000.0)
        let result = parseData input
        Assert.AreEqual(expected, result)
